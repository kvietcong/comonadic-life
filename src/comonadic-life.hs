import System.Random
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.List (intercalate, unfoldr)
import Control.Comonad (Comonad(..))
import Control.Concurrent (threadDelay)
import System.Console.ANSI (clearScreen, setCursorPosition, hideCursor, getTerminalSize)

surround :: String -> String -> String
surround padding = between padding padding

between :: String -> String -> String -> String
between pre post mid = pre ++ mid ++ post

filterReplace :: [Char] -> Char -> String -> String
filterReplace toReplace newChar =
    map (\ch ->if ch `elem` toReplace then newChar else ch)

data Cell = Alive | Dead deriving (Eq, Enum, Bounded)
instance Show Cell where show cell = if cell == Alive then "O" else " "
instance Random Cell where
    random gen = case randomR (0,1) gen of
                   (r, gen') -> (toEnum r, gen')
    randomR (a,b) gen = case randomR (fromEnum a, fromEnum b) gen of
                          (r, gen') -> (toEnum r, gen')

data Z a = Z [a] a [a] deriving Eq

class Comonad c => Zipper c where
    shiftLeft       :: c b -> c b
    shiftRight      :: c b -> c b

instance Functor Z where
    fmap function (Z ls f rs) =
        Z (function <$> ls) (function f) (function <$> rs)

instance Show a => Show (Z a) where
    show (Z ls f rs) = (unwords . (map show . reverse)) ls
                    ++ between "(" ")" (show f)
                    ++ (unwords . map show) rs

instance Comonad Z where
    extract (Z _ focus _) = focus
    duplicate z@(Z ls _ rs) = Z getAllLefts z getAllRights
        where getAllLefts = tail $
                  scanl (\lastZ _ -> shiftLeft lastZ) z ls
                  -- take (length ls + 1) $ iterate shiftLeft z
              getAllRights = tail $
                  scanl (\lastZ _ -> shiftRight lastZ) z rs
                  -- take (length rs + 1) $ iterate shiftRight z

instance Zipper Z where
    shiftLeft (Z (l:ls) f rs) = Z ls l (f:rs)
    shiftLeft zipper = zipper

    shiftRight (Z ls f (r:rs)) = Z (f:ls) r rs
    shiftRight zipper = zipper

newtype ZZ a = ZZ (Z (Z a)) deriving Eq

class Comonad c => Zipper2D c where
    goUp    :: c a -> c a
    goLeft  :: c a -> c a
    goDown  :: c a -> c a
    goRight :: c a -> c a

instance Functor ZZ where
    fmap function (ZZ zz) = ZZ $ (fmap . fmap) function zz

instance Show a => Show (ZZ a) where
    show (ZZ (Z us f ds)) = intercalate "\n"
        [ intercalate "\n" $ (map (filterReplace "()" ' ' . surround " " . show) . reverse) us
        , between "(" ")" $ show f
        , intercalate "\n" $ map (filterReplace "()" ' ' . surround " " . show) ds]

instance Comonad ZZ where
    extract (ZZ innerZips) = extract . extract $ innerZips
    duplicate (ZZ innerZips) = ZZ <$> ZZ ((dupe . dupe) innerZips)
        where dupe zz = Z (getAllLefts zz) zz (getAllRights zz)
              getAllLefts zz@(Z _ (Z ls _ _) _) = tail $
                  scanl (\lastZZ _ -> shiftLeft <$> lastZZ) zz ls
                  -- take (length ls + 1) $ iterate (fmap shiftLeft) zz
              getAllRights zz@(Z _ (Z _ _ rs) _) = tail $
                  scanl (\lastZZ _ -> shiftRight <$> lastZZ) zz rs
                  -- take (length rs + 1) $ iterate (fmap shiftRight) zz

instance Zipper2D ZZ where
    goUp (ZZ innerZips) = ZZ $ shiftLeft innerZips
    goDown (ZZ innerZips) = ZZ $ shiftRight innerZips
    goLeft (ZZ innerZips) = ZZ $ shiftLeft <$> innerZips
    goRight (ZZ innerZips) = ZZ $ shiftRight <$> innerZips

getNeighbors :: ZZ a -> [a]
getNeighbors zz@(ZZ (Z us (Z ls _ rs) ds)) = extract <$> neighbors
  where canGoLeft = not . null $ ls
        canGoRight = not . null $ rs
        canGoUp = not . null $ us
        canGoDown = not . null $ ds
        neighbors = [goLeft zz | canGoLeft]
                 ++ [goLeft . goUp $ zz | canGoLeft && canGoUp]
                 ++ [goLeft . goDown $ zz | canGoLeft && canGoDown]
                 ++ [goRight zz | canGoRight]
                 ++ [goRight . goUp $ zz | canGoRight && canGoUp]
                 ++ [goRight . goDown $ zz | canGoRight && canGoDown]
                 ++ [goUp zz | canGoUp]
                 ++ [goDown zz | canGoDown]

createRandomCellRows :: [Cell] -> Int -> [Z Cell]
createRandomCellRows cells width = nextRow : createRandomCellRows (drop (width + 1) cells) width
    where nextRow = Z ls f rs
          ls = take halfWidth cells
          rs = take halfWidth . drop halfWidth $ cells
          f = cells !! width
          halfWidth = width `div` 2

createRandomCellGrid :: RandomGen g => Int -> Int -> g -> ZZ Cell
createRandomCellGrid width height gen = ZZ (Z us f ds)
    where us = take halfHeight randomRows
          ds = take halfHeight . drop halfHeight $ randomRows
          f = randomRows !! height
          randomRows = createRandomCellRows (randoms gen) width
          halfHeight = height `div` 2

createInfiniteRandomCellLists :: RandomGen g => g -> [[Cell]]
createInfiniteRandomCellLists = map randoms . unfoldr (Just . split)

createInfiniteRandomCellRows :: [[Cell]] -> [Z Cell]
createInfiniteRandomCellRows cells = nextRow : createInfiniteRandomCellRows (drop 3 cells)
    where nextRow = Z ls f rs
          ls = cells !! 1
          rs = cells !! 2
          f = head . head $ cells

createInfiniteRandomGrid :: RandomGen g => g -> ZZ Cell
createInfiniteRandomGrid gen = ZZ (Z us f ds)
    where infiRows = createInfiniteRandomCellRows
                   $ createInfiniteRandomCellLists gen
          -- TODO: Fix this so it is actually random
          us = drop 10 infiRows
          ds = drop 1000 infiRows
          f = head infiRows

gameOfLifeRules :: ZZ Cell -> Cell
gameOfLifeRules cells = case extract cells of
                          Dead -> if alive == 3 then Alive else Dead
                          Alive -> if alive `elem` [2,3] then Alive else Dead
    where alive = length . filter (==Alive) $ getNeighbors cells

gameOfLife :: ZZ Cell -> [ZZ Cell]
gameOfLife = iterate (extend gameOfLifeRules)

microsecondsInSecond :: Int
microsecondsInSecond = 1000000

animate :: Int -> [a] -> (a -> String) -> IO ()
animate delay states showFunction = do
    forM_ states (\state -> do putStrLn $ showFunction state
                               threadDelay delay
                               setCursorPosition 0 0)

showZ :: Show a => Int -> Z a -> String
showZ width (Z ls f rs) = (unwords . map show . reverse . take safeWidth) ls
                       ++ between "(" ")" (show f)
                       ++ (unwords . map show . take safeWidth) rs
                           where safeWidth = width `div` 4 - 1

showZZ :: Show a => Int -> Int -> ZZ a -> String
showZZ width height (ZZ (Z us f ds)) = do
    intercalate "\n"
        [ intercalate "\n"
            $ (map (filterReplace "()" ' ' . showZ width)
              . reverse
              . take safeHeight) us
        , showZ width f
        , intercalate "\n"
            $ (map (filterReplace "()" ' ' . showZ width)
              . take safeHeight) ds
        ] where safeHeight = height `div` 2 - 1

evolutionsPerSecond :: Int
evolutionsPerSecond = 5

main :: IO ()
main = do
    hideCursor
    clearScreen
    setCursorPosition 0 0

    gen <- getStdGen
    (height, width) <- fromMaybe (-1, -1) <$> getTerminalSize

    animate
        (microsecondsInSecond `div` evolutionsPerSecond) 
        -- (gameOfLife $ createRandomCellGrid 1000 1000 gen)
        (gameOfLife $ createInfiniteRandomGrid gen)
        (showZZ width height)
