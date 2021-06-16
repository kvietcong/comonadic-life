import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
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

data Cell = Alive | Dead deriving (Eq, Enum)
instance Show Cell where show cell = if cell == Alive then "O" else " "

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
                  take (length ls + 1) $ iterate shiftLeft z
              getAllRights = tail $
                  take (length rs + 1) $ iterate shiftRight z

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
        where dupe zipper2D = Z (getAllLefts zipper2D) zipper2D (getAllRights zipper2D)
              getAllLefts zz@(Z _ (Z ls _ _) _) = tail $
                  take (length ls + 1) $ iterate (fmap shiftLeft) zz
              getAllRights zz@(Z _ (Z _ _ rs) _) = tail $
                  take (length rs + 1) $ iterate (fmap shiftRight) zz

instance Zipper2D ZZ where
    goUp (ZZ innerZips) = ZZ $ shiftLeft innerZips
    goDown (ZZ innerZips) = ZZ $ shiftRight innerZips
    goLeft (ZZ innerZips) = ZZ $ shiftLeft <$> innerZips
    goRight (ZZ innerZips) = ZZ $ shiftRight <$> innerZips

getNeighbors :: ZZ a -> [a]
getNeighbors zipper2D@(ZZ (Z up (Z left _ right) down)) = extract <$> neighbors
  where canGoLeft = not . null $ left
        canGoRight = not . null $ right
        canGoUp = not . null $ up
        canGoDown = not . null $ down
        neighbors = [goLeft zipper2D | canGoLeft]
                 ++ [goLeft . goUp $ zipper2D | canGoLeft && canGoUp]
                 ++ [goLeft . goDown $ zipper2D | canGoLeft && canGoDown]
                 ++ [goRight zipper2D | canGoRight]
                 ++ [goRight . goUp $ zipper2D | canGoRight && canGoUp]
                 ++ [goRight . goDown $ zipper2D | canGoRight && canGoDown]
                 ++ [goUp zipper2D | canGoUp]
                 ++ [goDown zipper2D | canGoDown]

infiCellAlt :: [Cell]
infiCellAlt = [Alive,Dead] ++ infiCellAlt

createCells :: Int -> Int -> ZZ Cell
createCells width height = ZZ (Z rows (head rows) rows)
    where cells = take (width `div` 4 - 1) infiCellAlt
          rows = replicate (height `div` 2 - 1) $ Z cells Alive cells


gameOfLifeRules :: ZZ Cell -> Cell
gameOfLifeRules cells = case extract cells of
                          Dead -> if alive == 3 then Alive else Dead
                          Alive -> if alive `elem` [2,3] then Alive else Dead
    where alive = length . filter (==Alive) $ getNeighbors cells

gameOfLife :: ZZ Cell -> [ZZ Cell]
gameOfLife = iterate (extend gameOfLifeRules)

microsecondsInSecond :: Int
microsecondsInSecond = 1000000

customAnimate :: Int -> [a] -> (a -> String) -> IO ()
customAnimate delay states displayFunction = do
    forM_ states (\state -> do putStrLn $ displayFunction state
                               threadDelay delay
                               setCursorPosition 0 0)

animate :: Show a => Int -> [a] -> IO ()
animate delay states = customAnimate delay states show

animateNoCursor :: Show a => Int -> [a] -> IO ()
animateNoCursor delay states = customAnimate delay states $ filterReplace "()" ' ' . show

evolutionsPerSecond :: Int
evolutionsPerSecond = 6

main :: IO ()
main = do
    (height, width) <- fromMaybe (-1, -1) <$> getTerminalSize
    putStrLn $ "The terminal dimensions are " ++ show width ++ "x" ++ show height
    threadDelay $ 2 * microsecondsInSecond

    hideCursor
    clearScreen
    setCursorPosition 0 0
    animate
        (microsecondsInSecond `div` evolutionsPerSecond) 
        (gameOfLife $ createCells width height)
