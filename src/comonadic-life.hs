import Control.Comonad
import Data.List (intercalate)
import System.Process (system)
import Control.Concurrent (threadDelay)

data Cell = Alive | Dead deriving (Eq, Enum)

instance Show Cell where
    show cell = case cell of
                  Alive -> "O"
                  Dead  -> "."

data Zipper a = Zipper [a] a [a] deriving Eq

instance Functor Zipper where
    fmap function (Zipper left mid right) = Zipper
        (function <$> left)
        (function mid)
        (function <$> right)

instance Show a => Show (Zipper a) where
    show (Zipper left mid right) = (show . reverse) left
                                ++ " " ++ show mid ++ " "
                                ++ show right

shiftRight :: Zipper a -> Zipper a
shiftRight (Zipper left mid (r:rs)) = Zipper (mid:left) r rs
shiftRight zipper@(Zipper _ _ []) = zipper

shiftLeft :: Zipper a -> Zipper a
shiftLeft (Zipper (l:ls) mid right) = Zipper ls l (mid:right)
shiftLeft zipper@(Zipper [] _ _) = zipper

instance Comonad Zipper where
    extract (Zipper _ mid _) = mid
    duplicate zipper@(Zipper left _ right) = Zipper lefts zipper rights
        where lefts = tail $ scanl (\z _ -> shiftLeft z) zipper left
              rights = tail $ scanl (\z _ -> shiftRight z) zipper right

newtype Zipper2D a = Zipper2D (Zipper (Zipper a)) deriving Eq

instance Functor Zipper2D where
    fmap function (Zipper2D zipper2D) = Zipper2D $
        (fmap . fmap) function zipper2D

instance Show a => Show (Zipper2D a) where
    show (Zipper2D (Zipper upZips (Zipper left mid right) downZips)) =
        intercalate "\n" [
            intercalate "\n" ((map show . reverse) upZips),
            (show . reverse) left ++ "{" ++ show mid ++ "}" ++ show right,
            intercalate "\n" (map show downZips)]

instance Comonad Zipper2D where
    extract (Zipper2D zipper2D) = extract . extract $ zipper2D
    duplicate (Zipper2D zipper2D) = Zipper2D <$> Zipper2D ((dupe . dupe) zipper2D)
        where dupe zipper = Zipper (lefts zipper) zipper (rights zipper)
              lefts zipper@(Zipper _ (Zipper left _ _) _) =
                  tail $ scanl (\z _ -> shiftLeft <$> z) zipper left
              rights zipper@(Zipper _ (Zipper _ _ right) _) =
                  tail $ scanl (\z _ -> shiftRight <$> z) zipper right

goUp :: Zipper2D a -> Zipper2D a
goUp (Zipper2D zipper2D) = Zipper2D $ shiftLeft zipper2D

goDown :: Zipper2D a -> Zipper2D a
goDown (Zipper2D zipper2D) = Zipper2D $ shiftRight zipper2D

goLeft :: Zipper2D a -> Zipper2D a
goLeft (Zipper2D zipper2D) = Zipper2D $ shiftLeft <$> zipper2D

goRight :: Zipper2D a -> Zipper2D a
goRight (Zipper2D zipper2D) = Zipper2D $ shiftRight <$> zipper2D

getNeighbors :: Zipper2D a -> [a]
getNeighbors zipper2D@(Zipper2D (Zipper up (Zipper left _ right) down)) = extract <$> neighbors
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

exampleCells :: Zipper2D Cell
exampleCells = Zipper2D (Zipper rows (head rows) rows)
    where cells = take 36 infiCellAlt
          rows = replicate 12 $ Zipper cells Alive cells

class Display a where
    display :: a -> String

instance Show a => Display (Zipper a) where
    display (Zipper left mid right) = concat ((map show . reverse) left
                                   ++ [show mid]
                                   ++ map show right)

instance Show a => Display (Zipper2D a) where
    display (Zipper2D (Zipper upZips midZip downZips)) =
        intercalate "\n" [
            intercalate "\n" ((map display . reverse) upZips),
            display midZip,
            intercalate "\n" (map display downZips)]

gameOfLifeRules :: Zipper2D Cell -> Cell
gameOfLifeRules cells = case cell of
                          Dead -> if alive == 3 then Alive else Dead
                          Alive -> if alive `elem` [2,3] then Alive else Dead
    where cell = extract cells
          neighbors = getNeighbors cells
          alive = length . filter (==Alive) $ neighbors

gameOfLife :: Zipper2D Cell -> [Zipper2D Cell]
gameOfLife = iterate (extend gameOfLifeRules)

microSecondsInSecond :: Int
microSecondsInSecond = 1000000

customAnimate :: Display a => Int -> [a] -> (a -> String) -> IO ()
customAnimate delay states displayFunction = do
    mapM_
        (\state -> do
            putStrLn $ displayFunction state
            threadDelay delay
            system "cls"
        ) states

animate :: Display a => Int -> [a] -> IO ()
animate delay states = customAnimate delay states display

main :: IO ()
main = do
    _ <- system "cls"
    animate (microSecondsInSecond `div` 3) (gameOfLife exampleCells)
