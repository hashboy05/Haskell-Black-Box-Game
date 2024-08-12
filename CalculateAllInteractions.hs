import Data.List
import Data.Char
import System.IO

type Pos = (Int,Int)
data Side = North | East | South | West deriving (Show,Eq,Ord)
type EdgePos = (Side,Int)
type Atoms = [Pos]
type Interactions = [(EdgePos,Marking)]
data Marking = Absorb | Reflect | Path EdgePos deriving (Show,Eq)

--The code starts from here. This function applies list comprehension using the data Side and Integers from 1 to grid size n.
calcBBInteractions :: Int -> Atoms -> Interactions
calcBBInteractions n atoms = [((dir,a),calcOutput (dir,a) atoms n) | dir <- [North,East,South,West], a <- [1..n]]

--This function calculates the output of each EdgePos. It checks the direction the ray is coming from. It then calls the function getMarking.
calcOutput :: EdgePos -> Atoms -> Int -> Marking
calcOutput (dir,a) atoms n | dir == North = getMarking a 1 False False True False atoms n
                         | dir == East = getMarking n a False False False True atoms n
                         | dir == South = getMarking a n True False False False atoms n
                         | otherwise = getMarking 1 a False True False False atoms n

--This function uses four booleans that each represent the direction that the ray is going, with each being north, east, south and west respectively. The function will recurse until the ray is out of the grid's range (or reaches a different point of the grid), gets absorbed or reflected.
getMarking :: Int -> Int -> Bool -> Bool -> Bool -> Bool -> Atoms -> Int -> Marking
getMarking x y north east south west atoms n
  | x `elem` [0,n+1] || y `elem` [0,n+1] = printPath
  | isAbsorbed x y atoms = Absorb
  | isReflected x y north east south west atoms n = Reflect
  | otherwise = isDeflected x y north east south west atoms n
  where
    printPath
      | north = Path (North,x)
      | east = Path (East,y)
      | south = Path (South,x)
      | west = Path (West,y)

--This function checks if the ray is absorbed.
isAbsorbed :: Int -> Int -> Atoms -> Bool
isAbsorbed x y atoms = (x,y) `elem` atoms

--This function checks if the ray is reflected.
isReflected :: Int -> Int -> Bool -> Bool -> Bool -> Bool -> Atoms -> Int -> Bool
isReflected x y north east south west atoms n
  | ((x-1,y) `elem` atoms || (x+1,y) `elem` atoms) && (north || south) = True
  | ((x,y-1) `elem` atoms || (x,y+1) `elem` atoms) && (east || west) = True
  | otherwise = False

--This function checks if the ray is deflected or goes straight.
isDeflected :: Int -> Int -> Bool -> Bool -> Bool -> Bool -> Atoms -> Int -> Marking
isDeflected x y north east south west atoms n
  | (x+1,y+1) `elem` atoms && (x,y+1) `notElem` atoms && (x+1,y) `notElem` atoms = if south then getMarking (x-1) y False False False True atoms n else getMarking x (y-1) True False False False atoms n
  | (x-1,y+1) `elem` atoms && (x,y+1) `notElem` atoms && (x-1,y) `notElem` atoms = if south then getMarking (x+1) y False True False False atoms n else getMarking x (y-1) True False False False atoms n
  | (x-1,y-1) `elem` atoms && (x,y-1) `notElem` atoms && (x-1,y) `notElem` atoms = if north then getMarking (x+1) y False True False False atoms n else getMarking x (y+1) False False True False atoms n
  | (x+1,y-1) `elem` atoms && (x,y-1) `notElem` atoms && (x+1,y) `notElem` atoms = if north then getMarking (x-1) y False False False True atoms n else getMarking x (y+1) False False True False atoms n
  | otherwise = goStraight x y north east south west atoms n

--This function changes the position of the ray.
goStraight :: Int -> Int -> Bool -> Bool -> Bool -> Bool -> Atoms -> Int -> Marking
goStraight x y north east south west atoms n
  | south = getMarking x (y+1) north east south west atoms n
  | north = getMarking x (y-1) north east south west atoms n
  | east = getMarking (x+1) y north east south west atoms n
  | west = getMarking (x-1) y north east south west atoms n