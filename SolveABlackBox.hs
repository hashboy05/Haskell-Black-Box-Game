import Data.List
import Data.Char
import System.IO

type Pos = (Int,Int)
data Side = North | East | South | West deriving (Show,Eq,Ord)
type EdgePos = (Side,Int)
type Atoms = [Pos]
type Interactions = [(EdgePos,Marking)]
data Marking = Absorb | Reflect | Path EdgePos deriving (Show,Eq)

--This code starts from here. It checks whether there are 0 atoms or empty interactions first, otherwise the code will run to check for atoms.
solveBB :: Int -> Interactions -> [Atoms]
solveBB 0 _ = []
solveBB _ [] = []
solveBB n interactions = printAtoms (everyCombinations n (getGridCoor (maxValue))) interactions maxValue
  where
    maxValue = maximum (getGridSize interactions)

--This function prints out all the possible atoms lists that satisfy the interactions, and this function recurses until there are no more atoms to check.
printAtoms :: [Atoms] -> Interactions -> Int -> [Atoms]
printAtoms [] interactions n = []
printAtoms (x:xs) interactions n = if (containInteractions interactions (calcBBInteractions n x)) then [x] ++ printAtoms xs interactions n else printAtoms xs interactions n

--Checks if the input interactions are valid by using the function from challenge 1.
containInteractions :: Interactions -> Interactions -> Bool
containInteractions input interactionsFrom1 = all (`elem` interactionsFrom1) input

--This function prints out all the coordinates inside the grid using list comprehension. It takes in the maximum number from the function getGridSize and generates the grid according to the maximum number.
getGridCoor :: Int -> [(Int,Int)]
getGridCoor n = [(x,y) | x <- [1..n], y <- [1..n]]

--This function gets the list of the values from the interactions by checking the entry number and the marking number, and returns the list of the numbers.
getGridSize :: Interactions -> [Int]
getGridSize [] = []
getGridSize (((_,b),Path(_,c)):xs) = b : c : getGridSize xs
getGridSize (((_,b),_):xs) = b : getGridSize xs

--This function prints out all the possible atom combinations according to the number of atoms you input.
everyCombinations :: Int -> Atoms -> [Atoms]
everyCombinations 0 _ = [[]]
everyCombinations _ [] = []
everyCombinations n (x:xs) = map (x:) (everyCombinations (n-1) xs) ++ everyCombinations n xs

--Everything below here is from challenge 1. It is used to check for interactions in the function solveBB.
calcBBInteractions :: Int -> Atoms -> Interactions
calcBBInteractions n atoms = [((dir,a),calcOutput (dir,a) atoms n) | dir <- [North,East,South,West], a <- [1..n]]

calcOutput :: EdgePos -> Atoms -> Int -> Marking
calcOutput (dir,a) atoms n | dir == North = getMarking a 1 False False True False atoms n
                         | dir == East = getMarking n a False False False True atoms n
                         | dir == South = getMarking a n True False False False atoms n
                         | otherwise = getMarking 1 a False True False False atoms n

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

isAbsorbed :: Int -> Int -> Atoms -> Bool
isAbsorbed x y atoms = (x,y) `elem` atoms

isReflected :: Int -> Int -> Bool -> Bool -> Bool -> Bool -> Atoms -> Int -> Bool
isReflected x y north east south west atoms n
  | ((x-1,y) `elem` atoms || (x+1,y) `elem` atoms) && (north || south) = True
  | ((x,y-1) `elem` atoms || (x,y+1) `elem` atoms) && (east || west) = True
  | otherwise = False

isDeflected :: Int -> Int -> Bool -> Bool -> Bool -> Bool -> Atoms -> Int -> Marking
isDeflected x y north east south west atoms n
  | (x+1,y+1) `elem` atoms && (x,y+1) `notElem` atoms && (x+1,y) `notElem` atoms = if south then getMarking (x-1) y False False False True atoms n else getMarking x (y-1) True False False False atoms n
  | (x-1,y+1) `elem` atoms && (x,y+1) `notElem` atoms && (x-1,y) `notElem` atoms = if south then getMarking (x+1) y False True False False atoms n else getMarking x (y-1) True False False False atoms n
  | (x-1,y-1) `elem` atoms && (x,y-1) `notElem` atoms && (x-1,y) `notElem` atoms = if north then getMarking (x+1) y False True False False atoms n else getMarking x (y+1) False False True False atoms n
  | (x+1,y-1) `elem` atoms && (x,y-1) `notElem` atoms && (x+1,y) `notElem` atoms = if north then getMarking (x-1) y False False False True atoms n else getMarking x (y+1) False False True False atoms n
  | otherwise = goStraight x y north east south west atoms n

goStraight :: Int -> Int -> Bool -> Bool -> Bool -> Bool -> Atoms -> Int -> Marking
goStraight x y north east south west atoms n
  | south = getMarking x (y+1) north east south west atoms n
  | north = getMarking x (y-1) north east south west atoms n
  | east = getMarking (x+1) y north east south west atoms n
  | west = getMarking (x-1) y north east south west atoms n