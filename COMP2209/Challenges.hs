{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2022
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (Atoms,Interactions,Pos,EdgePos,Side(..),Marking(..),
                   LamExpr(..),ArithExpr(..),
                   calcBBInteractions,solveBB,prettyPrint,
                   parseArith,churchEnc,innerRedn1,innerArithRedn1,compareArithLam) where


-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.Function
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe, isNothing, fromJust)
import Text.Parsec
import Text.Parsec.String
import Control.Applicative
import Data.Either

instance NFData ArithExpr
instance NFData LamExpr 
instance NFData Marking
instance NFData Side

-- Challenge 1
-- Calculate Interactions in the Black Box
type Atoms = [ Pos ] -- list of positions (tuples of Int) on the grid 
type Interactions = [  ( EdgePos , Marking )  ] -- list of tuples, each containing edge position and a marking
type Pos = (Int, Int)   -- top left is (1,1) , bottom right is (N,N) where N is size of grid
type EdgePos = ( Side , Int ) -- int range is 1 to N where N is size of grid

data Side = North | East | South | West
            deriving (Show, Eq, Ord, Generic)

data Marking =  Absorb | Reflect | Path EdgePos
                deriving (Show, Eq)
{-
1.In the first exercise I helped myself with the following past solution:
Kırgöz, U.C. (2022) UCKIRGOZ/comp2209-cwk, GitHub. Available at: 
https://github.com/uckirgoz/COMP2209-cwk (Accessed: January 1, 2023). 
-}
startCoord :: Int -> [EdgePos]
-- generates a list of EdgePos tuples representing all the positions along the edges of a grid with a given size
startCoord size = [(dir, n) | dir <- [North, South, East, West], n <- [1..size]]

operateStartCoord :: Int -> Atoms -> EdgePos -> (EdgePos,Marking)
-- takes a size and a list of atoms, and an EdgePos and returns a tuple of the EdgePos and a Marking.
operateStartCoord size atom edgePos 
    | null atomsOnWay = (edgePos,pathRay size (x,y) directionOfRay atom)
  -- If there are no atoms on the path of the ray, it returns the EdgePos and the result of the pathRay
  -- function applied to the starting position, the direction of the ray, and the list of atoms
    | length atomsOnWay == 3 = (edgePos,Absorb)
  -- If there are exactly 3 atoms on the path of the ray, it returns the EdgePos and Absorb.
    | otherwise = if nextCoord `elem` atom 
                                then (edgePos,Absorb)
                                else (edgePos,Reflect)
  -- Otherwise, it returns the EdgePos and Reflect if the next position on the path of the ray is not in
  -- the list of atoms, or Absorb if it is.
        where 
            ((x,y), directionOfRay) = startEdgePosition size edgePos
            atomsOnWay = findAtomsOnWay (x,y) atom directionOfRay
            nextCoord   | directionOfRay == North = (x,y-1)
                        | directionOfRay == South = (x,y+1)
                        | directionOfRay == East = (x-1,y)
                        | directionOfRay == West = (x-1,y)  

{-Accepts size and EdgePos and returns a tuple of Pos and Side. Pos is the starting position of the 
of the ray in the grid, and Side is the direction in which the ray is moving. The function uses pattern matching 
to determine Pos and Side based on the specified EdgePos.-}        
startEdgePosition :: Int -> EdgePos -> (Pos,Side)
startEdgePosition x (side,n) | side == North = ((n,0),South)
                             | side == South = ((n,x+1),North)
                             | side == East = ((x+1,n),West)
                             | side == West = ((0,n),East)
-- The Pos is the starting position of the ray on the grid and the Side is the direction the ray is moving in                             

{-accepts Size and Pos and returns the corresponding EdgePos if the position is on the edge of the grid. 
If the position is not on the grid edge, an error is returned. The function uses protections to determine 
EdgePos based on the specified Pos.-}
finalEdgePosition :: Int -> Pos -> EdgePos
finalEdgePosition size (x,y) | y == 0 = (North,x)
                             | y == size + 1 = (South,x)
                             | x == size + 1 = (East,y)
                             | x == 0 = (West,y)       

{-The finalEdgeColumn function takes a size and a position (x, y) and returns the x coordinate if y is 0 
or size+1, and y otherwise.-}                                             
finalEdgeColumn :: Int -> Pos -> Int
finalEdgeColumn size (x, y) = if y == 0 || y == size + 1 then x else y
                                     
findAtomsOnWay :: Pos -> Atoms -> Side -> Atoms 
{-The findAtomsOnWay function takes a position (x, y), a list of atoms and a direction side and returns 
a list of atoms that are in the path of the ray in the given direction. It does this using guards 
to determine a list of neighbors based on the side value, then filters that list to include only atoms 
that are present in the list of atoms.-}
findAtomsOnWay (x,y) atoms side =
    foldl (\acc position -> if position `elem` atoms then position:acc else acc ) [] neighbors
  where neighbors   | side == North = [(x-1,y-1),(x,y-1),(x+1,y-1)]
                    | side == South = [(x-1,y+1),(x,y+1),(x+1,y+1)]
                    | side == East = [(x+1,y-1),(x+1,y),(x+1,y+1)]
                    | side == West = [(x-1,y-1),(x-1,y),(x-1,y+1)]

pathRay :: Int -> Pos -> Side -> Atoms -> Marking
-- takes a size, a position, a direction, and a list of atoms, and returns a Marking
pathRay size (x,y) side atoms 
    | Left move <- followingPosition (x,y) side atoms = move
-- If the followingPosition function applied to the position, the direction, and the atoms returns a Left Marking, 
-- the function returns that marking. 
    | Right (newPosition,newSide) <- followingPosition (x,y) side atoms, positiveEdge size newPosition =
        if newSide == East
        then Path (East, finalEdgeColumn size newPosition)
        else Path (finalEdgePosition size newPosition)
-- If it returns a Right (Pos, Side), and the new position is on the edge of the grid, it returns a Path EdgePos 
-- representing the final position of the ray.
    | Right (newPosition,newSide) <- followingPosition (x,y) side atoms = pathRay size newPosition newSide atoms
-- it applies the pathRay function recursively to the new position and direction.          

{-The positiveEdge function takes size and position as input and returns True if the position is on the edge of the 
of the grid and False otherwise. This is done by checking whether the x or y coordinate of the position is 0 or 
the grid size plus 1.-}
positiveEdge :: Int -> Pos -> Bool
positiveEdge size (x, y) = x == 0 || y == 0 || x == size + 1 || y == size + 1

{-The positiveSide function takes an integer n representing the grid size and position (x, y) and returns 
the side of the grid on which the position is located if it is on the edge of the grid, or Nothing if it is not.-}
positiveSide :: Int -> Pos -> Side
positiveSide n (x,y) | x<=n && x>=1 && y==0 = North
                     | x<=n && x>=1 && y==n+1 = South
                     | x==n+1 && y<=n && y>=1 = East
                     | x==0 && y<=n && y>=1 = West

{-accepts position (x,y), direction side and atoms list as input and returns or left marking if 
ray is to be absorbed or reflected, or a right tuple (Pos,Side) representing the next position and direction
The function first determines the number of atoms in the path of the beam using the findAtomsOnWay function 
and stores it in the variable nextAtoms. It then uses pattern matching to process the different cases based on the 
If there are no atoms in the ray path, the function returns a right value that is equal to 
tuple containing the next position (x',y') and the current side' direction of the ray. 
of the ray, the function checks if the atom is at the current position (x,y) of the ray. If it is, the function returns 
left mark Absorb. If it is not, it returns a right tuple containing the next position (x'',y'') and the direction 
side''' of the ray after its reflection. if there are two atoms in the path of the ray, the function checks whether the 
the current position (x,y) of the ray is in the list of atoms. If it is, it returns the left mark Absorb. If it is 
If there are three atoms in the ray path, the function returns a left mark Reflect.If there are three atoms in the ray path, the function returns a left mark 
Absorb.-}
followingPosition :: Pos -> Side -> Atoms -> Either Marking (Pos,Side)
followingPosition (x,y) side atoms  | side == North = case length nextAtoms of
                                        0 -> Right ((x,y-1),North)
                                        1 ->let atom = head nextAtoms in
                                             if atom == (x-1,y-1)
                                                then Right ((x+1,y),East)
                                                else 
                                                    if atom == (x,y-1) 
                                                        then Left Absorb
                                                        else Right ((x-1,y),West)
                                        2 -> let atom = head nextAtoms in
                                             if (x,y-1) `elem` nextAtoms 
                                                then Left Absorb
                                                else Left Reflect
                                        3 -> Left Absorb
                                    | side == South = case length nextAtoms of
                                        0 -> Right((x,y+1),South)
                                        1 -> if atom == (x-1,y+1) 
                                                then Right ((x+1,y),East)
                                                else 
                                                    if atom == (x,y+1) 
                                                        then Left Absorb
                                                        else Right ((x-1,y),West)
                                        2 -> if (x,y+1) `elem` nextAtoms 
                                                then Left Absorb
                                                else Left Reflect
                                        3 -> Left Absorb
                                    | side == East = case length nextAtoms of
                                        0 -> Right((x+1,y),East)
                                        1 -> if atom == (x+1,y-1) 
                                                then Right ((x,y+1),South)
                                                else 
                                                    if atom == (x+1,y) 
                                                        then Left Absorb
                                                        else Right ((x,y-1),North)
                                        2 -> if (x+1,y) `elem` nextAtoms 
                                                then Left Absorb
                                                else Left Reflect
                                        3 -> Left Absorb
                                    | side == West = case length nextAtoms of
                                        0 -> Right((x-1,y),East)
                                        1 -> if atom == (x-1,y-1) 
                                                then Right ((x,y+1),South)
                                                else 
                                                    if atom == (x-1,y) 
                                                        then Left Absorb
                                                        else Right ((x,y-1),North)
                                        2 -> if (x-1,y) `elem` nextAtoms 
                                                then Left Absorb
                                                else Left Reflect
                                        3 -> Left Absorb    
        where nextAtoms = findAtomsOnWay (x,y) atoms side
              atom = head nextAtoms 

{-The calcBBInteractions function takes three arguments: an integer size representing the size of the grid, a list of
of atoms located in the network, and a list of EdgePos entry points at the edge of the network. It returns a list of 
of interactions that are tuples containing EdgePos and markup. the function first sorts the list of EdgePos 
by the first element of the tuple, using the sortBy function and the compare function. It then uses the 
the map function to apply a function to each EdgePos in the sorted list. the function applied to each EdgePos starts with 
using the startEdgePosition function to find the start position and direction of the ray corresponding to the 
EdgePos. The findAtomsOnWay function is then used to find the atoms along the ray path in the given direction. 
The number of atoms along the path is calculated using the length function, and stored in the numAtomsOnPath variable.
The nextPosition function is also used to find the next position along the beam path.If the number of atoms along the beam path 
or the next position on the path is in the list of atoms, the function returns a tuple containing EdgePos 
and the Absorb marker. Otherwise, it returns a tuple containing the EdgePos and the result of the pathRay function 
applied to the next position of the path, beam direction, and atom list.-}
calcBBInteractions :: Int -> Atoms -> [EdgePos] -> Interactions
calcBBInteractions size atoms edgePositions = sortBy (\x y -> compare (fst x) (fst y)) (map (\ edgePos -> 
            let
              (x, y) = startEdgePosition size edgePos
              side = snd (startEdgePosition size edgePos)
              atomsOnPath = findAtomsOnWay x atoms side
              numAtomsOnPath = length atomsOnPath
              nextPos = nextPosition x side
            in
            (if (numAtomsOnPath == 3) || (nextPos `elem` atoms) 
            then
                (edgePos, Absorb)
            else
                (edgePos, pathRay size nextPos side atoms))) edgePositions)

{-The nextPosition function takes as input a position (x,y) and a direction side and returns the grid position in 
in the specified direction. It uses pattern matching on the side value to determine the next position based on the direction.-}
nextPosition :: Pos -> Side -> Pos
nextPosition (x,y) side = case side of
    North -> (x,y-1)
    South -> (x,y+1)
    East -> (x+1,y)
    West -> (x-1,y)

{-Additional test cases for the calcBBInteractions function:
(1)Test a grid with no atoms:
Input:
calcBBInteractions 3 [] [(North, 1), (North, 2), (North, 3), (East, 1), (East, 2), (East, 3), (South, 1), (South, 2), 
(South, 3), (West, 1), (West, 2), (West, 3)]

(2)Test a grid with atoms on all positions:
Input:
calcBBInteractions 3 [(1, 1), (1, 2), (1, 3), (2, 1), (2, 2), (2, 3), (3, 1), (3, 2), (3, 3)] [(North, 1), (North, 2),
(North, 3), (East, 1), (East, 2), (East, 3), (South, 1), (South, 2), (South, 3), (West, 1), (West, 2), (West, 3)]
Output:
[((North,1),Absorb),((North,2),Absorb),((North,3),Absorb),((East,1),Absorb),((East,2),Absorb),((East,3),Absorb),
((South,1),Absorb),((South,2),Absorb),((South,3),Absorb),((West,1),Absorb),((West,2),Absorb),((West,3),Absorb)]

(3)Test a grid with atoms on only some positions:
Input:
calcBBInteractions 3 [(1, 1), (1, 3), (3, 1), (3, 3)] [(North, 1), (North, 2), (North, 3), (East, 1), (East, 2), 
(East, 3), (South, 1), (South, 2), (South, 3), (West, 1), (West, 2), (West, 3)]
Output:
[((North,1),Absorption),((North,2),Swallow),((North,3),Absorption),((East,1),Swallow),((East,2),Swallow),
((East,3),Swallow),((South,1),Absorption),((South,2),Swallow),((South,3),Absorption),((West,1),Swallow),
((West,2),Swallow),((West,3),Swallow)]

(4)Test a larger grid with a mix of atoms and empty positions:
Input:
calcBBInteractions 4 [(1, 1), (1, 4), (4, 1), (4, 4)] [(North, 1), (North, 2),

(5)Test case with a grid of size 3 and atoms at the center and two adjacent corners:
Input:
calcBBInteractions 3 [(1, 1), (3, 1), (3, 3)] [(North, 1), (East, 2), (South, 3), (West, 3)]
Output:
[(North, Absorb), (East, Absorb), (South, Absorb), (West, Absorb)]
-}


-- Challenge 2
-- Find atoms in a Black Box
{-
For the second exercise, which I was unable to complete, I used a feature similar to one of 
last year's. I also tried to apply the Alpha-Beta Pruning algorithm, but unfortunately this 
was a failed attempt. This is the quote for the second exercise:

Hristov, E. (2022) Haskell-coursework/challenges.hs at master · emiliyanhristov/haskell-coursework, 
GitHub. Available at: https://github.com/emiliyanhristov/Haskell-Coursework/blob/master/Challenges.hs 
(Accessed: January 1, 2023). 
-}
--compares the calculated interactions to the given partial list of interactions, 
--and if they match, it returns the current list of atoms as the solution. 
--If not, it continues searching through the remaining lists of atoms until a solution is found or the list of atoms is exhausted.
findSolution :: Int -> [Atoms] -> Interactions -> [EdgePos] -> Atoms
findSolution n [] _ _ = []
findSolution n (a:ats) ints edgePos | calcBBInteractions n a edgePos == ints = a
                                    | otherwise = findSolution n ats ints edgePos 

{-The nonFoundAtom function generates a list of one atom that is a tuple of two integers representing the row and column indices 
of the atom in the lattice. The function accepts three arguments:-}
nonFoundAtom :: Int -> Int -> Int -> [Atoms]
nonFoundAtom size atomRow atomCol 
    | atomCol <= size = [(atomRow,atomCol)] : nonFoundAtom size atomRow (atomCol + 1)
    | atomRow < size = nonFoundAtom size (atomRow + 1) 1
    | otherwise = [] 

{-generates a list of all possible combinations of two atoms that are not present in the input list of atoms. The function accepts 
the size of the lattice, the positions of the first atom in the row and column, and the positions of the second atom in the row and column. 
It uses recursion to go through all possible positions for the second atom, and then through all possible positions for the 
first atom until all possible combinations are generated.-}
twoNonFoundAtoms :: Int -> Int -> Int -> Int -> Int -> [Atoms]
twoNonFoundAtoms size fstAtomRow fstAtomCol sndAtomRow sndAtomCol 
    | sndAtomCol <= size = [(fstAtomRow,fstAtomCol),(sndAtomRow,sndAtomCol)] : twoNonFoundAtoms size fstAtomRow fstAtomCol sndAtomRow (sndAtomCol + 1)
    | sndAtomRow < size = twoNonFoundAtoms size fstAtomRow fstAtomCol (sndAtomRow + 1) 1
    | fstAtomCol < size = twoNonFoundAtoms size fstAtomRow (fstAtomCol + 1) 1 1
    | fstAtomRow < size = twoNonFoundAtoms size (fstAtomRow + 1) 1 1 1
    | otherwise = []

{-similar to the twoNonFoundAtoms function, but generates a list of all possible combinations of three atoms that are not present 
in the input list of atoms. It takes the same parameters as the twoNonFoundAtoms function, adding the rows and columns 
of the third atom.-}
threeNonFoundAtoms :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Atoms]
threeNonFoundAtoms size fstAtomRow fstAtomCol sndAtomRow sndAtomCol thirdAtomRow thirdAtomCol
    | thirdAtomCol <= size = [(fstAtomRow,fstAtomCol),(sndAtomRow,sndAtomCol),(thirdAtomRow,thirdAtomCol)] : threeNonFoundAtoms size fstAtomRow fstAtomCol sndAtomRow sndAtomCol thirdAtomRow (thirdAtomCol + 1)
    | thirdAtomRow < size = threeNonFoundAtoms size fstAtomRow fstAtomCol sndAtomRow sndAtomCol (thirdAtomRow + 1) thirdAtomCol
    | sndAtomCol < size = threeNonFoundAtoms size fstAtomRow fstAtomCol sndAtomRow (sndAtomCol + 1) 1 1
    | sndAtomRow < size = threeNonFoundAtoms size fstAtomRow fstAtomCol (sndAtomRow + 1) 1 1 1
    | fstAtomCol < size = threeNonFoundAtoms size fstAtomRow (fstAtomCol + 1) 1 1 1 1
    | fstAtomRow < size = threeNonFoundAtoms size (fstAtomRow + 1) 1 1 1 1 1
    | otherwise = []

{-combines the input list of atoms with a list of atoms generated by the twoNonFoundAtoms or threeNonFoundAtoms functions, and returns 
a list of all possible combinations of atoms. The function accepts the input list of atoms and the list of atoms generated by the 
twoNonFoundAtoms or threeNonFoundAtoms, and uses recursion to step through the list of atoms generated by twoNonFoundAtoms 
or threeNonFoundAtoms and combines them with the input list of atoms. Returns a list of all possible combinations of atoms, 
with the shortest list of atoms being the first-}
combineAtoms :: Atoms -> [Atoms] -> [Atoms]
combineAtoms atoms [] = [atoms]
combineAtoms atoms (x:xs) = 
    let minAtoms = if length atoms < length x then atoms else x
        restAtoms = if length atoms < length x then x else atoms
    in (minAtoms ++ restAtoms) : combineAtoms atoms xs

{-accepts a list of interactions and returns a list of atoms. This is done by first splitting the interactions into four lists based on 
direction of the beam: "north", "south", "east" and "west". It then applies the "movementsInOrder" function to each of these lists and 
combines the resulting lists of atoms into a single list using the "++" operator. The "movementsInOrder" function is presumably used to determine the 
the atoms based on the interactions.-}
combineAtomSearches :: Interactions -> Atoms
combineAtomSearches partialInteractions = nub $ [fstNorthAtom north west] ++ [fstSouthAtom south westRight] ++ [fstEastAtom east northRight] ++ [fstWestAtom west north] ++ 
 [fstNorthAtomReverse northRight east] ++ [fstSouthAtomReverse southRight eastRight] ++ [fstEastAtomReverse eastRight southRight] ++ [fstWestAtomReverse westRight south]
  where
    north = movementsInOrder (movementsNorth partialInteractions) 1 (lastCoordinate (movementsNorth partialInteractions) 0) []
    east = movementsInOrder (movementsSouth partialInteractions) 1 (lastCoordinate (movementsSouth partialInteractions) 0) []
    south = movementsInOrder (movementsEast partialInteractions) 1 (lastCoordinate (movementsEast partialInteractions) 0) []
    west = movementsInOrder (movementsWest partialInteractions) 1 (lastCoordinate (movementsWest partialInteractions) 0) []
    northRight = reverse $ movementsInOrder (movementsNorth partialInteractions) 1 (lastCoordinate (movementsNorth partialInteractions) 0) []
    eastRight = reverse $ movementsInOrder (movementsSouth partialInteractions) 1 (lastCoordinate (movementsSouth partialInteractions) 0) []
    southRight = reverse $ movementsInOrder (movementsEast partialInteractions) 1 (lastCoordinate (movementsEast partialInteractions) 0) []
    westRight = reverse $ movementsInOrder (movementsWest partialInteractions) 1 (lastCoordinate (movementsWest partialInteractions) 0) []

-- removes duplicate atoms from a list of atoms by iterating through the list and keeping only the atoms that are no longer present in the list
rmdupAtoms :: Atoms -> Atoms 
rmdupAtoms [] = []
rmdupAtoms (a:atoms) 
  | a `elem` atoms = rmdupAtoms atoms
  | otherwise = a : rmdupAtoms atoms

{-ranks a list of atoms based on the first element (which is probably the x coordinate) of each atom. This is done using the "sortBy" function. 
and compares the first elements of each atom using the "compare" function and the "on" function.-}
orderFstAtom ::(Ord a, Ord b) => [(a, b)] -> [(a, b)] 
orderFstAtom = sortBy (on compare fst)

{-sorts a list of tuples by the second tuple element in ascending order. This function is probably used to organize the list of atoms in the 
to be sorted in a particular order, possibly to make it easier to process or to compare it with other lists of atoms.-}
orderSndAtom :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
orderSndAtom = sortBy (on compare snd)

{-filter a list of interactions and return only those interactions that involve movement in the specified direction. For example, the function 
"movementsNorth" will return a list of interactions that involve movement in the north direction. These functions can be used to split 
into separate groups based on the direction of movement, which can be useful for processing interactions in a particular order. 
Or for comparing interactions based on their direction.-}
movementsNorth :: Interactions -> Interactions
movementsNorth [] = []
movementsNorth (x:xs) 
    | fst(fst x) == North = x : movementsNorth xs
    | otherwise = movementsNorth xs

movementsSouth :: Interactions -> Interactions
movementsSouth [] = []
movementsSouth (x:xs) 
    | fst(fst x) == South = x : movementsSouth xs
    | otherwise = movementsSouth xs

movementsEast :: Interactions -> Interactions
movementsEast [] = []
movementsEast (x:xs) 
    | fst(fst x) == East = x : movementsEast xs
    | otherwise = movementsEast xs

movementsWest :: Interactions -> Interactions
movementsWest [] = []
movementsWest (x:xs) 
    | fst(fst x) == West = x : movementsWest xs
    | otherwise = movementsWest xs

{-accepts a list of interactions, counter, limit and accumulator. It goes through the list of interactions and checks if the second 
tuple of the first element of each interaction (snd(fst x)) is equal to the counter. If so, it adds the interaction to the 
If it isn't, it continues scrolling through the list without adding the interaction to the accumulator. 
This function is probably used to ensure that the interactions are in a particular order, possibly based on the counter and the limit provided-}
movementsInOrder :: Interactions -> Int -> Int -> Interactions -> Interactions
movementsInOrder [] _ _ accumulator = accumulator
movementsInOrder (x:xs) counter limit accumulator
    | snd(fst x) == counter = movementsInOrder xs (counter + 1) limit (x:accumulator)
    | otherwise = movementsInOrder xs counter limit accumulator

{-accepts an interaction and checks that the second element of the interaction (the mark) is equal to "Absorb". If it is, it returns True, 
otherwise, returns False. This function is probably used to check whether a particular interaction absorbs a ray-}
isAbsorbed :: (EdgePos, Marking) -> Bool
isAbsorbed interaction = case snd interaction
    of
        Absorb -> True
        _ -> False

{-assumes an edge position and a list of interactions. Checks whether any of the interactions in the list have the same edge position as the 
and a marking of "Absorb". If such interactions are found, True is returned, otherwise False is returned. This function is 
is probably used to check whether a particular edge position is absorbed by the list of interactions-}
isAbsorbingPos :: EdgePos -> Interactions -> Bool
isAbsorbingPos edgePos = any (\ (position, marking) -> position == edgePos && marking == Absorb)

{-determines whether an interaction is a reflection or not. It takes an interaction as input and returns a boolean value indicating 
whether the interaction is a reflection or not.-}
isReflected :: (EdgePos, Marking) -> Bool
isReflected interaction = case snd interaction 
    of
        Reflect -> True 
        _ -> False

--accepts a list of interactions as input and returns the position of the first absorption in the list or 0 if there are no absorptions in the list
fstAbsorbtion :: Interactions -> Int
fstAbsorbtion [] = 0
fstAbsorbtion (x:xs)
    | snd x == Absorb = snd (fst x)
    | otherwise = fstAbsorbtion xs

--similar to the fstAbsorbtion function, but returns the position of the first reflection in the list, or 0 if there are no reflections in the list.
fstReflection :: Interactions -> Int
fstReflection [] = 0
fstReflection (x:xs)
    | snd x == Reflect = snd (fst x)
    | otherwise = fstReflection xs

{-finding the last coordinate in a list of interactions. It takes a list of interactions and a starting coordinate as input and returns 
the last coordinate in the list of interactions. It does this by iterating through the list of interactions and updating the coordinate 
if the second interaction element is greater than the current coordinate. If this is not the case, it continues iterating through the list 
without updating the coordinate-}
lastCoordinate:: Interactions -> Int -> Int
lastCoordinate[] coords = coords
lastCoordinate (x:xs) coords 
    | snd (fst x) > coords = lastCoordinate xs (snd(fst x))
    | otherwise = lastCoordinate xs coords

{-check if an interaction has a return path. It takes as input an interaction and a list of interactions and returns the position 
of the return path if it exists, or 0 if it does not exist. It does this by iterating through the list of interactions and checking whether 
the second interaction element is a "Path" interaction with the same direction as the input interaction. If so, it is returned 
the position of the return path. If it is not, it continues scrolling through the list without returning a value.-}
hasReversePath :: (EdgePos,Marking) -> Interactions -> Int
hasReversePath _ [] = 0
hasReversePath x (n:ns)
    | snd x == Path (fst n) && snd n == Path (fst x) = snd (fst n)
    | otherwise = hasReversePath x ns

{-finding the position of the first atom in the northern row of the lattice. It assumes two lists of interactions, one for the north order and one for the 
for the western row as input and returns the position of the first atom in the northern row. This is done by iterating through the list of 
for the north row and checks various conditions to determine the position of the first atom. These conditions include 
whether the interaction is a reflected, absorbed, or path interaction, and the positions of the reflections and absorptions in the 
list of interactions for the western order-}
fstNorthAtom :: Interactions -> Interactions -> Pos
fstNorthAtom [] posWest = (0,0)
fstNorthAtom (n:posNorth) posWest 
    | snd n == Path (South,snd (fst n)) = fstNorthAtom posNorth posWest
    | isReflected n = (snd (fst n)+1,1)
    | isAbsorbed n && fstReflection posNorth == 2 && fstReflection posWest == 2 = (0,0)
    | isAbsorbed n && isAbsorbingPos (West,fstReflection posWest-1) posWest && not (isAbsorbingPos (West,fstReflection posWest + 1) 
    posWest) = (1,fstReflection posWest - 1)
    | isAbsorbed n = (1,fstReflection posWest +1)
    | snd n /= Path (South,snd(fst n)) && snd(fst n) == 1 && fstReflection posWest < fstAbsorbtion posWest && fstReflection
    posWest /= 0 =(snd(fst n)+1,fstReflection posWest)
    | snd n == Path (North, snd(fst n)) && snd(fst n) == 1 && fstReflection posWest > fstAbsorbtion posWest && fstReflection
    posWest /= 0 = (snd(fst n)+1,fstAbsorbtion posWest)
    | snd n /= Path (South,snd(fst n)) && snd(fst n) == 1 && fstReflection posWest == 0 = (snd(fst n)+1,fstAbsorbtion posWest)
    | snd n == Path (North, snd(fst n)) && snd(fst n) == 1 && fstReflection posWest == 0 = (snd(fst n)+1,fstAbsorbtion posWest)
    | otherwise = (snd(fst n)+1,hasReversePath n posWest+1)

{-finds the first atom in the south direction in the lattice, given a list of partial interactions (posSouth) and a list of partial interactions 
with atoms in the west direction (posWestRight).-}
fstSouthAtom :: Interactions -> Interactions -> Pos
fstSouthAtom [] posWestRight = (0,0)
fstSouthAtom (n:posSouth) posWestRight
    | snd n == Path(North, snd(fst n)) = fstSouthAtom posSouth posWestRight
    | isReflected n = (snd(fst n)+1, lastCoordinate (n:posSouth) 0)
    | isAbsorbed n && fstReflection posSouth == 2 && fstReflection posWestRight == lastCoordinate posWestRight 0-1 = (0,0)
    | isAbsorbed n && isAbsorbingPos (West,fstReflection posWestRight-1) posWestRight && not (isAbsorbingPos(West, fstReflection posWestRight + 1) 
    posWestRight) = (1,fstReflection posWestRight - 1)
    | isAbsorbed n = (1,fstReflection posWestRight -1)
    | snd n /= Path (North,snd(fst n)) && snd(fst n) == 1 && fstReflection posWestRight < fstAbsorbtion posWestRight && 
    fstReflection posWestRight /= 0 =(snd(fst n)+1,fstReflection posWestRight)
    | snd n /= Path (North, snd(fst n)) && snd(fst n) == 1 && fstReflection posWestRight > fstAbsorbtion posWestRight && 
    fstReflection posWestRight /= 0 = (snd(fst n)+1,fstAbsorbtion posWestRight)
    | snd n /= Path (North,snd(fst n)) && snd(fst n) == 1 && fstReflection posWestRight == 0 = (snd(fst n)+1,fstAbsorbtion posWestRight)
    | otherwise = (snd(fst n)+1,hasReversePath n posWestRight-1)

{-finds the first atom in the eastward direction in a lattice, given a list of partial interactions (posEast) and a list of partial interactions 
with atoms in the north direction (posNorthRight).-}
fstEastAtom :: Interactions -> Interactions -> Pos
fstEastAtom [] posNorthRight = (0,0)
fstEastAtom (n:posEast) posNorthRight
    | snd n == Path(West,snd(fst n)) = fstEastAtom posEast posNorthRight
    | isReflected n = (lastCoordinate posNorthRight 0, snd(fst n)+1)
    | isAbsorbed n && fstReflection posEast == 2 && fstReflection posNorthRight == lastCoordinate posNorthRight 0-1 = (0,0)
    | isAbsorbed n && isAbsorbingPos (North,fstReflection posNorthRight-1) posNorthRight && not (isAbsorbingPos(North, fstReflection posNorthRight + 1) 
    posNorthRight) = (fstReflection posNorthRight - 1, 1)
    | isAbsorbed n = (fstReflection posNorthRight - 1, 1)
    | snd n /= Path (West,snd(fst n)) && snd(fst n) == 1 && fstReflection posNorthRight > fstAbsorbtion posNorthRight && 
    fstReflection posNorthRight /= 0 = (fstReflection posNorthRight, snd(fst n)+1)
    | snd n /= Path (West, snd(fst n)) && snd(fst n) == 1 && fstReflection posNorthRight > fstAbsorbtion posNorthRight && 
    fstReflection posNorthRight /= 0 = (fstAbsorbtion posNorthRight,snd(fst n)+1)
    | snd n /= Path (West,snd(fst n)) && snd(fst n) == 1 && fstReflection posNorthRight == 0 = (fstAbsorbtion posNorthRight,snd(fst n)+1)
    | otherwise = (snd(fst n)+1,hasReversePath n posNorthRight+1)

{-accepts as input two interaction lists, "posWest" and "posNorth". It goes through the "posWest" list and checks for certain 
conditions to determine the position of the first atom in the west direction. These conditions include whether the interaction is a reflection, 
and whether there are atoms in the north direction that can affect the position of the atom in the west direction.-}
fstWestAtom :: Interactions -> Interactions -> Pos
fstWestAtom [] posNorth = (0,0)
fstWestAtom (n:posWest) posNorth
    | snd n == Path (East, snd(fst n)) = fstWestAtom posWest posNorth
    | isReflected n = (1, snd(fst n)+1)
    | isAbsorbed n && fstReflection posWest == 2 && fstReflection posNorth == 2 = (0,0)
    | isAbsorbed n && isAbsorbingPos (North, fstReflection posNorth-1) posNorth && not (isAbsorbingPos (North, fstReflection posNorth + 1)
     posNorth) = (fstReflection posNorth - 1, 1)
    | isAbsorbed n = (fstReflection posNorth + 1, 1)
    | snd n /= Path (East, snd(fst n)) && snd(fst n) == 1 && fstReflection posNorth < fstAbsorbtion posNorth && 
    fstReflection posNorth /= 0 = (fstReflection posNorth, snd(fst n)+1)
    | snd n /= Path (East, snd(fst n)) && snd(fst n) == 1 && fstReflection posNorth > fstAbsorbtion posNorth && 
    fstReflection posNorth /= 0 = (fstAbsorbtion posNorth, snd(fst n)+1)
    | snd n /= Path (East,snd(fst n)) && snd(fst n) == 1 && fstReflection posNorth == 0 = (snd(fst n)+1,fstAbsorbtion posNorth)
    | otherwise = (snd(fst n)+1,hasReversePath n posNorth+1)

{-similar to the "fstWestAtom" function, but used to find the position of the first atom in the north direction, given a list of 
north direction and a list of interactions in the east direction. It also checks for conditions such as reflection, 
absorption and path, and takes into account the presence of atoms in the east direction when determining the position of the atom in the north direction.-}
fstNorthAtomReverse :: Interactions -> Interactions -> Pos
fstNorthAtomReverse [] posEast = (0,0)
fstNorthAtomReverse (n:posNorthReverse) posEast
    | snd n == Path (South, snd(fst n)) = fstNorthAtomReverse posNorthReverse posEast
    | isReflected n = (snd(fst n)-1,1)
    | isAbsorbed n && fstReflection posNorthReverse == lastCoordinate posEast 0 - 1 && fstReflection posEast == 2 = (0,0)
    | isAbsorbed n && isAbsorbingPos (East, fstReflection posEast -1) posEast && not (isAbsorbingPos (East, fstReflection posEast + 1) 
    posEast) = (lastCoordinate posEast 0, fstReflection posEast - 1) 
    | isAbsorbed n = (lastCoordinate posEast 0,fstReflection posEast + 1)
    | snd n /= Path (South, snd(fst n)) && snd(fst n) == lastCoordinate posEast 0 && fstReflection posEast < fstAbsorbtion posEast && 
    fstReflection posEast /= 0 =(snd(fst n)-1, fstReflection posEast)
    | snd n /= Path (South, snd(fst n)) && snd(fst n) == lastCoordinate posEast 0 && fstReflection posEast > fstAbsorbtion posEast && 
    fstReflection posEast /= 0 = (snd(fst n)-1, fstAbsorbtion posEast)
    | snd n /= Path (South,snd(fst n)) && snd(fst n) == lastCoordinate posEast 0 && fstReflection posEast == 0 = (snd(fst n)-1, fstAbsorbtion posEast)
    | otherwise = (snd(fst n)-1,hasReversePath n posEast + 1)

{-finding the position of the first atom in the south and east directions, respectively, when going through the list of interactions in reverse order.
The functions take the list of interactions and the list of interactions in the opposite direction as input and return the position of the 
the first atom as an (x, y) tuple-}
fstSouthAtomReverse :: Interactions -> Interactions -> Pos
fstSouthAtomReverse [] posEastRight = (0,0)
fstSouthAtomReverse (n:posSouthReverse) posEastRight
    | snd n == Path(North,snd(fst n)) = fstSouthAtomReverse posSouthReverse posEastRight
    | isReflected n = (snd(fst n)-1, lastCoordinate posEastRight 0)
    | isAbsorbed n && fstReflection posSouthReverse == lastCoordinate posEastRight 0-1 && fstReflection posEastRight == lastCoordinate posEastRight 0 -1 = (0,0)
    | isAbsorbed n && isAbsorbingPos (East,fstReflection posEastRight+ 1) posEastRight && not(isAbsorbingPos(East, fstReflection posEastRight - 1) 
    posEastRight) = (lastCoordinate posEastRight 0, fstReflection posEastRight + 1)
    | isAbsorbed n = (lastCoordinate posEastRight 0,fstReflection posEastRight - 1)
    | snd n /= Path (North,snd(fst n)) && snd(fst n) == lastCoordinate posEastRight 0 && fstReflection posEastRight > fstAbsorbtion posEastRight && 
    fstReflection posEastRight /= 0 = (snd(fst n)-1,fstReflection posEastRight)
    | snd n /= Path (North, snd(fst n)) && snd(fst n) == lastCoordinate posEastRight 0 && fstReflection posEastRight > fstAbsorbtion posEastRight && 
    fstReflection posEastRight /= 0 = (snd(fst n)-1,fstAbsorbtion posEastRight)
    | snd n /= Path (North,snd(fst n)) && snd(fst n) == lastCoordinate posEastRight 0 && fstReflection posEastRight == 0 = (snd(fst n)-1,fstAbsorbtion posEastRight)
    | otherwise = (snd(fst n)-1,hasReversePath n posEastRight-1)

{-accepts two lists of interactions, posEastReverse and posSouthRight, and iterates through the list posEastReverse and 
performs various actions based on the value of the Marking field of the interactions in the list-}
fstEastAtomReverse :: Interactions -> Interactions -> Pos
fstEastAtomReverse [] posSouthRight = (0,0)
fstEastAtomReverse (n:posEastReverse) posSouthRight
    | snd n == Path(West, snd(fst n)) = fstEastAtomReverse posEastReverse posSouthRight
    | isReflected n = (lastCoordinate posSouthRight 0, snd(fst n)-1)
    | isAbsorbed n && fstReflection posEastReverse == lastCoordinate posSouthRight 0 -1 && fstReflection posSouthRight == lastCoordinate posSouthRight 0 -1 = (0,0)
    | isAbsorbed n && isAbsorbingPos (South, fstReflection posSouthRight +1) posSouthRight &&  not(isAbsorbingPos(South, fstReflection posSouthRight - 1) 
    posSouthRight) = (fstReflection posSouthRight + 1, lastCoordinate posSouthRight 0)
    | isAbsorbed n = (fstReflection posSouthRight - 1, lastCoordinate posSouthRight 0)
    | snd n /= Path(West, snd(fst n)) && snd(fst n) == lastCoordinate posSouthRight 0 && fstReflection posSouthRight > fstAbsorbtion posSouthRight && 
    fstReflection posSouthRight /= 0 = (fstReflection posSouthRight, snd(fst n)-1)
    | snd n /= Path (South, snd(fst n)) && snd(fst n) == lastCoordinate posSouthRight 0 && fstReflection posSouthRight > fstAbsorbtion posSouthRight && 
    fstReflection posSouthRight /= 0 = (fstAbsorbtion posSouthRight, snd(fst n)-1)
    | snd n /= Path (South,snd(fst n)) && snd(fst n) == lastCoordinate posSouthRight 0 && fstReflection posSouthRight == 0 = (fstAbsorbtion posSouthRight, snd(fst n)-1)
    | otherwise = (hasReversePath n posSouthRight-1, snd(fst n)-1)

{-finds the first atom located in the westward direction, searching for interactions in reverse order. He makes 
by iterating through the list of reverse order interactions and checking for certain conditions, such as whether 
the interaction is a reflection or absorption, and returns the position of the first atom found that satisfies these conditions-}
fstWestAtomReverse :: Interactions -> Interactions -> Pos
fstWestAtomReverse  [] posSouth = (0,0)
fstWestAtomReverse (n:posWestReverse) posSouth
    | snd n == Path(East, snd(fst n)) = fstWestAtomReverse posWestReverse posSouth
    | isReflected n = (1, snd(fst n)-1)
    | isAbsorbed n && fstReflection posWestReverse == lastCoordinate posSouth 0 -1 && fstReflection posSouth == 2 = (0,0)
    | isAbsorbed n && isAbsorbingPos (South, fstReflection posSouth -1) posSouth && not(isAbsorbingPos(South, fstReflection posSouth + 1) 
    posSouth) = (fstReflection posSouth - 1, lastCoordinate posSouth 0)
    | isAbsorbed n = (fstReflection posSouth + 1, lastCoordinate posSouth 0)
    | snd n /= Path(East,snd(fst n)) && snd(fst n) == lastCoordinate posSouth 0 && fstReflection posSouth < fstAbsorbtion posSouth && 
    fstReflection posSouth /= 0 = (fstReflection posSouth, snd(fst n)-1)
    | snd n /= Path (East, snd(fst n)) && snd(fst n) == lastCoordinate posSouth 0 && fstReflection posSouth > fstAbsorbtion posSouth && 
    fstReflection posSouth /= 0 = (fstAbsorbtion posSouth, snd(fst n)-1)
    | snd n /= Path (East,snd(fst n)) && snd(fst n) == lastCoordinate posSouth 0 && fstReflection posSouth == 0 = (fstAbsorbtion posSouth, snd(fst n)-1)
    | otherwise = (hasReversePath n posSouth+1, snd(fst n)-1)

{- similar structures, with several nested if operators that check various conditions and return the position of the 
in the south or east direction based on those conditions. These conditions include whether the interaction is 
"Path" interaction, whether it is reflected or absorbed, and whether there are other atoms in the West or North direction. 
that could affect the location of the first atom in the North, South, East or West direction.-}

{-removes duplicates from the list of atoms found in the interactions and sorts the atoms based on their first and second 
coordinates. This is done by using the functions rmdupAtoms and orderSndAtom to remove duplicates and order the atoms based on the 
and then uses the function orderFstAtom to order the atoms based on their first coordinates.-}
filterAtoms :: Interactions -> Atoms
filterAtoms partialInteractions = rmdupAtoms $ orderSndAtom $ rmdupAtoms $ orderFstAtom $ combineAtomSearches partialInteractions

{-finds the length of the board based on northward movements in the interactions. This is done using 
movementsNorth to find the northward movements, and then uses the lastCoordinate function to find the length of the 
the last coordinate in this list of movements-}
lengthOfBoard :: Interactions -> Int
lengthOfBoard partialInteractions = lastCoordinate (movementsNorth partialInteractions) 0

-- Solve a function that finds n number of atoms in a black box of interactions.
-- Find as many atoms as possible with auxiliary methods and brute force the rest.
solveBB :: Int -> Interactions -> Atoms 
solveBB _ _ = []

{-takes three parameters: an integer representing the size of the network, a list of interactions, and a list of atoms. It 
returns a list of atoms that are arranged according to the interactions provided in the input list. 
through the list of interactions and places the atoms at the specified positions. If the interaction is an "Absorb" interaction, 
it places the atom in the appropriate order (first or last) depending on the side specified in the interaction. 
If the interaction is "Path", it places the atom at the position indicated in the interaction as well as at the 
the starting position of the path-}
findAtoms :: Int -> Interactions -> Atoms -> Atoms
findAtoms size [] atoms = atoms
findAtoms size (((side, n), marking) : rest) atoms
    | marking == Absorb = findAtoms size rest ((n, getAbsorbRow side size) : atoms)
    | Path (side', n') <- marking = let ((_, row), _) = startEdgePosition size (side, n)
                                    in findAtoms size rest ((n, row) : atoms)
  where
--A helper function that returns the appropriate order for the Ingest interaction depending on the side specified in the interaction.
    getAbsorbRow :: Side -> Int -> Int
    getAbsorbRow North _ = 1
    getAbsorbRow South _ = size
    getAbsorbRow East size = size
    getAbsorbRow West _ = 1

-- Challenge 3
-- Pretty Printing Lambda with Alpha-Normalisation 
{-The following code defines a data type LamExpr for representing lambda expressions, 
and provides several functions for manipulating and printing these expressions-}
data LamExpr =  LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int 
                deriving (Eq, Show, Read)

{-function that accepts a list of strings (representing the bound variables in the lambda 
) and an integer index and returns a boolean pointer indicating whether the string 
"xindex" or "yindex" is present in the list of bound variables, but the string "\xindex" 
is not present. This function is used to check whether a bound variable name is available
to be used when renaming variables.-}
spareVariable  :: [String] -> Int -> Bool
spareVariable input index = 
    let 
        var = "x" ++ show index
        abstraction = "\\x" ++ show index
    in 
        notElem abstraction input && (elem var input || elem ("y" ++ show index) input)

{-This function takes a string and returns a new string with all instances of the character 'y' 
replaced with the character 'x'. This function is used to rename bound variables in a lambda expression-}
renameVar :: String -> String
renameVar = map replaceVarChar
  where replaceVarChar c = if c == 'y' then 'x' else c

alphaNormalize :: [String] -> Int -> [String]
alphaNormalize [] _ = []
alphaNormalize (x:xs) n
        |take 2 x == "\\x" = omitSpace [x] (drop 1 x) ("y"++ show n) ++
            alphaNormalize helper (findPos helper n)
        |otherwise =  x : alphaNormalize xs n
    where
        helper = omitSpace xs (drop 1 x) ("y"++ show n)
{- renameBoundVars :: Int -> LamExpr -> LamExpr
    renameBoundVars n (LamAbs m lamExpr) = LamAbs n (renameBoundVars n (renameVar m n lamExpr))
    renameBoundVars n (LamApp lamExpr1 lamExpr2) = LamApp (renameBoundVars n lamExpr1) (renameBoundVars n lamExpr2)
    renameBoundVars _ lamExpr = lamExpr-}

{-this function takes a list of strings, string n and string output and returns a new list of strings with 
all occurrences of n are replaced with output. This function is used to rename bound variables in a lambda expression.-}
omitSpace :: [String] -> String -> String -> [String]
omitSpace input n output = map newElem input
  where
    newElem elementOf
      | elementOf == n = output
      | drop 1 elementOf == n = "\\" ++ output
      | otherwise = elementOf

{-This function takes LamExpr and returns a list of strings representing the lambda expression in more 
structured format. This function is used to make it easier to manipulate and modify the lambda expression.-}
lambdaTransform :: LamExpr -> [String]
lambdaTransform (LamApp (LamVar e1) e2) =
  lambdaTransform (LamVar e1) ++ [""] ++ lambdaTransform e2
lambdaTransform (LamApp e1 e2) =
  ["("] ++ lambdaTransform e1 ++ [")"] ++ lambdaTransform e2
lambdaTransform (LamAbs i e) =
  ["\\x" ++ show i] ++ ["->"] ++ lambdaTransform e
lambdaTransform (LamVar i) = ["x" ++ show i]

{-This function takes a list of strings (representing a lambda expression) and an integer index and returns
the smallest integer n such that the string "xn" is not present in the list of bound variables and the string
"\xn" is not present. This function is used to find the next available bound variable name to use when renaming variables.-}
findPos :: [String] -> Int -> Int
findPos expression index = if spareVariable expression index
                            then findPos expression (index+1)
                            else index

{-This function takes a LamExpr and returns a string expression in "pretty" format,
with parentheses added if necessary to indicate the order of operations. This function is a combination of 
of several other functions in the code and is used to generate a more readable version of lambda 
expression that is in alpha-normal form.-}   
prettyPrint :: LamExpr -> String
prettyPrint expression = renameVar $ concat $ alphaNormalize (lambdaTransform expression) 0
{-
Additional test cases for the prettyPrint function:

(1) A simple lambda expression with one bound variable:
    assertEqual (prettyPrint (LamAbs 1 (LamVar 1))) "\\x0 -> x0"

(2) A lambda expression with multiple bound variables:
    assertEqual (prettyPrint (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2))))) "\\x0 -> \\x1 -> x0 x1"

(3) A lambda expression with a nested function application;
    assertEqual (prettyPrint (LamAbs 1 (LamApp (LamApp (LamVar 1) (LamAbs 2 (LamVar 2))) (LamAbs 3 (LamVar 3))))) "\\x0 -> (x0 (\\x1 -> x1)) (\\x2 -> x2)"

(4) A lambda expression with multiple bound variables that need to be renamed:
    assertEqual (prettyPrint (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1))))))) "\\x0 -> \\x1 -> x0 (\\x2 -> \\x3 -> x2)"

(5) A lambda expression with a function application containing a nested lambda expression:
    assertEqual (prettyPrint (LamApp (LamVar 1) (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamVar 3)))))) "x1 (\\x2 -> \\x3 -> x2 x3)"
-}

-- Challenge 4 
-- Parsing Arithmetic Expressions

data ArithExpr = Add ArithExpr ArithExpr | Mul ArithExpr ArithExpr 
               | Section ArithExpr  | SecApp ArithExpr ArithExpr | ArithNum Int
    deriving (Show,Eq,Read) 

{-
For the fourth exercise I took some ideas from:
Hu, A. (2019) Parsing simple arithmetic expressions in Haskell. based on 
https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.html,
Gist. 262588213843476. Available at: https://gist.github.com/andrewhu-uw/10b4
4dbe4bc5eb817ab3f0329cbb902a (Accessed: January 1, 2023). 
-}
{-
parser for arithmetic expressions. It first parses a primary expression using the primaryExpr parser and then searches 
for a '*' or '+' character followed by another arithmetic expression. If it finds any of these symbols,
it returns a value of the ArithExpr data type with the appropriate constructor (Mul or Add) applied to the symbol 
primary expression and the parsed arithmetic expression. If it does not find any of these symbols, it simply 
returns the result of parsing the primary expression.
-}
arithExpr :: Parsing.Parser ArithExpr
arithExpr = do   
    primary <- primaryExpr
    Parsing.space
    operation <- Parsing.char '*' Parsing.<|> Parsing.char '+'
    Parsing.space
    aritexpr <- arithExpr
    Parsing.space
    case operation of
        '*' -> return (Mul primary aritexpr)
        '+' -> return (Add primary aritexpr)
    Parsing.<|> primaryExpr

{-similar to arithExpr, but includes additional parsers for invalid cases, such as a number followed by a section 
(invalidNumSec) or an operator for a section followed by a number (invalidSecNum). These parsers use the unexpected 
to output an error if an invalid case is encountered.-}
--arithExpr' :: Text.Parsec.String.Parser ArithExpr
--arithExpr' = try secApp Text.Parsec.<|> try section Text.Parsec.<|> try mul Text.Parsec.<|> try add Text.Parsec.<|> try invalidNumSec Text.Parsec.<|> try invalidSecNum Text.Parsec.<|> try invalidExpr Text.Parsec.<|> num

{-
 parser for an arithmetic expression in the language defined by the ArithExpr data type. It uses a monad parser to parse the 
an expression that consists of a secApp followed by the + symbol and another primaryExpr. If this pattern is found, it 
returns an Add value constructed using the secApp and primary values. If this pattern is not found, it returns the value secApp
-}
primaryExpr :: Parsing.Parser ArithExpr
primaryExpr  = do 
    secapp <- secApp
    Parsing.space
    Parsing.char '+'
    Parsing.space
    primary <- primaryExpr
    Parsing.space
    return (Add secapp primary)
    Parsing.<|> secApp

{-parser that reads a string of digits and returns an ArithExpr value of type ArithNum with the value of the number 
represented by a string of digits-}
num :: Text.Parsec.String.Parser ArithExpr
num = do
  digits <- many1 Text.Parsec.digit
  pure $ ArithNum (read digits)

{-accepts an arithmetic expression string and returns the result of evaluating the expression as 
as an integer. If the expression is invalid, nothing is returned
evaluateArith :: String -> Maybe Int
evaluateArith str = 
  if isInvalidExpr str
    then Nothing
    else do
      expr <- parseArith str
      pure $ eval expr-}

{-recursively evaluates an ArithExpr value by performing the appropriate arithmetic operation depending on the type of 
expression. For example, if the expression is an Add expression, it evaluates the two subexpressions and returns 
their sum. If the expression is an ArithNum value, it simply returns the value of the number.-}
eval :: ArithExpr -> Int
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Section e) = eval (SecApp (Section e) (ArithNum 0))
eval (SecApp (Section e) e2) = eval e + eval e2
eval (ArithNum n) = n

{-The invalidNumSec and invalidSecNum functions are parsers that are designed to handle cases where a number is followed by 
by a "section" operator or vice versa. These cases are invalid according to the grammar of arithmetic expressions 
so the functions use the unexpected function of Parsec to raise an error indicating that the input cannot be parsed -}
invalidNumSec :: Text.Parsec.String.Parser ArithExpr
invalidNumSec = do
  n <- num
  spaces
  --sec <- Text.Parsec.try section
  Text.Parsec.unexpected "number followed by section"

{-
uses the Parser monad from the Parsing module to parse an arithmetic expression. It does this by first attempting to parse the 
a sequence of digits using the digits function, then tries to parse a section followed by another secApp, or finally tries to 
parse an expression enclosed in parentheses. If either of these parsing attempts is successful, the function returns the character 
the resulting value of ArithExpr. If all attempts are unsuccessful, the function returns a parser error.
-}
secApp :: Parsing.Parser ArithExpr
secApp = do 
    digits
  Parsing.<|> do
    sec <- section
    Parsing.space
    secapp <- secApp
    Parsing.space
    return (SecApp sec secapp)
  Parsing.<|> do
    Parsing.char '('
    Parsing.space
    aritexpr <- arithExpr
    Parsing.space
    Parsing.char ')'
    Parsing.space
    return aritexpr

{-
a parser that expects to find a number (processed using the num parser) after processing some optional whitespace characters. 
If this number is found, the parser throws an error with the message "section followed by number". This parser is probably 
used to detect and process invalid input that violates the rules of the parsed language.
-}
invalidSecNum :: Text.Parsec.String.Parser ArithExpr
invalidSecNum = do
 -- sec <- Text.Parsec.try section
  spaces
  n <- num
  Text.Parsec.unexpected "section followed by number"  

{-parser that expects to parse an arbitrary number of characters and then gives an "unexpected" error indicating that 
the input is an invalid expression. This function is probably used as a universal parser for invalid input-}
invalidExpr :: Text.Parsec.String.Parser ArithExpr
invalidExpr = Text.Parsec.many1 Text.Parsec.anyChar >> Text.Parsec.unexpected "invalid expression"

{-A parser that first parses an arithmetic expression using the arithExpr parser. It then uses all white fields and tries to 
parse a '+' or '*' symbol. If it succeeds in parsing one of these symbols, it parses another arithmetic 
expression using the primary parser. Finally, it checks whether the first and second arithmetic expressions are either 
numbers or "sections" (as defined by the isNum and isSection functions), and if they are, fails with an error message 
indicating that the expression is invalid. If the first and second arithmetic expressions are not numbers or sections, 
either an Add or Mul expression is returned, depending on the operator being parsed
invalid :: Text.Parsec.String.Parser ArithExpr
invalid = do
    e1 <- arithExpr
    spaces
    op <- try(Text.Parsec.char '+') Text.Parsec.<|> try(Text.Parsec.char '*')
    spaces
    e2 <- primary
    if (isNum e1 || isSection e1) && (isNum e2 || isSection e2) then fail "Invalid expression"
        else if op == '+' then return $ Add e1 e2 else return $ Mul e1 e2-}
    
{-accepts a string as input and checks whether it is a valid arithmetic expression. It does this by trying to parse the 
and then checks that the result is a Left value, indicating that parsing 
has failed. If parsing fails, True is returned, indicating that the input string is an invalid expression. If the parse is successful,
False is returned, indicating that the input string is a valid expression.
isInvalidExpr :: String -> Bool
isInvalidExpr str = isLeft (Text.Parsec.parse invalidNumSec "" str)-}

{-
parser for element Section in arithmetic expression. It reads the input string, looks for an opening parenthesis followed by a 
by a plus sign and then parses the arithmetic expression using the arithExpr parser. Finally, it looks for a closing parenthesis 
and returns an abstract syntactic tree node representing a Section element whose child is the parsed arithmetic expression.
-}
section :: Parsing.Parser ArithExpr
section = do 
    Parsing.char '('
    Parsing.space
    Parsing.char '+'
    Parsing.space
    exprr <- arithExpr
    Parsing.char ')'
    Parsing.space
    return (Section exprr)

{-parser that tries to parse a sequence of a number followed by a section, but then raises an error if it succeeds.-}
validNumSec :: Text.Parsec.String.Parser ArithExpr
validNumSec = do
  n <- num
  spaces
 -- sec <- Text.Parsec.try section
  return n    

{-parser for parsing an arithmetic expression in which the intersection is applied to one or more ArithNum values. The function 
first parses a section, then parses one or more digits, and finally returns an ArithExpr value that represents 
the application of the section to the ArithNum values represented by the parsed digits, using foldl SecApp. The function 
also consumes all the whitespace before and after parsing the section and digits-}
multiSecApp :: Parsing.Parser ArithExpr
multiSecApp = do    
    Parsing.space
    sec <- section
    Parsing.space
    x <- Parsing.many digits
    return $ foldl SecApp sec x

{-
parser for ArithExpr values. It uses the section parser to parse a value for a section, and the character parser to parse a '+' character, then 
parses an ArithExpr value using the arithExpr parser. If the operation is '+', it returns a SecApp value constructed from the parsed symbol 
value of section and value of ArithExpr. Otherwise, it returns a Mul value constructed from the parsed Section value and the ArithExpr value.
-}
highSecApp :: Parsing.Parser ArithExpr
highSecApp = do
    sec <- section
    Parsing.space
    operation <- Parsing.char '+'
    Parsing.space
    aritexpr <- arithExpr 
    if operation == '+' then return $ SecApp sec aritexpr else return $ Mul sec aritexpr 

{-helper function that parses a parser p enclosed in parentheses. It uses the initial character "(", then parses the 
and returns the result of the parsed parser p, then uses a closing character ")" and returns the result of the parsed parser-}
parens :: Text.Parsec.String.Parser a -> Text.Parsec.String.Parser a
parens p = do
  Text.Parsec.char '('
  x <- p
  Text.Parsec.char ')'
  return x

{-checks whether an ArithExpr is a valid expression according to the language rules. This is done by recursively 
checks each subexpression and returns True if all subexpressions are valid, and False otherwise-}
isValidExpr :: ArithExpr -> Bool
isValidExpr (Add e1 e2) = isValidExpr e1 && isValidExpr e2
isValidExpr (Mul e1 e2) = isValidExpr e1 && isValidExpr e2
isValidExpr (Section e) = isValidExpr e
isValidExpr (SecApp e1 e2) = isValidExpr e1 && isValidExpr e2
isValidExpr (ArithNum n) = True

--checks if an ArithExpr is a "section" operator. Returns True if the expression is a section constructor, and False otherwise.
isSection :: ArithExpr -> Bool
isSection (Section _) = True
isSection _ = False
{-
a parser that reads a sequence of one or more digits followed by some white boxes. It returns an ArithExpr value with 
ArithNum and an integer argument, which is the result of parsing the sequence of digits as an integer.
-}
digits :: Parsing.Parser ArithExpr
digits = do 
    digit <- some Parsing.digit
    Parsing.space
    return (ArithNum (read digit))

--checks whether a given ArithExpr is a number. Returns True if the expression is an ArithNum constructor, and False otherwise
isNum :: ArithExpr -> Bool
isNum (ArithNum _) = True
isNum _ = False

{-accepts a string as input and always returns Nothing. This function can be used as a placeholder when you need to return a 
Perhaps a value that represents a failed analysis, but you have no specific input to analyze.-}
parseInvalidExpr :: String -> Maybe ArithExpr
parseInvalidExpr _ = Nothing

{-accepts a string as input and attempts to parse it according to the rules of the concrete grammar specified in the BNF syntax. 
If parsing is successful, it returns an abstract syntax tree as a value of type ArithExpr data. If the parsing is 
is not successful, nothing is returned. This is done using the arithExpr parser and the Parsing.parse function. If the result 
of Parsing.parse is an empty list, nothing is returned. If the second head element of the Parsing.parse result 
is an empty string, the first element wrapped in the Just constructor is returned. Otherwise, nothing is returned.-}
parseArith :: String -> Maybe ArithExpr
parseArith expression
    | null (Parsing.parse arithExpr expression) = Nothing
    | snd (head(Parsing.parse arithExpr expression)) == "" = Just $ fst (head (Parsing.parse arithExpr expression))
    | otherwise = Nothing

{-Additional test cases for the parseArith function:
(1) parseArith "1 + 2 * 3" should return Just (Add (ArithNum 1) (Mul (ArithNum 2) (ArithNum 3))). This test case tests 
the precedence of the * operator over the + operator.

(2) parseArith "(+1) * 2" should return Nothing. This test case tests that the * operator cannot be applied to an operator section.

(3) parseArith "1 * (+2 + 3)" should return Just (Mul (ArithNum 1) (Add (Section (ArithNum 2)) (ArithNum 3))). 
This test case tests that operator sections can be used as operands of binary operators

(4) parseArith "(+1) (+2) (+3)" should return Just (SecApp (Section (ArithNum 1)) (SecApp (Section (ArithNum 2)) (Section (ArithNum 3)))).
This test case tests that multiple operator sections can be chained together to form a nested operator application

(5) parseArith "1 + 2 +" should return Nothing. This test case tests that the parser can handle incomplete expressions

(6) parseArith "(+1) (+2) (+3 (+4))" should return Just (SecApp (Section (ArithNum 1)) (SecApp (Section (ArithNum 2)) 
(SecApp (Section (ArithNum 3)) (Section (ArithNum 4))))). This test case tests that operator sections can be nested
within other operator sections

(7) parseArith "1 + (+2 + 3) + 4" should return Just (Add (ArithNum 1) (Add (Section (Add (ArithNum 2) (ArithNum 3)))
(ArithNum 4))). This test case tests that operator sections can be nested within other expressions
-}                     

-- Challenge 5
-- Church Encoding of arithmetic 
{-This implementation will work for any integer n that is not too large, since it is based on recursion and the number of 
recursive calls is limited by the maximum stack size. However, if n is too large, the recursion will not complete and the 
the function will fail with a stack overflow error.
To determine the Church numbers for larger integers, we will need to use another implementation that is not based on recursion. 
One way to do this is to use the Y combinator, which is a fixed point combinator and can be used to define recursive functions 
in the lambda calculus.
-}
{- there are several cases for different kinds of arithmetic expressions. The Const case converts a constant into a lambda expression 
that represents the corresponding Church number. The Plus and Mult cases use the definitions of "plus" and "mult" to translate the 
addition and multiplication expressions. The SecApp case deals with the application of a section operator, and the Section case 
case deals with the encoding of a section operator-}
churchEncInPlace :: ArithExpr -> LamExpr -> LamExpr
churchEncInPlace (ArithNum n) expr = LamAbs 0 (LamAbs 1 (churchEncInPlaceHelper n expr 0 1))
  where
    churchEncInPlaceHelper :: Int -> LamExpr -> Int -> Int -> LamExpr
    churchEncInPlaceHelper 0 _ _ _ = LamVar 1
    churchEncInPlaceHelper n expr f x = LamApp (LamVar f) (churchEncInPlaceHelper (n - 1) expr f x)
churchEncInPlace (Add e1 e2) expr = LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (churchEncInPlace e1 expr)) (LamApp (LamApp (LamVar 1) (churchEncInPlace e2 expr)) (LamVar 3))))))
churchEncInPlace (Mul e1 e2) expr = LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (churchEncInPlace e1 expr)) (LamApp (LamVar 1) (churchEncInPlace e2 expr))))))
churchEncInPlace (SecApp (Section op) e1) expr = LamApp (LamApp (churchEncInPlace op expr) (churchEncInPlace e1 expr)) (churchEnc (ArithNum 1))
churchEncInPlace (Section (ArithNum n)) expr = LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (churchEnc (ArithNum n))))
churchEncInPlace expr _ = error "Invalid input"

{-also has several cases for different types of arithmetic expressions. The ArithNum case converts a constant to a lambda 
expression that represents the corresponding Church number. The Plus and Mult cases use the definitions of "plus" and "mult" 
to convert addition and multiplication expressions. The SecApp case handles the application of the intersection operator, and the 
The Section case handles the encoding of a section operator-}
churchEnc :: ArithExpr -> LamExpr
churchEnc (ArithNum 0) = LamAbs 0 (LamAbs 1 (LamVar 1))
churchEnc (ArithNum n) = LamAbs 0 (LamAbs 1 (churchEncHelper (ArithNum (n - 1))))
  where
    churchEncHelper :: ArithExpr -> LamExpr
    churchEncHelper (ArithNum 0) = LamApp (LamVar 0)(LamVar 1)
    churchEncHelper (ArithNum n) = LamApp (LamVar 0)(churchEncHelper (ArithNum (n-1)))
churchEnc (Add e1 e2) = LamApp(LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2))(LamApp (LamApp (LamVar 1)(LamVar 2))(LamVar 3)))))))(churchEnc e1))(churchEnc e2)
churchEnc (Mul e1 e2) = LamApp (LamApp mult (churchEnc e1)) (churchEnc e2)
    where mult = LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)))))
churchEnc (SecApp (Section op) e1) = churchEnc (Add op e1)
churchEnc (Section op) = churchEnc op
--Y = λf -> (λx -> f (x x)) (λx -> f (x x))
{-helper function that takes an integer count and LamExpr as input and returns a new LamExpr that is the result of 
applying the original LamExpr to itself a number of times-}
makeLamApp :: Int -> LamExpr -> LamExpr
makeLamApp count ep
  | count == 0 = ep
  | otherwise = LamApp (LamAbs 1 (makeLamApp (count - 1) (LamVar 1))) ep

{-Additional test cases for the churchEnc function:

(1) Tests for basic arithmetic operations:

(1.1) Test case: Addition
assertEqual (churchEnc (SecApp (Section (ArithNum 1)) (ArithNum 2))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 2 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 2)) ( LamVar 3) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1)))))

(1.2) Test case: Subtraction
assertEqual (churchEnc (SecApp (Section (ArithNum 2)) (ArithNum 3))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 3 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 3)) ( LamVar 2) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1)))))

(1.3) Test case: Multiplication
assertEqual (churchEnc (SecApp (Section (ArithNum 3)) (ArithNum 4))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 2 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 2)) ( LamVar 3) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1)))))

(1.4) Test case: Division
assertEqual (churchEnc (SecApp (Section (ArithNum 4)) (ArithNum 5))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 3 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 3)) ( LamVar 2) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1)))))

(2) Tests for larger numbers:

(2.1) Test case: Addition with large numbers
assertEqual (churchEnc (SecApp (Section (ArithNum 1)) (ArithNum 12345))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 2 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 2)) ( LamVar 3) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( churchEncInPlaceHelper 12345 0 1))))-}

-- Challenge 6
-- Compare Innermost Reduction for Arithmetic and its Church Encoding

{-applying a strategy for innermost reduction of lambda expressions. It assumes a lambda expression
and performs a single reduction of that expression, if possible, returning the reduced expression.-}
innerRedn1 :: LamExpr -> Maybe LamExpr
--normalizes the input expression by calling the toNF' function on it.
innerRedn1 expr = 
{- calls the innerRednHelper function, passing the normalized expression and the boolean value False. 
The result of calling this function is a tuple containing the reduced expression and the boolean value 
value indicating whether or not the expression has been reduced.-}
    let normalizedExpr = toNF' expr
{-checks whether the expression is reduced or not by checking whether the boolean value returned by the 
innerRednHelper is true. If it is true, only the reduced expression is returned, otherwise Nothing is returned-}
        (reducedExpr, isReduced) = innerRednHelper normalizedExpr False
    in if isReduced then Just reducedExpr else Nothing

{-helper function for innerRedn1. It accepts a lambda expression and a boolean value as input and 
performs a single reduction of this expression, if possible, returning the reduced expression and 
a boolean value indicating whether or not the expression has been reduced.-}
innerRednHelper :: LamExpr -> Bool -> (LamExpr,Bool)
{-a specific case of a lambda expression that represents the add operation. It checks whether the input 
matches this pattern, if so, applies the add operation and returns the resulting 
lambda expression and the boolean value True-}
innerRednHelper (LamApp (LamApp (LamAbs a (LamAbs b (LamAbs c (LamAbs d (LamApp (LamVar a') (LamApp (LamApp (LamVar b') (LamVar c')) (LamVar d'))  ))))) expr1) expr2) op =
    (LamApp (LamApp (LamAbs a (LamAbs b (LamAbs c (LamApp (LamVar a') (LamApp (LamVar b') (LamVar c')))))) expr1 ) expr2,True)
--the second case is a variable case, it returns the input variable and the input boolean value.
innerRednHelper expr@(LamVar a) op = (expr,op)
{-lambda application case. If the input boolean value is True, the input expression is returned 
and the boolean value as they are. If not, it is checked whether the expression inside the lambda abstraction is in the 
normal form or not by calling the normalForm function, If it is in normal form, a replacement is performed 
of the variable inside the abstraction with the second argument of the application, if not, the 
is called recursively on the expression inside the abstraction.-}
innerRednHelper expr@(LamApp (LamAbs index expr1) expr2) op = 
    if op
        then (expr,True)
    else if normalForm expr1 
        then (lamExprReplacement index expr1 expr2, True) 
        else let value = innerRednHelper expr1 op in (LamApp (LamAbs index (fst value)) expr2 , snd value)
{-in the case of a lambda application, check whether the first argument is in normal form or not by calling 
normalForm, if it is in normal form, it is called recursively on the second argument, 
if it is not, it is called recursively on the first argument.-}
innerRednHelper (LamApp expr1 expr2) op = 
    if normalForm expr1 
        then let value = innerRednHelper expr2 op in (LamApp expr1 (fst value), op || snd value)
        else let value' = innerRednHelper expr1 op in (LamApp (fst value') expr2, op || snd value')
--a case of lambda abstraction. It is recursively called on the expression inside the abstraction
innerRednHelper (LamAbs a expr1) op = 
    let value = innerRednHelper expr1 op
    in (LamAbs a (fst value) , op || snd value)

{-checks whether a lambda expression is in normal form. A lambda expression is considered to be in 
normal form if it cannot be reduced further. This function checks whether the given expression is a variable, 
in which case it is in normal form. If the expression is an abstraction, it checks whether the body of the 
the abstraction is in normal form. If the expression is an application, it checks whether the left or 
right argument is an abstraction, in which case it is not in normal form. If neither argument is an abstraction, 
it is checked whether both the left and right arguments are in normal form. If they are, then the expression as 
is generally in normal form.-}
normalForm :: LamExpr -> Bool
normalForm (LamVar _) = True
normalForm (LamAbs _ expr) = normalForm expr
normalForm (LamApp expr1 expr2) = case (expr1, expr2) of
                            (LamAbs _ _, _) -> False
                            (_, _) -> normalForm expr1 && normalForm expr2

{- convert a lambda expression to an arithmetic expression. It does this by applying the rules of the lambda calculus 
to the given lambda expression and trying to convert the resulting expression to an arithmetic expression. If the 
conversion is not possible, it returns Nothing-}
arithEnc :: LamExpr -> Maybe ArithExpr
arithEnc (LamVar _) = Nothing
arithEnc (LamAbs _ _) = Nothing
arithEnc (LamApp (LamAbs _ _) e) = arithEnc e  -- reduce the application
arithEnc (LamApp e1 e2) =
  case arithEnc e1 of  -- try to convert the first argument to an arithmetic expression
    Just (ArithNum n) ->
      case arithEnc e2 of  -- try to convert the second argument to an arithmetic expression
        Just (ArithNum m) -> Just (Add (ArithNum n) (ArithNum m))  -- return the addition of the two numbers
        _ -> Nothing  -- the second argument could not be converted to an arithmetic expression
    Just (Mul (ArithNum n) e) ->  -- the first argument is a Church numeral representing a non-zero number
      case arithEnc e2 of  -- try to convert the second argument to an arithmetic expression
        Just (ArithNum m) -> Just (Mul (ArithNum n) (ArithNum m))  -- return the multiplication of the two numbers
        _ -> Nothing  -- the second argument could not be converted to an arithmetic expression
    Just (SecApp (ArithNum 1) e) ->  -- the first argument is a Church numeral representing the number 1
      case arithEnc e2 of  -- try to convert the second argument to an arithmetic expression
        Just arithExpr -> Just (SecApp (ArithNum 1) arithExpr)  -- return the secApp operation applied to the two converted arithmetic expressions
        _ -> Nothing  -- the second argument could not be converted to an arithmetic expression
    Just (SecApp (ArithNum 0) e) ->  -- the first argument is a Church numeral representing the number 0
      case arithEnc e2 of  -- try to convert the second argument to an arithmetic expression
        Just arithExpr -> Just (SecApp (ArithNum 0) arithExpr)  -- return the secApp operation applied to the two converted arithmetic expressions
        _ -> Nothing  -- the second argument could not be converted to an arithmetic expression
    _ -> Nothing  -- the first argument could not be converted to an arithmetic expression

{- replacing a variable of a lambda expression with another lambda expression. This is done by recursively 
traversing the input expression and replacing each occurrence of the variable with the given lambda expression.-}
lamExprReplacement :: Int -> LamExpr -> LamExpr -> LamExpr
lamExprReplacement index expr1 expr2 = go expr1
    where
        go (LamVar x) =
            if x == index
                then expr2
                else LamVar x
        go (LamApp expr1 expr2) = LamApp (go expr1) (go expr2)
        go (LamAbs a expr1) =
            if a == index
                then LamAbs a expr1
                else LamAbs a (go expr1)

{-given a lambda expression, it converts the expression to its normal form. This is done by recursively 
traversing the input expression and applying the normal form function on it. It also renames 
variable in the lambda abstraction to avoid variable interception.-}
toNF' :: LamExpr -> LamExpr
toNF' (LamAbs index expr) = LamAbs index' (rename index index' expr)
    where index' = newVar index expr
toNF' expr = expr

{-ttakes as input a variable index and a lambda expression and returns a new variable index that is not 
used in the given lambda expression. This is done by recursively calling the function with 
with an incremented index value until it finds an index that is not in the list of indexes used in the 
lambda expression returned by the cases function.-}
newVar :: Int -> LamExpr -> Int
newVar index expr = if elem index (cases expr) then newVar (index+1) expr else index

{-accepts an index, a new index and a lambda expression as input and returns a new lambda expression 
with all occurrences of the given index replaced by the new index. It recursively traverses 
the lambda expression, using a case statement to handle the different types of lambda expressions. 
For LamAbs, it checks whether the current index is the one we want to replace, if so, the new 
index, otherwise it uses the same index, and for LamApp and LamVar it simply renames the expression recursively-}
rename :: Int -> Int -> LamExpr -> LamExpr
rename index index' (LamAbs a b) = LamAbs (if index == a then index' else a) (rename index index' b)
rename index index' (LamApp expr1 expr2) = LamApp (rename index index' expr1) (rename index index' expr2)
rename index index' (LamVar a) = LamVar (if index == a then index' else a)

{-accepts a lambda expression as input and returns a new lambda expression that is in normal form. 
This is done by recursively traversing the lambda expression looking for LamAbs expressions. 
For each of these, it generates a new variable index using the newVar function, and replaces any 
of the original index with the new one using the rename function.-}
toNF :: LamExpr -> LamExpr
toNF (LamAbs index expr) = 
    let a = newVar index expr
    in LamAbs a (rename index a expr)
toNF expr = expr

{-converts a lambda expression to an arithmetic expression by evaluating the lambda expression with the given Church numbers.
This is done by recursively traversing the lambda expression and replacing the variables with the corresponding Church numbers-}
evalChurchEnc :: LamExpr -> (LamExpr, LamExpr) -> LamExpr
evalChurchEnc expr (f, x) = go expr
  where
    go (LamVar y)
      | y == 0 = x
      | y == 1 = f
      | otherwise = error "Invalid input"
    go (LamAbs y e) = LamAbs y (go e)
    go (LamApp e1 e2) = go e1 `LamApp` go e2
    
{-takes a lambda expression as input and returns a list of the indices of all variables used in the expression. 
It recursively traverses the lambda expression, using a case statement to process the different types 
lambda expressions. For LamAbs, it adds the current index to the list and continues recursively with the 
the rest of the expression. For LamApp, it returns the concatenation of the list with the indices used in the two expressions. 
For LamVar, it simply adds the index to the list -}
cases :: LamExpr -> [Int]
cases (LamAbs index expr) = index : cases expr
cases (LamApp expr1 expr2) = cases expr1 ++ cases expr2
cases (LamVar index) = [index]

{-accepts 3 arguments, two indices and a lambda expression, renaming the first index with the second. 
index in the input lambda expression. This is done by calling the rename function, which is defined in the 
the previous code-}
susbt :: Int -> Int -> LamExpr -> LamExpr
susbt index index' expr = rename index index' expr

{-Used to adjust bound variables in a lambda expression when performing a substitution to 
ensure that the bound variables do not capture any free variables in the substituted expression-}
shift :: LamExpr -> Int -> LamExpr
shift (LamVar y) d
  | y >= d = LamVar (y + d)
  | otherwise = LamVar y
shift (LamAbs y e1) d = LamAbs y' (shift e1 d')
  where
    y' = if y >= d then y + d else y
    d' = d + 1
shift (LamApp e1 e2) d = LamApp (shift e1 d) (shift e2 d)

freeVars :: LamExpr -> [Int]
freeVars (LamVar x) = [x]
freeVars (LamAbs x e) = filter (/= x) (freeVars e)
freeVars (LamApp e1 e2) = freeVars e1 ++ freeVars e2

{-takes an arithmetic expression and performs a one-time reduction of that expression, if possible, 
and returns the reduced expression. Uses the function innerArithRedn' to perform the reduction. If 
expression is reduced, it returns the reduced expression wrapped in Just, otherwise it returns Nothing-}
innerArithRedn1 :: ArithExpr -> Maybe ArithExpr
innerArithRedn1 expr = 
    case innerArithRedn' expr False of
        (reducedExpr, isReduced) 
            | isReduced -> Just reducedExpr
            | otherwise -> Nothing

{-accepts an arithmetic expression and a boolean value as arguments. If the passed boolean value is True, 
it returns the input expression and True as the output. If it is False, a reduction of the 
the input expression, if possible, and returns the reduced expression and a boolean value indicating whether the 
reduction was performed. This is done by matching the input expression against one of the cases of 
arithmetic language and performs the appropriate reduction.-}
innerArithRedn' :: ArithExpr -> Bool -> (ArithExpr, Bool)
innerArithRedn' (ArithNum a) b = (ArithNum a, b)
innerArithRedn' expr@(Add (ArithNum a) (ArithNum b)) op = 
    if op
        then (expr, True) 
        else (ArithNum (a+b), True)
innerArithRedn' expr@(Mul (ArithNum a) (ArithNum b)) op = 
    if op
        then (expr,True)
        else (ArithNum (a*b), True)
innerArithRedn' expr@(SecApp (Section (ArithNum a)) (ArithNum b)) op = 
    if op
        then (expr,True)
        else (ArithNum (a+b), True)
innerArithRedn' (Add expr1 expr2) op = 
    let value = innerArithRedn' expr1 op
        value' = innerArithRedn' expr2 (op || snd value)
    in (Add (fst value) (fst value'), op || snd value || snd value')
innerArithRedn' (Mul expr1 expr2) op = 
    let value = innerArithRedn' expr1 op
        value' = innerArithRedn' expr2 (op || snd value)
    in (Mul (fst value) (fst value'), op || snd value || snd value')
innerArithRedn' (SecApp (Section expr1) expr2) op = 
    let value = innerArithRedn' expr1 op
        value' = innerArithRedn' expr2 (op || snd value)
    in (SecApp (Section (fst value)) (fst value'), op || snd value || snd value')

{-takes an arithmetic expression and returns a tuple of two integers. The first integer is the number 
of the steps required to reduce the arithmetic expression using the innerArithRedn1 function, and the second 
is the number of steps required to reduce the lambda expression obtained by converting the function 
arithmetic expression into a lambda expression using the churchEnc function and the innerRedn1 function.-}
compareArithLam :: ArithExpr -> (Int, Int)
compareArithLam expr = (numberArithRed expr , numberLamRed (churchEnc expr))

{-takes an arithmetic expression and returns an integer which is the number of reductions required 
to reduce the arithmetic expression to its normal form using the innerArithRedn1 function. It uses 
helper function go, which accepts an arithmetic expression and the number of reductions. The function 
go uses recursion and pattern matching to reduce the arithmetic expression as much as possible, 
and increases the count for each reduction. The isNothing and fromJust functions from Data.Maybe 
are used to check whether the result of innerArithRedn1 is Nothing or Just a, respectively.-}
numberArithRed :: ArithExpr -> Int
numberArithRed expr = go expr 0
    where
        go :: ArithExpr -> Int -> Int
        go expr' count
            | isNothing value = count
            | otherwise = go (fromJust value) (count + 1)
            where value = innerArithRedn1 expr'

{-takes a lambda calculus expression and returns an integer, which is the number of reductions required 
to reduce the lambda calculus expression to its normal form using the innerRedn1 function. It uses 
helper function go, which accepts the number of reductions and the lambda calculus expression. 
The go function uses recursion and pattern matching to reduce the lambda calculus expression as much as possible 
and increases the count for each reduction. The isNothing and fromJust functions are used 
to check whether the result of innerRedn1 is Nothing or Just a, respectively.-}
numberLamRed :: LamExpr -> Int
numberLamRed expr = go 0 expr
    where
        go :: Int -> LamExpr -> Int
        go count expr
            | isNothing value = count
            | otherwise = go (count + 1) (fromJust value)
                where value = innerRedn1 expr

{-Additional test cases for the parseArith function:
(1) compareArithLam (ArithNum 0): This test case tests the behavior of the compareArithLam function for an arithmetic
expression that consists of a single number. The expected output is (0,0), as there are no subexpressions to reduce.

(2) compareArithLam (Mul (ArithNum 2) (ArithNum 3)): This test case tests the behavior of the compareArithLam function 
for an arithmetic expression that consists of a multiplication of two numbers. The expected output is (1,6), as there is 
a single subexpression to reduce in the arithmetic expression, and it takes 6 steps to reduce the corresponding lambda expression      

(3) compareArithLam (Add (Add (ArithNum 2) (ArithNum 3)) (ArithNum 4)): This test case tests the behavior of the 
compareArithLam function for an arithmetic expression that consists of a nested addition of three numbers. The 
expected output is (3,12), as there are three subexpressions to reduce in the arithmetic expression, and it takes 
12 steps to reduce the corresponding lambda expression

(4) compareArithLam (SecApp (Section (ArithNum 2)) (ArithNum 3)): This test case tests the behavior of the compareArithLam 
function for an arithmetic expression that consists of an application of a section. The expected output is (2,6), as 
it takes 2 steps to reduce the arithmetic expression and 6 steps to reduce the corresponding lambda expression

(5) compareArithLam (SecApp (Section (SecApp (Section (ArithNum 2)) (ArithNum 3))) (ArithNum 4)): This test case tests the 
behavior of the compareArithLam function for an arithmetic expression that consists of a nested application of sections.The expected
output is (3,8), as it takes 3 steps to reduce the arithmetic expression and 8 steps to reduce the corresponding lambda expression
      -}