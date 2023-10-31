-- 0.1
data Rank = Numeric Int | Jack | Queen | King | Ace
 deriving (Show, Eq)

data Suit = Hearts | Diamonds | Clubs | Spades
 deriving (Show, Eq)

data Card = Card Suit Rank
 deriving (Show, Eq)

-- 0.2
getCardValue :: Card -> Int
getCardValue (Card suit (Numeric number))
    | 2 <= number && number <= 10 = number
    | otherwise = undefined
getCardValue (Card suit Ace) = 11
getCardValue (Card suit rank) = 10

-- 0.3
-- Define Hand as a list (note: type is an alias like typedef in C)
type Hand = [Card]

(<+>) :: Hand -> Hand -> Hand
(<+>) hand1 hand2 = hand1 ++ hand2

-- example1 :: Hand
-- wow this language can't be read at all
-- example1 = Const (Card (Numeric 2) Heart) (Const (Card (Numeric 3) Club) Nil)

-- example2 :: Hand
-- example2 = Const (Card Ace Heart) (Const (Card (Numeric 10) Club) Nil)

-- TASK 1 IS READY TO BE SERVED

-- 1.1
-- isInHand :: Hand -> Card -> Bool
-- isInHand Nil card = False -- basically if the last hand is the empty hand then return false
-- isInHand (Const card1 hand2) card
    -- | card1 == card = True -- if the card is the currently observed, then return true
    -- | otherwise = isInHand hand2 card -- if not search for next card

-- returns a Hand with every card having one type
getAllFromType :: Suit -> Hand
-- Hand with literate types and "manually appended" all numeric cards.
getAllFromType suit = Card suit Jack : (Card suit Queen : (Card suit King : (Card suit Ace : helper 10)))
    where
        -- Should recursively iterate from number 10 to 2
        helper :: Int -> Hand
        helper 1 = []
        helper num = Card suit (Numeric num) : helper (num-1)

fullDeck :: Hand
fullDeck = getAllFromType Hearts <+> getAllFromType Diamonds <+> getAllFromType Clubs <+> getAllFromType Spades

-- 1.2
numOfAces :: Hand -> Int
numOfAces [] = 0
numOfAces ((Card suit rank) : nextHand)
    | rank == Ace = 1 + numOfAces nextHand
    | otherwise = numOfAces nextHand

-- 1.3
getmaxValue :: Hand -> Int
getmaxValue [] = 0
getmaxValue (card : nextHand) = getCardValue card + getmaxValue nextHand

getminValue :: Hand -> Int
-- get the biggest possible value and then change 11 points to 1 point per ace
getminValue hand = getmaxValue hand - numOfAces hand * 11 + numOfAces hand

-- pick mininmal value when busting, else pick maximal value
getValue :: Hand -> Int
getValue hand
    | getmaxValue hand > 21 = getminValue hand
    | otherwise = getmaxValue hand

-- TASK 2 IS READY TO BE SERVED
-- 2.1
-- ET -> empty tree, FT -> full tree
data SearchTree = ET | FT SearchTree Int SearchTree
 deriving (Show, Eq)

-- TODO: UNCOMMENT
-- If a tree is empty then just set the value and append two empty trees
-- Ff not then decide and append to the left or right subtree
-- insert :: Int -> SearchTree -> SearchTree
-- this looks so cursed
-- insert num ET = FT ET num ET
-- insert num (FT left val right)
    -- | num < val = FT (insert num left) val right
    -- | otherwise = FT left val (insert num right)

-- 2.2
-- The empty tree should always return False since there is nothing in it
-- If found return True
-- If not found then search left and then (that's how at least || works in other languages I know) right
isElem :: Int -> SearchTree -> Bool
isElem num ET = False
isElem num (FT left val right)
    | val == num = True
    | otherwise = isElem num left || isElem num right

-- 2.3
-- calculates the minimum of a tree
-- aka the most left node
minT :: SearchTree -> Int
-- if empty then return undefined thank you prechecker for at least giving me warnings. I wish I could display them in vscode somehow
minT ET = undefined
-- if left is empty then return val
minT (FT ET val right) = val
-- if not search left
minT (FT left val right) = minT left

-- removes minimum of a tree
deleteMin :: SearchTree -> SearchTree
-- same issue
deleteMin ET = ET
deleteMin (FT ET val right) = right
deleteMin (FT (FT leftleft leftval leftright) val right)
    -- if the value of the left subtree is the minimum, then delete the branch
    | leftval == minT (FT (FT leftleft leftval leftright) val right) = FT ET val right
    -- else search left
    | otherwise = FT (deleteMin (FT leftleft leftval leftright)) val right

-- deletes the root of a (sub-) tree
deleteRoot :: SearchTree -> SearchTree
-- same issue
deleteRoot ET = ET
deleteRoot (FT ET val right) = right
deleteRoot (FT left val ET) = left
deleteRoot (FT left val right) = FT left (minT right) (deleteMin right)

-- uncomment
{-
delete :: Int -> SearchTree -> SearchTree
delete num ET = ET
delete num (FT left val right)
    | val == num = deleteRoot (FT left val right)
    | val > num = FT (delete num left) val right
    | val < num = FT left val (delete num right)
    | otherwise = FT left val right -- Return the normal tree if something went wrong (which won't)
-}

-- TASK 3 IS READY TO BE SERVED
-- 3.0
-- same definition as in the searchtree
-- only difference is polymorphism
-- wow these tasks were a lot easier
-- I propably made the last task way more harder bc I didn't use lists
data Tree a = E | F (Tree a) a (Tree a)
 deriving (Show, Eq)

-- 3.1
sumTree :: Tree Int -> Int
sumTree E = 0
sumTree (F a num b) = sumTree a + num + sumTree b

-- 3.2
mirrorTree :: Tree a -> Tree a
mirrorTree E = E
-- (a, val, b) maps to (b, val a)
mirrorTree (F a val b) = F (mirrorTree b) val (mirrorTree a)

-- 3.3.1
toList :: Tree a -> [a]
toList E = []
toList (F a val b) = toList a ++ [val] ++ toList b

-- 3.3.2
-- the implementation has a runtime of O(n),
-- because the recursion will break down to the case that the (sub-) tree is empty
-- every node gets exactly one call of this method and ++ has on average constant runtime (if it's implemented like it should be)

-- 4.1.1.1
reverse1 :: [a] -> [a]
-- IB : reverse of empty is empty
reverse1 [] = []
-- IS : [a, ...] should map to [reverse ... a]
reverse1 (a : lst) = reverse1 lst ++ [a]

-- 4.1.1.2
-- the runtime is bigger than O(n) maybe O(n) * log n, but there seem to be some differences in time in each execution

-- 4.1.2
-- by temporary storing the currently calculated result,
-- we can iteratively change it while removing elements of the list that contains the elements,
-- that should be stacked in front of the temporary result
-- by doing that, we only have to use the memory that the list itself would need.
-- The big chain of recursive recalls does not have to be stored, nor accessed
-- This results in an optimal runtime of O(n)
reverse2 :: [a] -> [a]
reverse2 a = fst (revAkk [] a)
    where
        -- maps (temp), (todo) -> (nextTemp, nextTodo)
        revAkk :: [a] -> [a] -> ([a], [a])
        revAkk temp [] = (temp, [])
        revAkk temp (a2 : restTodo) = revAkk (a2 : temp) restTodo

-- 4.2

-- Had to move it out from indexOf bc the compiler thought that the inner and outer method types could be different
recSearch :: Eq a => a -> [a] -> Int -> Maybe Int
recSearch key [] num = Nothing
recSearch key (currElem : restList) num
    | key == currElem = Just num
    | otherwise = recSearch key restList (num + 1)

-- uses recursive iteration with recSearch
indexOf :: Eq a => a -> [a] -> Maybe Int
indexOf key [] = Nothing
indexOf key list = recSearch key list 0

-- 4.3
-- Straightforward
tails :: [a] -> [[a]]
tails [] = [[]]
tails (currElem : restList) = (currElem : restList) : tails restList

-- Here you need a bit of magic
-- The idea is that you first reverse the list and scan from the last elements similar to tails but write them the other way around.
inits :: [a] -> [[a]]
inits [] = [[]]
inits (currElem : restList) = helper (reverse2 (currElem : restList))
    where
        helper :: [a] -> [[a]]
        helper [] = [[]]
        helper (currElem2 : restList2) = helper restList2 ++ [restList2 ++ [currElem2]]

-- 4.4
-- inserts an element into a defined position in a list
insertInAt :: a -> [a] -> Int -> [a]
insertInAt element list 0 = element : list
-- good luck reading that
insertInAt element (current : nextList) index = current : insertInAt element nextList (index - 1)
insertInAt element list index = undefined

-- iterates between all indexes in a list and executes insertInAt for each of them
insert :: a -> [a] -> [[a]]
insert val list = recu val list 0
    where
        recu :: a -> [a] -> Int -> [[a]]
        recu elem2 [] num = []
        recu elem2 list2 num
            -- you need to stop one value after the length since the result will have |M| + 1 insertions.
            | num == (length list2 + 1) = []
            | otherwise = insertInAt elem2 list2 num : recu elem2 list2 (num + 1)

{-
-- 4.5
perms :: [a] -> [[a]]
perms [] = [[]]
perms (elem: (nextelem : nextlist)) = insert elem (nextelem : nextlist) ++ appendToEach nextelem (perms (nextlist ++ [elem]))
-- Case there is no second next element
perms (elem: list) = insert elem list
-}

-- generate tuple:
-- start have len(alphabet) words ✔

-- for each element in alphabet:
-- add element to tuple ✔
-- copy all tuples len(alphabet) ✔
-- repeat

startTuple :: [a] -> [[a]]
startTuple [] = [[]]
startTuple [elem] = [[elem]]
startTuple (elem : nextList) = [elem] : startTuple nextList

appendToEach :: a -> [[a]] -> [[a]]
appendToEach something [[]] = [[something]]
appendToEach something [] = []
appendToEach something [[elem]] = [elem : [something]]
appendToEach something (currentList : restListList) = (currentList ++ [something]) : appendToEach something restListList

copyTuples :: [[a]] -> Int -> [[a]]
copyTuples [[]] _ = [[]]
copyTuples _ 0 = [[]]
copyTuples [elem] 1 = [elem]
copyTuples [elem] n = elem : copyTuples [elem] (n-1)
-- wow double induction
copyTuples (currentList : nextListofLists) n = copyTuples [currentList] n ++ copyTuples nextListofLists n

-- temp, elem, length, output
calculateNextStep :: [[a]] -> a -> Int -> [[a]]
-- first append the element and then multiply the amount of elements by the amount of next possible keys (which is always len(alphabet))
calculateNextStep list elem num = copyTuples (appendToEach elem list) num

-- input list, length of that, counter, temp, output
recTuple :: [a] -> Int -> Int -> [[a]] -> [[a]]
recTuple (elem : restList) len counter temp
    | len == counter = temp
    | otherwise = calculateNextStep (recTuple (restList) len (counter+1) temp) elem len

tuple :: [a] -> [[a]]
tuple [] = [[]]
tuple (elem : nextList)
    | null nextList = []
    | otherwise = [[]]

{-
-- 
removeFirstOccurance :: Eq a => a -> [a] -> [a]
removeFirstOccurance _ [] = []
removeFirstOccurance key (elem: list)
    | key == elem = [elem]
    | otherwise = elem : removeFirstOccurance elem list

-- we have to assume a has operator Eq
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [ i:j | i <- xs, j <- perms $ removeFirstOccurance i xs ]
-}

-- subsets :: [a] -> [[a]]
-- subsets [] = [[]]
-- subsets (elem: list) = insert elem list ++ subsets list