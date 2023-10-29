-- 0.1
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
 deriving (Show, Eq)

data Suit = Heart | Diamond | Club | Spade
 deriving (Show, Eq)

data Card = Card Rank Suit
 deriving (Show, Eq)

-- 0.2
getCardValue :: Card -> Int
getCardValue (Card Two suit) = 2
getCardValue (Card Three suit) = 3
getCardValue (Card Four suit) = 4
getCardValue (Card Five suit) = 5
getCardValue (Card Six suit) = 6
getCardValue (Card Seven suit) = 7
getCardValue (Card Eight suit) = 8
getCardValue (Card Nine suit) = 9
getCardValue (Card Ace suit) = 11
-- Other values are automatically 10 because there is only Ten, Jack, Queen and King left
getCardValue (Card rank suit) = 10

-- 0.3
-- Define Hand as linked list
-- this is a complete nightmare
data Hand = Nil | Const Card Hand
 deriving (Show, Eq)

(<+>) :: Hand -> Hand -> Hand
(<+>) Nil hand = hand
(<+>) hand Nil = hand
(<+>) (Const card1 hand1) hand2 = Const card1 ((<+>) hand1 hand2)

example1 :: Hand
-- wow this language can't be read at all
example1 = Const (Card Two Heart) (Const (Card Three Club) Nil)

example2 :: Hand
example2 = Const (Card Ace Heart) (Const (Card Ace Club) Nil)

-- 1.1
-- isInHand :: Hand -> Card -> Bool
-- isInHand Nil card = False -- basically if the last hand is the empty hand then return false
-- isInHand (Const card1 hand2) card
    -- | card1 == card = True -- if the card is the currently observed, then return true
    -- | otherwise = isInHand hand2 card -- if not search for next card

-- returns a Hand with every card having one type
getAllFromType :: Suit -> Hand
-- I am ashamed
getAllFromType suit = Const (Card Two suit) (Const (Card Three suit) (Const (Card Four suit) (Const (Card Five suit) (Const (Card Six suit) (Const (Card Seven suit) (Const (Card Eight suit) (Const (Card Nine suit) (Const (Card Ten suit) (Const (Card Jack suit) (Const (Card Queen suit) (Const (Card King suit) (Const (Card Ace suit) Nil))))))))))))

fullDeck :: Hand
fullDeck = getAllFromType Heart <+> getAllFromType Diamond <+> getAllFromType Club <+> getAllFromType Spade

-- 1.2
numOfAces :: Hand -> Int
numOfAces Nil = 0
numOfAces (Const (Card rank suit) nextHand)
    | rank == Ace = 1 + numOfAces nextHand
    | otherwise = numOfAces nextHand

-- 1.3
getmaxValue :: Hand -> Int
getmaxValue Nil = 0
getmaxValue (Const card nextHand) = getCardValue card + getmaxValue nextHand

getminValue :: Hand -> Int
-- get the biggest possible value and then change 11 points to 1 point per ace
getminValue hand = getmaxValue hand - numOfAces hand * 11 + numOfAces hand

-- pick mininmal value when busting, else pick maximal value
getValue :: Hand -> Int
getValue hand
    | getmaxValue hand > 21 = getminValue hand
    | otherwise = getmaxValue hand

-- 2.1
-- ET -> empty tree, FT -> full tree
data SearchTree = ET | FT SearchTree Int SearchTree
 deriving (Show, Eq)

-- If a tree is empty then just set the value and append two empty trees
-- Ff not then decide and append to the left or right subtree
insert :: Int -> SearchTree -> SearchTree
-- this looks so cursed
insert num ET = FT ET num ET
insert num (FT left val right)
    | num < val = FT (insert num left) val right
    | otherwise = FT left val (insert num right)

-- represents (1(2)(3(4)6))
exTree1 :: SearchTree
exTree1 = FT (FT ET 1 ET) 2 (FT (FT ET 3 ET) 4 (FT ET 6 ET))

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
delete :: Int -> SearchTree -> SearchTree
-- I know there is a more efficiant implementation with O(log_2 n), but to spare me some time, I will use the easy way out with O(n)
delete num ET = ET
-- delete (FT left val right) num
    -- |
    