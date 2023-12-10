-- A3

-- to show the result in the timeline, we need to derive Show for data and Present
-- it is not necessary to derive Show for Wish, since it is only called but never returned
-- (for a Nice child, a Wish becomes a Present, for a Naughty Child it becomes Nothing)
data Child = Naughty | Nice
    deriving Show

-- error 1: needs Constructor, a is just a typevariable and cannot be used to create an object
data Wish a = Wish a


-- error 2: Present needs one parameter, as it is defined having one
data Present a = Box a | WrappingPaper (Present a)
    deriving Show

-- error 3: datatype Present has just one parameter, and Functors are defined in such a way
-- every instance of a Functor must need one parameter, hence the syntax in this case
-- does not want a parameter for Present declared
instance Functor Present where
  fmap f (Box a)           = Box (f a)
  fmap f (WrappingPaper p) = WrappingPaper (fmap f p)

-- the non termination of the program arises here:
-- the code hard to read with those if then statements (prettier to use guards
-- in a case like this), so we changed it to a nice little three liner
-- the problem itself arises from the use of (==) within the instance definition itself
-- in this case it ends up calling itself over and over again, without ever evaluating the
-- meaning of == for a Child.
-- much better readable and leads to fully functional code
instance Eq Child where
    Naughty == Naughty  = True
    Nice == Nice        = True
    _ == _              = False

-- the reason why we get "[" in the terminal when we call the non-terminating version
-- of the program is the way Haskell evaluates functions.
-- because the infinite loop (described above in Eq Child) arises within santasLittleHelper
-- Haskell has already begun to built the list that is the return value of the santa function
-- that is why we get a "[" in the console eventhough the program doesn't terminate.

wrap :: Int -> a -> Present a
wrap 0 x = Box x
-- error 4: WrappingPaper constructor expects to contain a Present a
-- wrap produces a Box x, if everything is packed up nicely for the kids, otherwise
-- it calls itself recursively, producing WrappingPaper and finally wrapped Boxes
-- what this means is, that WrapperPaper must contain the result of wrap as a whole, 
-- while wrap must be called without the use of commata
wrap n x = WrappingPaper (wrap (n-1) x)

-- error 5: lambda function not necessary (not the error, but a correction to improve the
-- code's readability); wrong order of elements in line 41, function should return Int (n+1)
-- and then the list
santa :: [(Child, Wish a)] -> [(Child, Maybe (Present a))]
santa xs = snd (foldr santasLittleHelper (0, []) xs)
  where santasLittleHelper (c, Wish x) (n, cs) = let c' = if c == Naughty then (c, Nothing) else (c, Just (wrap n x))
                                                 in (n + 1, c':cs)