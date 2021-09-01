module Ch5 where

import Prelude (Unit, (+), (-), (<), (>), (>=), (/=), (==), (<<<), show, discard, negate, otherwise, type (~>))

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

-- call a function with its supplied parameters flipped, the idiomatic way
flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- same thing, just with explicit parentheses, implying that we return a lambda function that flips parameters
flip' :: ∀ a b c. (a -> b -> c) -> (b -> a -> c)
flip' f = \x y -> f y x

-- takes two parameters and returns the first one, discarding the second, useful for const predicates
const :: ∀ a b. a -> b -> a
const x _ = x

-- takes a function and its parameter, and applies it
apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

-- operator for application, for nice (albeit backwards) chaining, right-associative, with 0 precedence as
-- this needs to be applied last after everything has been computed on the right of it
infixr 0 apply as $

-- takes a parameter and a function, applying the parameter to it
applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply -- x f = f x

-- operator for reverse application, useful for creating data manipulation pipelines that read nicely in English,
-- left-associative with 1 precedence as this needs to be applied last after everything has been computed
-- to the left of it, but before $ is applied, so they can be mixed and matched
infixl 1 applyFlipped as #

-- creates a List containing the parameter that is passed in
singleton :: ∀ a. a -> List a
singleton x = x : Nil -- Cons x Nil

-- tests whether the supplied list is empty
null :: ∀ a. List a -> Boolean
null Nil = true
null _   = false

-- creates a new list which has the supplied element added to the end
snoc :: ∀ a. List a -> a -> List a
snoc Nil x      = singleton x
snoc (y : ys) x = y : snoc ys x

-- calculates the length of a linked list, but not tail-recursive
-- this means the program has to save the return address of each recursive call, which can lead to stack-overflow
length :: ∀ a. List a -> Int
length Nil      = 0
length (_ : xs) = 1 + length xs

-- tail-recursive version of length, which first accumulates the running length, then calls itself
-- this means that the recursive call is the last operation in the function
-- which means there is no need to return from it, so the program can return to the original caller in one step
length' :: ∀ a. List a -> Int
length' l = go 0 l where
    -- for having a nice type signature, a private 'go' function is defined that implements the tail-recursive
    -- version of length, meaning that it hides the fact that it needs an extra parameter for it to work
    go :: Int -> List a -> Int
    go acc Nil      = acc
    go acc (_ : xs) = go (acc + 1) xs

-- gets the first element of a list, or Nothing if empty
head :: ∀ a. List a -> Maybe a
head Nil     = Nothing
head (x : _) = Just x

-- returns the whole list except the first element, or Nothing if empty
tail :: ∀ a. List a -> Maybe (List a)
tail Nil      = Nothing
tail (_ : xs) = Just xs

-- gets the last element of a list, or Nothing if empty
last :: ∀ a. List a -> Maybe a
last Nil       = Nothing
last (x : Nil) = Just x
last (_ : xs)  = last xs -- tail recursive, as there is only 1 arm with a recursive call, which is the only instruction

-- returns the whole list except the last element, or Nothing if empty
init :: ∀ a. List a -> Maybe (List a)
init Nil       = Nothing
init (_ : Nil) = Just Nil -- if the list is only 1 long, return Nil, as the last element is the only element
init l         = Just $ go l where -- private function to create the returned list
    go Nil       = Nil -- unnecessary, as `go` is never called with an empty list, but the compiler needs it
    go (_ : Nil) = Nil -- if the list has only 1 element left, return Nil as it needs to be excluded
    go (x : xs)  = x : go xs -- if the list is longer than 1, append the first element to the return of `go xs`

-- breaks down the list into `head` and `tail`, or Nothing if the list is emtpy
uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail: xs }

-- gets the element at the specified index, or Nothing if the index is out of bounds (or negative)
index :: ∀ a. List a -> Int -> Maybe a
index Nil _       = Nothing
index _ i | i < 0 = Nothing -- this is above the recursive call, so it never recurses on negative indices
index (x : _) 0   = Just x
index (_ : xs) i  = index xs (i - 1)

infixl 8 index as !!

-- finds the first element's index that satisfies the given predicate
findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex _ Nil  = Nothing -- return nothing when an empty list is passed
findIndex pred l = go 0 l where
    go _ Nil      = Nothing -- return nothing when the list is exhausted and the index is not found
    go i (x : xs) = if pred x then Just i else go (i + 1) xs

-- finds the last element's index that satisfies the given predicate
findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex _ Nil  = Nothing
findLastIndex pred l = go Nothing 0 l where -- store the found index and current index and iterate through the list
    go :: Maybe Int -> Int -> List a -> Maybe Int -- unnecessary type signature, as the compiler infers it
    go fi _ Nil      = fi -- return the last index or nothing where the predicate was true
    go fi i (x : xs) = go (if pred x then Just i else fi) (i + 1) xs -- tail recursive as only `go` is here

-- returns a reversed version of the list, the `go` function handles the lists as two stacks
reverse :: List ~> List -- equivalent to ∀ a. List a ->  List a, denotes a natural transformation, easy to type
reverse Nil = Nil
reverse ol = go Nil ol where -- function that takes the reversed list and original list as parameter
    go rl Nil = rl -- base case, when the original list is empty, so return the reversed list as it is complete
    go rl (x : xs) = go (x : rl) xs -- put the first element of the original list on the reversed list and continue

-- flattens a list of lists into a single list
concat :: ∀ a. List (List a) -> List a
concat Nil = Nil -- when the list of lists is empty, return nil
concat (Nil : xss) = concat xss -- when the first list in the list of lists is empty, continue concatting the tail
concat ((x : xs) : xss) = x : concat (xs : xss) -- pattern match the first item in the first list, append to the front
                                                -- while concatting the rest of the list without this element

-- creates a new list of the elements for which the supplied predicate returned true
filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter pred (x : xs) = if pred x then x : filter pred xs else filter pred xs -- if the predicate holds, append x

-- alternative implementation of filter using guards
filter' :: ∀ a. (a -> Boolean) -> List a -> List a
filter' _ Nil = Nil
filter' pred (x : xs) -- might be more verbose as a single `if` would be enough, and functional code is usually terse
    | pred x = x : filter pred xs
    | otherwise = filter pred xs

-- tail-recursive filter, where the `go` function gets the list under construction and appends elements
-- for which the predicate is true - this causes the new list to be backwards, so it must be reversed manually
-- this means that the list must be processed twice, trading time-complexity for space-complexity
-- this implementation uses a 'point-free notation' for `go`, as the parameter for the original list is omitted
-- and is passed in automatically by currying - for this, the `<<<` operator is required for composition
filterTailrec :: ∀ a. (a -> Boolean) -> List a -> List a
filterTailrec pred = reverse <<< go Nil where -- this version builds the list backwards, so it needs to be reversed
    go nl Nil = nl -- the second parameter here is implied, as it is not present in the top function call
    go nl (x : xs) = if pred x then go (x : nl) xs else go nl xs

-- filter a list of maybes to keep only those elements which are not nothing
catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil      = Nil
catMaybes (x : xs) = case x of
    Just y  -> y : catMaybes xs
    Nothing -> catMaybes xs

-- same thing with pattern matching
catMaybes' :: ∀ a. List (Maybe a) -> List a
catMaybes' Nil             = Nil
catMaybes' (Nothing : xs)  = catMaybes' xs
catMaybes' ((Just x) : xs) = x : catMaybes' xs

-- generates a list containing numbers between the two inputs, inclusive
range :: Int -> Int -> List Int
range start end
    | start == end = singleton start -- if both parameters are the same, create a singleton list with that number
    | otherwise = start : range (start + (if start < end then 1 else (-1))) end -- otherwise, add the start value
                                                                                -- and recurse with an incremented or
                                                                                -- decremented index depending on the
                                                                                -- direction

-- same thing but the step direction is calculated only once
range' :: Int -> Int -> List Int
range' start end = go start where
    go start' | start' == end = singleton start'
              | otherwise     = start' : go (start' + step)
    step = if start < end then 1 else (-1) -- step is defined in the `where` section so it is accessible in `go`

-- tail recursive version of range
rangeTailrec :: Int -> Int -> List Int
rangeTailrec start end = go Nil end start where -- for generating the list forwards, numbers are generated backwards
    go rl start' end' | start' == end' = start' : rl -- so it starts from the end and progresses to the front
                      | otherwise      = go (start' : rl) (start' + step) end'
    step = if start < end then (-1) else (1) -- for that, the step direction is reversed to match backwards creation

test :: Effect Unit
test = do
   log "flip:"
   log $ show $ flip const 1 2 -- `log (show (flip const 1 2))`
   flip const 1 2 # show # log -- calculates `flip const 1 2`, then converts it to String, then logs it to console
   log ""

   log "singleton:"
   log $ show $ singleton "xyz"
   log ""

   log "null:"
   log $ show $ null Nil
   log $ show $ null ("abc" : Nil)
   log ""

   log "snoc:"
   log $ show $ snoc (1 : 2 : Nil) 3
   log ""

   log "length:"
   log $ show $ length $ 1 : 2 : 3 : Nil
   log $ show $ length $ 1 : 2 : 3: Nil
   log ""

   log "head:"
   log $ show (head Nil :: Maybe Unit) -- can also be: `log $ show $ head (Nil :: List Unit)`
   log $ show $ head ("abc" : "123" : Nil)
   log ""

   log "tail:"
   log $ show $ tail (Nil :: List Unit)
   log $ show $ tail ("abc" : "123" : Nil)
   log ""

   log "last:"
   log $ show $ (last Nil :: Maybe Unit)
   log $ show $ last ("a" : "b" : "c" : Nil)
   log ""

   log "init:"
   log $ show $ init (Nil :: List Unit)
   log $ show $ init (1 : Nil)
   log $ show $ init (1 : 2 : Nil)
   log $ show $ init (1 : 2 : 3 : Nil)
   log ""

   log "uncons:"
   log $ show $ uncons (1 : 2 : 3 : Nil)
   log ""

   log "index:"
   log $ show $ index (1 : Nil) 4
   log $ show $ index (1 : 2 : 3 : Nil) 1
   log $ show $ index (Nil :: List Unit) 0
   log $ show $ index (1 : 2 : 3 : Nil) (-99)

   log $ show $ (1 : 2 : 3 : Nil) !! 1
   log ""

   log "findIndex:"
   log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
   log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
   log $ show $ findIndex (10 /= _) (Nil :: List Int)
   log ""

   log "findLastIndex:"
   log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
   log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
   log $ show $ findLastIndex (_ == 10) (11: 12 : Nil)
   log ""

   log "reverse"
   log $ show $ reverse (10 : 20 : 30 : Nil)
   log ""

   log "concat"
   log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
   log ""

   log "filter"
   log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
   log ""

   log "catMaybes"
   log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
   log $ show $ catMaybes' (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
   log ""

   log "range"
   log $ show $ range 1 10
   log $ show $ range 3 (-3)

   log $ show $ range' 1 10
   log $ show $ range' 3 (-3)

   log $ show $ rangeTailrec 1 10
   log $ show $ rangeTailrec 3 (-3)
   log ""
