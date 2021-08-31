module Ch5 where

import Prelude (Unit, (+), show, discard)

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
length' :: ∀a. List a -> Int
length' l = go 0 l where
    -- for having a nice type signature, a private 'go' function is defined that implements the tail-recursive
    -- version of length, meaning that it hides the fact that it needs an extra parameter for it to work
    go :: Int -> List a -> Int
    go acc Nil      = acc
    go acc (_ : xs) = go (acc + 1) xs

-- gets the first element of a list, or Nothing if empty
head :: ∀a. List a -> Maybe a
head Nil     = Nothing
head (x : _) = Just x

-- returns the whole list except the first element, or Nothing if empty
tail :: ∀a. List a -> Maybe (List a)
tail Nil      = Nothing
tail (_ : xs) = Just xs

-- gets the last element of a list, or Nothing if empty
last :: ∀a. List a -> Maybe a
last Nil       = Nothing
last (x : Nil) = Just x
last (_ : xs)  = last xs -- tail recursive, as there is only 1 arm with a recursive call, which is the only instruction

-- returns the whole list except the last element, or Nothing if empty
init :: ∀a. List a -> Maybe (List a)
init Nil       = Nothing
init (_ : Nil) = Just Nil -- if the list is only 1 long, return Nil, as the last element is the only element
init l         = Just $ go l where -- private function to create the returned list
    go Nil       = Nil -- unnecessary, as `go` is never called with an empty list, but the compiler needs it
    go (_ : Nil) = Nil -- if the list has only 1 element left, return Nil as it needs to be excluded
    go (x : xs)  = x : go xs -- if the list is longer than 1, append the first element to the return of `go xs`


test :: Effect Unit
test = do
   log $ show $ flip const 1 2 -- `log (show (flip const 1 2))`
   flip const 1 2 # show # log -- calculates `flip const 1 2`, then converts it to String, then logs it to console

   log $ show $ singleton "xyz"

   log $ show $ null Nil
   log $ show $ null ("abc" : Nil)

   log $ show $ snoc (1 : 2 : Nil) 3

   log $ show $ length $ 1 : 2 : 3 : Nil
   log $ show $ length $ 1 : 2 : 3: Nil

   log $ show (head Nil :: Maybe Unit) -- can also be: `log $ show $ head (Nil :: List Unit)`
   log $ show $ head ("abc" : "123" : Nil)

   log $ show $ tail (Nil :: List Unit)
   log $ show $ tail ("abc" : "123" : Nil)

   log $ show $ (last Nil :: Maybe Unit)
   log $ show $ last ("a" : "b" : "c" : Nil)

   log $ show $ init (Nil :: List Unit)
   log $ show $ init (1 : Nil)
   log $ show $ init (1 : 2 : Nil)
   log $ show $ init (1 : 2 : 3 : Nil)
