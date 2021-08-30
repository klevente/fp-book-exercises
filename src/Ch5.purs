module Ch5 where

import Prelude (Unit, (+), show, discard)

import Data.List (List(..), (:))
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
null _ = false

-- creates a new list which has the supplied element added to the end
snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x

-- calculates the length of a linked list
length :: ∀ a. List a -> Int
length Nil = 0
length (_ : xs) = 1 + length xs

test :: Effect Unit
test = do
   log $ show $ flip const 1 2 -- log (show (flip const 1 2))
   flip const 1 2 # show # log -- calculates flip const 1 2, then converts it to String, then logs it to console
   log $ show $ singleton "xyz"
   log $ show $ null Nil
   log $ show $ null ("abc" : Nil)
   log $ show $ snoc (1 : 2 : Nil) 3
   log $ show $ length $ 1 : 2 : 3 : Nil
