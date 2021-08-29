module Ch5 where

import Prelude (Unit, show, discard)

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

test :: Effect Unit
test = do
   log $ show $ flip const 1 2 -- log (show (flip const 1 2))
   flip const 1 2 # show # log -- calculates flip const 1 2, then converts it to String, then logs it to console
