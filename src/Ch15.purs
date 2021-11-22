module Ch15 where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Int.Bits ((.&.))
import Data.List (List(..), (:))
import Data.Profunctor (class Profunctor)
import Effect (Effect)
import Effect.Console (log)

even :: Int -> Boolean
even x = x .&. 1 == 0

odd :: Int -> Boolean
odd x = x .&. 1 == 1

data Predicate a = Predicate (a -> Boolean)

runPredicate :: ∀ a. Predicate a -> a -> Boolean
runPredicate (Predicate f) = f

-- `Contravariant` maps the supplied function `f` over the `Predicate`,
-- augmenting the input value before the `Predicate`'s function `g` is executed
-- the resulting function is then re-wrapped inside a `Predicate`
instance contravariantPredicate :: Contravariant Predicate where
    cmap f (Predicate g) = Predicate $ g <<< f

data Moore s a b = Moore s (s -> b) (s -> a -> s)

instance profunctorMoore :: Profunctor (Moore s) where
    -- explicit function header added to see the types of the parameters
    dimap :: ∀ a b c d. (c -> a) -> (b -> d) -> Moore s a b -> Moore s c d
    -- here to access the second variable of the `transition` function, the first one had to be extracted
    -- then put back into the mapped function by utilising a lambda, very useful technique!
    dimap f g (Moore s0 output transition) = Moore s0 (g <<< output) (\s -> transition s <<< f)

-- function creating a `Moore` instance that adds together `Semiring`s
addr :: ∀ a. Semiring a => Moore a a a
addr = Moore zero identity (+)

runFoldL :: ∀ s a b f. Foldable f => Moore s a b -> f a -> b
-- use `foldl` with the `Moore`s `transition`, then map the result using `output` to its final form
-- this version is point-free, as the foldable parameter (`xs`) is implied
-- this is why `<<<` is used instead of `$`, as the functions can only be composed because they are shy 1 parameter
runFoldL (Moore s0 output transition) = output <<< foldl transition s0

test :: Effect Unit
test = do
    log "odd:"
    log $ show $ odd 0
    log $ show $ odd 1

    log "runPredicate:"
    log $ show $ runPredicate (Predicate odd) $ 10
    log $ show $ runPredicate (Predicate odd) $ 11

    log "Contravariant Predicate:"
    log $ show $ runPredicate (cmap (_ + 1) (Predicate odd)) 10
    log $ show $ runPredicate (cmap (_ + 2) (Predicate odd)) 10
    log $ show $ runPredicate ((_ + 1) >$< (Predicate odd)) 10
    log $ show $ runPredicate ((_ + 2) >$< (Predicate odd)) 10

    log $ "runFoldL:"
    log $ show $ runFoldL addr [1, 2, 3]
    log $ show $ runFoldL addr (1.0 : 2.0 : 3.0 : Nil)
