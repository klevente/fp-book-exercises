module Ch15 where

import Prelude

import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Int.Bits ((.&.))
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
