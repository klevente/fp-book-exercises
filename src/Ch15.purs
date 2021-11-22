module Ch15 where

import Prelude

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

test :: Effect Unit
test = do
    log "odd:"
    log $ show $ odd 0
    log $ show $ odd 1

    log "runPredicate:"
    log $ show $ runPredicate (Predicate odd) $ 10
    log $ show $ runPredicate (Predicate odd) $ 11
