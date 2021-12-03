module Ch23b where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class.Console (log)
import Effect.Random (random)

randomAff :: Aff Number
-- use `makeAff` to convert a function returning an `Effect` to a function returning an `Aff`
-- the parameter of this function is a callback that must be called once, with the computation's result or an error, if any
-- cb :: ∀ a. (Either Error a -> Effect Unit)
-- this function also returns an `Effect Canceler`, which means it will run in the `Effect` monad
randomAff = makeAff \cb -> do
    n <- random -- get a random value, unwrapping it from `Effect`
    cb $ Right n -- as `random` cannot fail, call the callback with the correct result by wrapping it in `Right`
    pure nonCanceler -- as no cleanup is required, return a `Canceler` that does nothing

test :: Effect Unit
test = do
    log "placeholder"
