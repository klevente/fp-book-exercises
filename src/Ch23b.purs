module Ch23b where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.State.Trans (StateT, runStateT)
import Data.Either (Either(..))
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Aff.Bus (BusRW)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)

randomAff' :: Aff Number
-- use `makeAff` to convert a function returning an `Effect` to a function returning an `Aff`
-- the parameter of this function is a callback that must be called once, with the computation's result or an error, if any
-- cb :: ∀ a. (Either Error a -> Effect Unit)
-- this function also returns an `Effect Canceler`, which means it will run in the `Effect` monad
randomAff' = makeAff \cb -> do
    n <- random -- get a random value, unwrapping it from `Effect`
    cb $ Right n -- as `random` cannot fail, call the callback with the correct result by wrapping it in `Right`
    pure nonCanceler -- as no cleanup is required, return a `Canceler` that does nothing

randomAff :: Aff Number
-- leverage `liftEffect`, which lifts the function from `Effect` to a destination monad, which is in this case, `Aff`
-- `liftEffect :: ∀ a. Effect a -> m a` => `Effect a -> Aff a`
randomAff = liftEffect random

type Config = { bus :: BusRW String } -- `Bus` which can be written and read, with `String` as its contained type
type State = { count :: Int } -- store how much random numbers to generate

type FiberM a = ReaderT Config (StateT State Aff) a -- the app's monad stack: ReaderT -> StateT -> Aff (since it will be run in the `Aff` monad

-- function for running the `FiberM` monad stack
runFiberM :: BusRW String -> FiberM Unit -> Aff (Tuple Unit State)
-- use `flip` to keep it mostly point-free, passing in the state and read-only values as the first params
runFiberM bus = flip runStateT { count: 10 } <<< flip runReaderT { bus }

test :: Effect Unit
test = do
    log "placeholder"
