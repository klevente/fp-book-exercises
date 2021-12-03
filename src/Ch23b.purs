module Ch23b where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State.Trans (StateT, runStateT, get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler, launchAff_, forkAff, delay)
import Effect.Aff.Bus (BusRW)
import Effect.Aff.Bus as Bus
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
runFiberM :: BusRW String -> FiberM Unit -> Aff Unit
-- use `flip` to keep it mostly point-free, passing in the state and read-only values as the first params
runFiberM bus = void <<< forkAff <<< flip runStateT { count: 10 } <<< flip runReaderT { bus }

-- used natural transformation syntax as both are `Monad`s, hence `Functor`s
liftAffToFiberM :: Aff ~> FiberM
-- need to `lift` twice because of the stack
liftAffToFiberM = lift <<< lift

-- the `logger` `Fiber` runs in the app's monad stack, i.e. `FiberM`
logger :: FiberM Unit
logger = forever do -- signal that this function needs to be ran over and over, without a stack overflow
    { bus } <- ask -- get the `bus` from the `Config`
    s <- liftAffToFiberM $ Bus.read bus -- `lift` twice as `read` runs in the `Aff` monad but this function runs in the `ReaderT` monad as it is on the top
    log $ "Logger: " <> s -- as all monad transformers have a `MonadEffect` instance, `log` can be called from anywhere without `lift`ing


-- constant :: Aff Number
-- constant = pure 0.0

delayRandom :: Aff Number
-- throw out the first computation's result in favor of the second by using `*>`, so this returns `randomAff` after 1 second
delayRandom = delay (Milliseconds 1000.0) *> randomAff

-- `randomGenerator` takes in a `pred` which decides whether the generated number should be published or not, also running in `FiberM`
randomGenerator :: String -> (Number -> Boolean) -> FiberM Unit
randomGenerator valueType pred = do
    { count } <- get -- get the current `count`
    unless (count <= 0) do -- if the `count` is less than `0`, do nothing, if not, run the `do` block
        { bus } <- ask -- get the `bus`
        liftAffToFiberM do -- as both calls operate inside `Aff`, wrap them in a `do` block and call `lift` on that, which makes it clearer
            n <- delayRandom -- get a random number
            -- if `pred` is true for `n`, write a nice message to the `bus`, by `flip`ping the parameters, the `String` building part can go to the end
            when (pred n) $ flip Bus.write bus
                $ "Found a value that is " <> valueType <> " (" <> show n <> ")"
        -- `modify_` takes in a lambda that returns the new state based on the current one;
        -- this lambda here captures the current state using `_`, then updates the count using struct update syntax (`=` instead of `:`)
        -- it is the same as: `\s -> s { count = count - 1 }`, but more terse
        modify_ _ { count = count - 1 }
        randomGenerator valueType pred



test :: Effect Unit
test = launchAff_ do
         bus <- Bus.make
         let forkFiberM = runFiberM bus -- partially apply `runFiberM` as the `Bus` is the same for all `Fiber`s
         forkFiberM logger
         forkFiberM $ randomGenerator "greater than 0.5" (_ > 0.5)
         forkFiberM $ randomGenerator "less than 0.5" (_ < 0.5)
         forkFiberM $ randomGenerator "greater than 0.1" (_ > 0.1)
