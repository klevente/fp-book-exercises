module Ch19 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

data Maybe a = Nothing | Just a
derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
    show = genericShow

instance functorMaybe :: Functor Maybe where
    map f (Just x) = Just $ f x
    map _ Nothing  = Nothing

instance applyMaybe :: Apply Maybe where
    apply Nothing  _ = Nothing
    apply (Just f) x = f <$> x

instance applicativeMaybe :: Applicative Maybe where
    pure = Just

instance bindMaybe :: Bind Maybe where
    bind Nothing  _ = Nothing
    bind (Just x) f = f x

instance monadMaybe :: Monad Maybe

data Either a b = Left a | Right b
derive instance functorEither :: Functor (Either a)
derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
    show = genericShow

instance applyEither :: Apply (Either a) where
    apply (Left err) _ = Left err
    apply (Right f)  x = f <$> x

instance applicativeEither :: Applicative (Either a) where
    pure = Right

instance bindEither :: Bind (Either a) where
    bind (Left err) _ = Left err
    bind (Right x)  f = f x

instance monadEither :: Monad (Either a)

-- type that combines the data of `Reader`, `Writer` and `State`
type RWSResult r w s = { r :: r, w :: w, s :: s }

-- model `RWS` like `State`, which holds a function of type: `s -> Tuple a s`, by including the state of all 3 `Monad`s
-- `r`: read-only value, do not care about it when it is returned from any function as it can never change
-- `w`: write-only value, do not care about it when it is an input to a function as it cannot be read
-- `s`: read-write value, it depends on both input and output in a function
newtype RWS r w s a = RWS (RWSResult r w s -> Tuple a (RWSResult r w s))

instance functorRWS :: Functor (RWS r w s) where
    -- first, run `g` on the input `rws`, then destructure the result to apply `f` to `x` and re-wrap everything
    map f (RWS g) = RWS \rws -> g rws # \(Tuple x rws') -> Tuple (f x) rws'

instance applyRWS :: Monoid w => Apply (RWS r w s) where
    -- as `RWS` is a `Monad`, leverage it by using `ap` for the `apply` function
    apply = ap

-- `w` is a `Monoid` as it needs to use `mempty` for creating an empty log sink
instance applicativeRWS :: Monoid w => Applicative (RWS r w s) where
    -- here, as `w` is write-only, its value is discarded in the destructured parameter, and is initialised as a new empty value
    -- this will be required in any future functions where `w` is an input
    pure x = RWS \{r, s} -> Tuple x { r, w: mempty, s }

-- `w` is a `Semigroup` as it uses `<>` to combine the logs
instance bindRWS :: Monoid w => Bind (RWS r w s) where
    bind :: ∀ a b. (RWS r w s a) -> (a -> RWS r w s b) -> (RWS r w s b)
    -- execute the first `RWS` by calling `g rws`
    bind (RWS g) f = RWS \rws -> g rws
        -- destructure the result, then run the second `RWS` which is obtained by `f x`, use `runRWS` to get back a `Tuple` which this lambda must return
        # \(Tuple x rws'@{ w }) -> runRWS (f x) rws'
            -- destructure the result, then return almost the same `Tuple`, with the combined logs of `g` and `f x`)
            -- `rws''@{ w: w'}` means that the function can access `rws''` and its `Writer` part named `w'` at the same time
            -- `rws'' { w = w <> w'}` is called "record update syntax", that returns the original record, with the fields specified in `{}` changed to the expression inside,
            -- meaning that `w` is changed to contain the concatenation of the original `w` and `w'`
            # \(Tuple y rws''@{ w: w' }) -> Tuple y rws'' { w = w <> w' }

-- `w` is a `Monoid` as `applicativeRWS` requires `w` to be a `Monoid`
instance monadRWS :: Monoid w => Monad (RWS r w s)

runRWS :: ∀ r w s a. RWS r w s a -> (RWSResult r w s -> Tuple a (RWSResult r w s))
runRWS (RWS f) = f

-- push data from the pure computation to the monadic one, hence why `a` is Unit, as this function exists solely because of its side-effect (from `Writer`)
tell :: ∀ r w s. w -> RWS r w s Unit
-- do not care about `w` in the incoming `RWSResult`, as it is a write-only value, so it is overwritten by the parameter `w`
tell w = RWS \{ r, s } -> Tuple unit { r, w, s }

-- pull data from the monadic computation to the pure one, hence `a` is `r` (from `Reader`)
ask :: ∀ r w s. Monoid w => RWS r w s r
-- return `r` as the pure result, paying attention that the log needs to be emptied
ask = RWS \{ r, s } -> Tuple r { r, w: mempty, s }

-- pull data from the monadic computation to the pure one, hence `a` is `s` (from `State`)
get :: ∀ r w s. Monoid w => RWS r w s s
get = RWS \{ r, s } -> Tuple s { r, w: mempty, s }

-- push data from the pure computation to the monadic one, hance `a` is unit (from `State`)
put :: ∀ r w s. Monoid w => s -> RWS r w s Unit
put s = RWS \{ r } -> Tuple unit { r, w: mempty, s }

type Config = { debugModeOn :: Boolean } -- read-only info
type Counter = Int -- state

--              r/o        w/o        state  res
rwsTest :: RWS Config (Array String) Counter Unit
rwsTest = do
    tell ["test the log"]
    tell ["test the log2", "test the log3"]
    config <- ask
    tell ["the config is " <> show config]
    counter <- get
    tell ["old counter is " <> show counter]
    put $ counter + 1
    newCounter <- get
    tell ["new counter is " <> show newCounter]
    pure unit

test :: Effect Unit
test = do
    log "Applicative Maybe:"
    log $ show $ Just (_ * 10) <*> Just 20
    log $ show $ Just (_ * 10) <*> pure 20

    log "Monad Maybe:"
    log $ show $ Just 20 >>= pure <<< (_ * 10)
    log $ show do
        x <- Just 20
        let y = x * 10
        pure y
    log $ show $ Just 20 >>= const Nothing >>= \y -> Just $ y + 42
    log $ show do
        _ <- Just 20
        y <- Nothing
        pure $ y + 42

    log "Applicative Either:"
    log $ show $ Right (_ * 10) <*> (Right 20 :: Either Unit _)
    log $ show $ Right (_ * 10) <*> (pure 20 :: Either Unit _)

    log "Monad Either:"
    log $ show $ (Right 20 :: Either Unit _) >>= pure <<< (_ * 10)
    log $ show do
        x <- Right 20 :: Either Unit _
        let y = x * 10
        pure y
    log $ show $ Right 20 >>= const (Left "error") >>= \y -> Right $ y + 42
    log $ show do
        _ <- Right 20
        y <- Left "error"
        pure $ y + 42

    log "rwsTest:"
    log $ show $ runRWS rwsTest { r: { debugModeOn: true }, w: mempty, s: 0 }
