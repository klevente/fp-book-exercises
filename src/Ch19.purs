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
