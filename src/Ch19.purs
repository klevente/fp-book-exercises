module Ch19 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
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
    apply Nothing _ = Nothing
    apply (Just f) x = f <$> x

instance applicativeMaybe :: Applicative Maybe where
    pure = Just

instance bindMaybe :: Bind Maybe where
    bind Nothing _ = Nothing
    bind (Just x) f = f x

instance monadMaybe :: Monad Maybe

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
