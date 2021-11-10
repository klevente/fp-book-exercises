module Ch13 where

import Prelude (class Show, Unit, show, discard, ($), (/))

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

data Maybe a = Nothing | Just a
derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
    show = genericShow

class Functor f where
    map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

instance functorMaybe :: Functor Maybe where
    -- cannot do anything meaningful with `Nothing`, so just return it
    map _ Nothing  = Nothing
    -- apply the function to the inner value and return it wrapped in `Just`
    map f (Just x) = Just $ f x

test :: Effect Unit
test = do
    log "Maybe Functor:"
    log $ show $ (_ / 2) <$> Just 10
    log $ show $ (_ / 2) <$> Nothing
