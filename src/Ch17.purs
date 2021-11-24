module Ch17 where

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
    -- if the value is present, execute the function and wrap the result in `Just`
    map f (Just x) = Just $ f x
    -- if there is no value available, return `Nothing`
    map _ Nothing  = Nothing
instance applyMaybe :: Apply Maybe where
    -- use `map` (`<$>`) for handling both cases where `x` is `Just y` and `Nothing`
    apply (Just f) x = f <$> x
    -- if there is no function, return `Nothing`
    apply Nothing _         = Nothing
instance applicativeMaybe :: Applicative Maybe where
    -- wrap the supplied value into the context, using `Just`; written in point-free, where the value is implied
    pure = Just

test :: Effect Unit
test = do
    log "Maybe Functor, Apply, Applicative:"
    log $ show $ (+) <$> Just 21 <*> Just 21
    log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)
    log $ show $ pure (+) <*> Just 17 <*> Just 25
