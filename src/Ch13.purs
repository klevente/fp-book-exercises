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

data Either a b = Left a | Right b
derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
    show = genericShow

-- correct implementation
instance functorEither :: Functor (Either a) where
    map _ (Left err) = Left err
    map f (Right x) = Right $ f x

-- while people might think this could also work, the issue with this is that the `left` on the left side
-- is of type `Either a b` while the right side expects `Either a c`, which cannot work, so extracting
-- the error and re-wrapping it in `Left` ensures that the second type parameter is correct in the return value
{-
instance functorEither :: Functor (Either a) where
    map f (Right x) = Right $ f x
    map _ left = left
-}

data Tuple a b = Tuple a b
derive instance genericTuple :: Generic (Tuple a b) _
instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
    show = genericShow

instance functorTuple :: Functor (Tuple a) where
    -- only map the second value inside the `Tuple` because this is a `Functor` not `Bifunctor`
    map f (Tuple x y) = Tuple x $ f y

test :: Effect Unit
test = do
    log "Maybe Functor:"
    log $ show $ (_ / 2) <$> Just 10
    log $ show $ (_ / 2) <$> Nothing

    log "Either Functor:"
    log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _) -- explicit type spec for the compiler to know what `Left` is
    log $ show $ (_ / 2) <$> Left "error reason"

    log "Tuple Functor:"
    log $ show $ (_ / 2) <$> Tuple 10 20
