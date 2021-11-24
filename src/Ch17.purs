module Ch17 where

import Prelude

import Data.Bifunctor (class Bifunctor)
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

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
    show = genericShow
derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance functorEither :: Functor (Either a) -- here the compiler generates the implementation!
instance bifunctorEither :: Bifunctor Either where
    bimap f _ (Left x) = Left $ f x
    bimap _ g (Right y) = Right $ g y
instance applyEither :: Apply (Either a) where
    apply (Right f) x  = f <$> x
    apply (Left err) _ = Left err
instance applicativeEither :: Applicative (Either a) where
    pure = Right

test :: Effect Unit
test = do
    log "Maybe Functor, Apply, Applicative:"
    log $ show $ (+) <$> Just 21 <*> Just 21
    log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)
    log $ show $ pure (+) <*> Just 17 <*> Just 25

    log "Either Applicative Laws:"
    log "Associative composition: (<<<) <$> u <*> v <*> w = u <*> (v <*> w)"
    log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) == (pure identity <*> (pure identity <*> pure 1) :: Either Unit Int)
    log "Identity: pure identity <*> x = x"
    log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit Int)
    log "Homomorphism: pure (f x) = pure f <*> pure x"
    log $ show $ pure (negate 1) == (pure negate <*> pure 1 :: Either Unit Int)
    log "Interchange: u <*> pure x = pure (_ $ x) <*> u"
    log $ show $ (pure negate <*> pure 1) == (pure (_ $ 1) <*> pure negate :: Either Unit Int)
