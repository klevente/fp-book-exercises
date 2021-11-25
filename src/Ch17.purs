module Ch17 where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
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

-- leverage `Either`'s implementation when it is desirable, but allow for modifications where it is required,
-- like removing the short-circuiting nature of `Either` to allow returning all errors from a function
newtype Validation err result = Validation (Either err result)

derive instance newtypeValidation :: Newtype (Validation err result) _
-- use `derive newtype` to automatically create `Functor` and `Bifunctor` instances using the underlying `Either`
derive newtype instance functorValidation :: Functor (Validation err)
derive newtype instance bifunctorValidation :: Bifunctor Validation
derive instance eqValidation :: (Eq err, Eq result) => Eq (Validation err result) -- `newtype` is not necessary here
derive instance ordValidation :: (Ord err, Ord result) => Ord (Validation err result) -- and here
-- override `Validation`'s `Apply` instance to gather all errors instead of short-circuiting on the first one
instance applyValidation :: Semigroup err => Apply (Validation err) where
    apply (Validation (Left err1)) (Validation (Left err2)) = Validation $ Left (err1 <> err2) -- combine errors
    apply (Validation (Left err)) _ = Validation $ Left err -- if the second param was not an error but the first is, return it
    apply (Validation (Right f)) x = f <$> x  -- use `map` to handle both cases when the first param is `Right`
instance applicativeValidation :: Semigroup err => Applicative (Validation err) where
    pure = Validation <<< Right

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
