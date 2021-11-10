module Ch13 where

import Prelude (class Show, Unit, show, discard, identity, ($), (/), (<>), (==), (<<<), (*))

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Console (log)

data Maybe a = Nothing | Just a
derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
    show = genericShow
derive instance eqMaybe :: Eq a => Eq (Maybe a)

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
derive instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b)

-- bind the first type variable `a` as only the last one can be variable
instance functorTuple :: Functor (Tuple a) where
    -- only map the second value inside the `Tuple` because this is a `Functor` not `Bifunctor`
    map f (Tuple x y) = Tuple x $ f y

data Threeple a b c = Threeple a b c
derive instance genericThreeple :: Generic (Threeple a b c) _
instance showThreeple :: (Show a, Show b, Show c) => Show (Threeple a b c) where
    show = genericShow

-- bind the first two type variables (`a`, `b`) as only the last one can be variable
instance functorThreeple :: Functor (Threeple a b) where
    -- only map the last value because this is a `Functor`
    map f (Threeple x y z) = Threeple x y $ f z

class Bifunctor f where
    bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> f a b -> f c d

rmap :: ∀ f a b c. Bifunctor f => (b -> c) -> f a b -> f a c
rmap = bimap identity

lmap :: ∀ f a b c. Bifunctor f => (a -> c) -> f a b -> f c b
-- point-free version: lmap f = flip bimap identity: flip bimap identity f == bimap f identity
lmap f = bimap f identity

instance bifunctorEither :: Bifunctor Either where
    bimap f _ (Left err)  = Left $ f err
    bimap _ g (Right x) = Right $ g x

instance bifunctorTuple :: Bifunctor Tuple where
    bimap f g (Tuple x y) = Tuple (f x) (g y)

instance bifunctorThreeple :: Bifunctor (Threeple a) where
    bimap f g (Threeple x y z) = Threeple x (f y) (g z)

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

    log "Threeple Functor:"
    log $ show $ (_ / 2) <$> Threeple 10 20 40

    log $ show $ "Maybe Identity for Nothing: " <> show ((identity <$> Nothing) == (Nothing :: Maybe Unit))
    log $ show $ "Maybe Identity for Just: " <> show ((identity <$> Just 10) == Just 10)
    let g x = x * 2
        f x = x * 3
    log $ show $ "Maybe Composition for Nothing: " <> show ((map (g <<< f) Nothing) == (map f <<< map g) Nothing)
    log $ show $ "Maybe Composition for Just: " <> show ((map (g <<< f) (Just 10)) == (map f <<< map g) (Just 10))

    log $ show $ "Tuple Identity: " <> show ((identity <$> Tuple 10 20) == Tuple 10 20)
    log $ show $ "Tuple Composition: " <> show ((map (g <<< f) (Tuple 10 20)) == (map f <<< map g) (Tuple 10 20))

    log "Either Bifunctor:"
    log $ show $ rmap (_ * 2) $ Left "error reason"
    log $ show $ rmap (_ * 2) $ (Right 10 :: Either Unit _) -- explicit type required as the first type cannot be inferred
    log $ show $ lmap toUpper $ (Left "error reason" :: Either _ Unit) -- here the second type cannot be inferred
    log $ show $ lmap toUpper $ Right 10

    log "Tuple Bifunctor:"
    log $ show $ rmap (_ * 2) $ Tuple 80 40
    log $ show $ lmap (_ / 2) $ Tuple 80 40
    log $ show $ bimap (_ / 2) (_ * 2) $ Tuple 80 40

    log "Threeple Bifunctor:"
    log $ show $ rmap (_ * 2) $ Threeple 99 80 40
    log $ show $ lmap (_ / 2) $ Threeple 99 80 40
    log $ show $ bimap (_ / 2) (_ * 2) $ Threeple 99 80 40
