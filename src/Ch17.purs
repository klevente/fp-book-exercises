module Ch17

-- this is a list of exported items that can be imported by others
( Age (..) -- export both the type and data constructors
, Either (..) -- required for `Validation`'s data constructor
, FamilyAges(..) -- export both the type and data constructors, but users should construct instances using `createFamilyAges`
, FamilyAgesRow -- export the type alias
, Validation(..) -- required for `createFamilyAges` as it returns a `Validation` instance
, createFamilyAges -- smart constructor for creating `FamilyAges` instances
, test
) where

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
derive instance genericValidation :: Generic (Validation err result) _
instance showValidation :: (Show err, Show result) => Show (Validation err result) where
    show = genericShow

newtype Age = Age Int
derive instance genericAge :: Generic Age _
instance showAge :: Show Age where
    show = genericShow -- this prints `(Age 10)`, while using `derive newtype instance` prints `10`
newtype FullName = FullName String
derive instance genericFullName :: Generic FullName _
instance showFullName :: Show FullName where
    show = genericShow -- this prints `(FullName xy)`, while using `derive newtype instance` prints `xy`

-- create rows for storing a family's age and names separately
-- the type parameter `r` signals that these are extensible rows, meaning that they can be combined
type FamilyAgesRow r = ( fatherAge :: Age, motherAge :: Age, childAge :: Age | r )
type FamilyNamesRow r = ( fatherName :: FullName, motherName :: FullName, childName :: FullName | r )
-- create a type for a family, which holds the names and ages of its members
-- for this, the empty `Family` record is extedned with the two rows defined before
newtype Family = Family { | FamilyNamesRow (FamilyAgesRow ()) } -- the `()` signals that `Ages` is extended by an empty row
-- can also be written like this: `Family (Record (FamilyNamesRow (FamilyAgesRow ())))`
derive instance genericFamily :: Generic Family _
instance showFamily :: Show Family where
    show = genericShow

newtype FamilyAges = FamilyAges { | FamilyAgesRow () }
derive instance genericFamilyAges :: Generic FamilyAges _
instance showFamilyAges :: Show FamilyAges where
    show = genericShow

newtype UpperAge = UpperAge Int
newtype LowerAge = LowerAge Int
-- validates that a given age is inside the given bounds; the `String` parameter is an error message to append
validateAge :: LowerAge -> UpperAge -> Age -> String -> Validation (Array String) Age
-- destructure newtypes as soon as possible as they've served their purpose of type safety and self-documentation
validateAge (LowerAge lower) (UpperAge upper) (Age age) who
    | age > upper = Validation $ Left [ who <> " is too old" ]
    | age < lower = Validation $ Left [ who <> " is too young" ]
    | otherwise   = Validation $ Right $ Age age

-- smart constructor for `FamilyAges`, i.e. the user can only call this function to create an instance of `FamilyAges`
-- the input parameter `FamilyAgesRow` can be any record that contains the same fields the row contains
-- this ensures that only valid `FamilyAges` instances can be created, as the inputs must pass through this function
createFamilyAges :: { | FamilyAgesRow () } -> Validation (Array String) FamilyAges
createFamilyAges { fatherAge, motherAge, childAge } = FamilyAges
    <$> ({ fatherAge: _, motherAge: _, childAge: _ } -- lambda that constructs the record with the supplied 3 parameters denoted by `_`
    <$> (validateAge (LowerAge 18) (UpperAge 100) fatherAge "Father") -- apply the result of this to the lambda, moving it into the context
    <*> (validateAge (LowerAge 18) (UpperAge 100) motherAge "Mother") -- apply the next parameter, now the partially applied function is also inside the context
    <*> (validateAge (LowerAge 1) (UpperAge 18) childAge "Child")) -- apply the final parameter, which yields the result in the context

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

    log "createFamilyAges:"
    log $ show $ createFamilyAges { fatherAge: Age 40,  motherAge: Age 30,  childAge: Age 10 }
    log $ show $ createFamilyAges { fatherAge: Age 400, motherAge: Age 300, childAge: Age 0 }
    log $ show $ createFamilyAges { fatherAge: Age 4,   motherAge: Age 3,   childAge: Age 10 }
    log $ show $ createFamilyAges { fatherAge: Age 40,  motherAge: Age 30,  childAge: Age 100 }
    log $ show $ createFamilyAges { fatherAge: Age 40,  motherAge: Age 3,   childAge: Age 0 }

