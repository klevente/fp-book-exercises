module Ch7b where

import Prelude

import Data.Int (fromString)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Console (log)

newtype CSV = CSV String -- signal that this String is in a CSV format
derive instance newtypeCSV :: Newtype CSV _
derive newtype instance eqCSV :: Eq CSV
derive newtype instance showCSV :: Show CSV

class ToCSV a where
    toCSV :: a -> CSV -- convert a Record to a CSV String

-- sample data types
newtype FullName = FullName String
derive instance newtypeFullName :: Newtype FullName _
instance showFullname :: Show FullName where
    show (FullName name) = name
derive newtype instance eqFullName :: Eq FullName

newtype Age = Age Int
derive instance newtypeAge :: Newtype Age _
derive newtype instance showAge :: Show Age
derive newtype instance eqAge :: Eq Age

data Occupation = Doctor | Dentist | Lawyer | Unemployed
derive instance eqOccupation :: Eq Occupation
derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
    show = genericShow
toOccupation :: String -> Maybe Occupation
toOccupation = case _ of -- very fragile, as adding a new occupation will fail silently, returning Nothing
    "Doctor"     -> Just Doctor
    "Dentist"    -> Just Dentist
    "Lawyer"     -> Just Lawyer
    "Unemployed" -> Just Unemployed
    _            -> Nothing

data Person = Person
    { name :: FullName
    , age :: Age
    , occupation :: Occupation
    }
derive instance eqPerson :: Eq Person


instance toCSVPerson :: ToCSV Person where
    toCSV (Person {name, age, occupation}) = CSV $ show name <> "," <> show age <> "," <> show occupation

class FromCSV a where
    fromCSV :: CSV -> Maybe a

instance fromCSVPerson :: FromCSV Person where
    fromCSV (CSV str) = case split (Pattern ",") str of -- messy implementation, clean solution requires Maybe monad
        [name, age, occupation] -> case fromString age of
            Just age' -> case toOccupation occupation of
                Just occupation' -> Just $ Person -- too much nesting
                    { name: FullName name
                    , age: Age age'
                    , occupation: occupation'
                    }
                Nothing -> Nothing
            Nothing -> Nothing
        _ -> Nothing

test :: Effect Unit
test = do
    log $ show $ toCSV
        (Person
            { name: FullName "Sue Smith"
            , age: Age 23
            , occupation: Doctor
            })
    log "toCSV Person:"
    log $ show $ toCSV
        (Person
            { name: FullName "Sue Smith"
            , age: Age 23
            , occupation: Doctor
            }) == CSV "Sue Smith,23,Doctor"
    log "Round-trip Test:"
    let person = Person
            { name: FullName "Sue Smith"
            , age: Age 23
            , occupation: Doctor
            }
    log $ show $ (toCSV person # fromCSV) == Just person
