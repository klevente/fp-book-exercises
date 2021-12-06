module Ch25a where

import Prelude

import Affjax as Ajax
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody
import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (launchAff_)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Generic (genericDecode, genericEncode, encodeJSON)
import Foreign.Generic.Class (class Encode, class Decode, defaultOptions)

newtype Centimeters = Centimeters Number
newtype Kilograms = Kilograms Number
newtype Years = Years Int
type Personal =
    { height :: Centimeters
    , weight :: Kilograms
    , age :: Years
    }
newtype GPA = GPA Number
data Grade = Preschool | Kindergarten | Grade Int | High Int | College Int
type Student =
    { grade :: Grade
    , teacher :: Teacher
    , gpa :: GPA
    , personal :: Personal
    }
data TeachingStatus = Student | Probationary | NonTenured | Tenured
type Teacher =
    { grades :: Array Grade
    , numberOfStudents :: Int
    , personal :: Personal
    , status :: TeachingStatus
    }

derive instance genericCentimeters :: Generic Centimeters _
-- use `derive newtype` to leverage the underlying `Number`'s implementation, prints: 12.34, which is enough for this use-case
derive newtype instance encodeCentimeters :: Encode Centimeters
derive newtype instance decodeCentimeters :: Decode Centimeters
-- use `Generic` to implement the codec, prints: `{"contents": 12.34,"tag":"Centimeters"}`, which preserves type safety, but is too complicated for here
{-instance encodeCentimeters :: Encode Centimeters where
    encode = genericEncode defaultOptions
instance decodeCentimeters :: Decode Centimeters where
    decode = genericDecode defaultOptions-}

derive instance genericKilograms :: Generic Kilograms _
derive newtype instance encodeKilograms :: Encode Kilograms
derive newtype instance decodeKilograms :: Decode Kilograms

derive instance genericYears :: Generic Years _
derive newtype instance encodeYears :: Encode Years
derive newtype instance decodeYears :: Decode Years

derive instance genericGPA :: Generic GPA _
derive newtype instance encodeGPA :: Encode GPA
derive newtype instance decodeGPA :: Decode GPA

derive instance genericGrade :: Generic Grade _
instance encodeGrade :: Encode Grade where
    encode = genericEncode defaultOptions
instance decodeGrade :: Decode Grade where
    decode = genericDecode defaultOptions

derive instance genericTeachingStatus :: Generic TeachingStatus _
instance encodeTeachingStatus :: Encode TeachingStatus where
    encode = genericEncode defaultOptions
instance decodeTeachingStatus :: Decode TeachingStatus where
    decode = genericDecode defaultOptions

-- all `Record`s are already `Encode` and `Decode` as long as all their fields are, so no further instantiations are necessary

teacher :: Teacher
teacher =
    { grades: [ Preschool, Kindergarten, Grade 1 ]
    , numberOfStudents: 23
    , personal: {
          height: Centimeters 162.56
        , weight: Kilograms 63.5
        , age: Years 31
        }
    , status: NonTenured
    }

test :: Effect Unit
test = launchAff_ do
    -- call the API: response is a `String` (json), the body is wrapped in `Just` and in `RequestBody.String`
    result <- Ajax.post ResponseFormat.string "http://localhost:3000/" $ Just $ RequestBody.String $ encodeJSON teacher
    -- `lmap` the error using `printError` to convert it to a `String`, which results in both arms of the `Either` to be `Show`able
    log $ show $ lmap Ajax.printError result

    -- one-liner version using reverse `bind`
    -- log =<< show <<< lmap Ajax.printError <$> (Ajax.post ResponseFormat.string "http://localhost:3000/" $ Just $ RequestBody.String $ encodeJSON teacher)
