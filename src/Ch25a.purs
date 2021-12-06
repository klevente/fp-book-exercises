module Ch25a where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

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

test :: Effect Unit
test = do
    log "placeholder"
