module Parser where

import Prelude

import Data.Either (Either)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)

-- type alias for holding the current state of the parser, this leverages all functionality possessed by `Tuple`
-- the first parameter is the string still needing to be parsed, while the second is the already parsed data
type ParserState a = Tuple String a

-- type class for parser errors
class ParserError (e :: Type) -- here `e` has an explicit definition of `Type`, as the compiler cannot infer it by itself

-- signature of a parser function, which takes in a `String` and returns an error or a `ParserState` if successful
-- the error type must have a `ParserError` type class instance that allows setting errors
type ParseFunction a e = ParserError e => String -> Either e (ParserState a)

newtype Parser a e = Parser (ParseFunction a e)

test :: Effect Unit
test = do
    log "placeholder"
