module Parser where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- type alias for holding the current state of the parser, this leverages all functionality possessed by `Tuple`
-- the first parameter is the string still needing to be parsed, while the second is the already parsed data
type ParserState a = Tuple String a

-- type class for parser errors
class ParserError (e :: Type) -- here `e` has an explicit definition of `Type`, as the compiler cannot infer it by itself

-- signature of a parser function, which takes in a `String` and returns an error or a `ParserState` if successful
-- the error type must have a `ParserError` type class instance that allows setting errors
type ParseFunction e a = ParserError e => String -> Either e (ParserState a)

newtype Parser e a = Parser (ParseFunction e a)

instance functorParser :: Functor (Parser e) where
    map :: ∀ a b. (a -> b) -> Parser e a -> Parser e b
    -- here, it is required to `map` twice as the value `f` expects is inside an `Either` and `Tuple` returned by `g`,
    -- so in order to unwrap them, both `map`'s are needed to access the inner value
    -- for double `map`ping, this is usually the preferred syntax, it is important to have `(map f)` in `()`s as
    -- the second `map` (`<$>`) expects a function as its first argument, which in this case is the `map`ped version of `f`
    -- with regular function invocations, it looks like this: `map (map f) (g s)`, which is a bit clearer
    map f (Parser g) = Parser \s -> (map f) <$> (g s)

-- in Haskell/PureScript, you think for 2 hours then code for 2 minutes
-- this function can be made simpler using `Monad`s, as `Either` is one
instance applyParser :: Apply (Parser e) where
    apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
    -- apply the first parser's function (`f`) to `s` and check the result
    apply (Parser f) (Parser g) = Parser \s -> case f s of
        Left err -> Left err -- return an error if the first `Parser` fails
        -- if successful, apply the second parser's function (`g`) to the remainder and check the result
        Right (Tuple s1 h) -> case g s1 of -- here `h` is the mapping function of type `a -> b`
            Left err -> Left err -- return an error if the second `Parser` fails
            Right (Tuple s2 x) -> Right $ Tuple s2 (h x) -- apply the mapping `h` to the value `x`

instance applicativeParser :: Applicative (Parser e) where
    -- return a successful parse of `x` without consuming any characters from the supplied input `s`
    pure x = Parser \s -> pure $ Tuple s x -- the `pure` here is for `Either`

test :: Effect Unit
test = do
    log "placeholder"
