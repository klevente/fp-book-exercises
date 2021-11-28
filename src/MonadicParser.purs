module MonadicParser where

import Prelude

import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (uncons, fromCharArray)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..), snd)
import Data.Unfoldable (class Unfoldable, replicate, none)
import Effect (Effect)
import Effect.Console (log)

-- type alias for holding the current state of the parser, this leverages all functionality possessed by `Tuple`
-- the first parameter is the string still needing to be parsed, while the second is the already parsed data
type ParserState a = Tuple String a

-- type class for parser errors
class ParserError (e :: Type) where -- here `e` has an explicit definition of `Type`, as the compiler cannot infer it by itself
    eof :: e
    invalidChar :: String -> e

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
    map f p = Parser \s -> map f <$> parse p s
    -- without using the `parse` function: `map f (Parser g) = Parser \s -> (map f) <$> (g s)`

-- in Haskell/PureScript, you think for 2 hours then code for 2 minutes
-- this function can be made simpler using `Monad`s, as `Either` is one
instance applyParser :: Apply (Parser e) where
    {-apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
    -- apply the first parser's function (`f`) to `s` and check the result
    apply p1 p2 = Parser \s -> case parse p1 s of
        Left err -> Left err -- return an error if the first `Parser` fails
        -- if successful, apply the second parser's function to the remainder and check the result
        Right (Tuple s1 h) -> case parse p2 s1 of -- here `h` is the mapping function of type `a -> b`
            Left err -> Left err -- return an error if the second `Parser` fails
            Right (Tuple s2 x) -> Right $ Tuple s2 (h x) -- apply the mapping `h` to the value `x`-}
    apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
    apply p1 p2 = Parser \s -> do
        -- execute `p1` on `s` and destructure the result, the `h` returned is the mapping function
        Tuple s1 h <- parse p1 s
        -- exeute `p2` on `s1` and destructure the result, `x` is the value that needs to be mapped
        Tuple s2 x <- parse p2 s1
        -- construct the return value, which consists of the rest of the input `s2` and the mapped value `h x`
        -- as everything is happening in the `Either` monad, `pure` is needed to return an `Either` instance (`Right`)
        pure $ Tuple s2 $ h x
    -- can also be written as: `import Control.Monad (ap) (...) apply = ap`, which does this:
    -- ap ff fx = do
    --     f <- ff
    --     x <- fx
    --     pure $ f x

instance applicativeParser :: Applicative (Parser e) where
    -- return a successful parse of `x` without consuming any characters from the supplied input `s`
    pure x = Parser \s -> pure $ Tuple s x -- the `pure` here is for `Either`
-- in all 3 instances, the pattern `Parser \s -> ...` is common; it will be more apparent when coding `Monad`s,
-- as this pattern is prevalent when dealing with a `Functor` containing a function

instance bindParser :: Bind (Parser e) where
    -- this function works in the `Either` monad, meaning that every computation inside the `do` block
    -- must return `Either`, which makes it so only the happy path needs to be coded, as `bindEither` handles all the errors
    bind p f = Parser \s -> do
        -- first, run the `Parser` `p` on the input `s` and destructure the result
        Tuple s1 x <- parse p s
        -- the remainder of the string (`s1`) is parsed using `f x`, which is of type `Parser b`
        -- this is because `f` is a monadic function, meaning it is: `f :: a -> Parser b` and `x :: a`
        parse (f x) s1

-- my version returns the result unwrapped in an `Either`
myParse :: ∀ e a. ParserError e => Parser e a -> String -> Either e a
myParse (Parser f) str = snd <$> f str

-- the book's version only returns the unwrapped parsing function to the caller
parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser f) = f

-- version of `parse` that is hard-coded to return a `PError`, so it does not need to be specified on the call site
parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

data PError = EOF | InvalidChar String
derive instance genericPError :: Generic PError _
instance showPError :: Show PError where
    show = genericShow
instance parserErrorPError :: ParserError PError where
    eof = EOF
    invalidChar = InvalidChar

-- parser returns the first character of the input as the parsed value, along with the tail of the input as the rest
char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
    Nothing -> Left eof -- return `eof` of the application-specific error type if the input could not be `uncons`ed
    Just { head, tail } -> Right $ Tuple tail head -- return the tail and the head in the `Tuple` in case of success

twoCharsA :: ∀ e. Parser e (Tuple Char Char)
twoCharsA = Tuple <$> char <*> char -- leverage `<*>` to easily handle the combination of 2 parsers

-- `twoChars` using `bind`
twoCharsB :: ∀ e. Parser e (Tuple Char Char)
-- get the first char in `c1`, then capture the second in `c2`, then wrap the result up using `pure` and `Tuple`
twoCharsB = char >>= \c1 -> char >>= \c2 -> pure $ Tuple c1 c2

-- `twoChars` using `do`
twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = do
    -- get the first char in `c1`
    c1 <- char
    -- get the second char in `c2`
    c2 <- char
    -- wrap the result up using `pure` and `Tuple`
    pure $ Tuple c1 c2

-- this version combines the returned characters into a `String` using `fromCharArray` and a helper lambda
threeCharsA :: ∀ e. Parser e String
threeCharsA = (\c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char

-- `threeChars` using `do`
threeChars :: ∀ e. Parser e String
threeChars = do
    c1 <- char
    c2 <- char
    c3 <- char
    pure $ fromCharArray [c1, c2, c3]

-- create a `Parser` that can parse the same thing `n` times - remember, this function does not do any parsing,
-- just handles the creation of said parser - `parse` still needs to be call with the resulted parser to start parsing
count :: ∀ e a f. Traversable f => Unfoldable f => Int -> Parser e a -> Parser e (f a)
count n p
    | n <= 0 = pure none -- in case of not positive numbers, just return a const `Parser` with an empty element (`Unfoldable` `none`)
    | otherwise = sequence (replicate n p) -- create an `f Parser e a` and convert it to `Parser e (f a)` using `sequence` (from `Traversable`)
    -- the `replicate` function can be used because `f` is constrained to be `Unfoldable`

-- return a `Parser` that always fails, i.e. returns `Left` with the specified `ParserError`
fail :: ∀ e a. ParserError e => e -> Parser e a
-- `const` is the same as `\_ -> Left err` but it describes the intent better
fail err = Parser $ const $ Left err

satisfy :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
-- parse the char into `c`, then check if the predicate `pred` holds for it
-- if it does, return it by wrapping it using `pure`, otherwise return a `Parser` that failed with the appropriate error
satisfy expected pred = char >>= \c -> if pred c then pure c else fail $ invalidChar expected

-- use `satisfy` to match a single digit
digit :: ∀ e. ParserError e => Parser e Char
-- `isDecDigit` works on codepoints, so the input char needs to be converted to one before checking it
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

test :: Effect Unit
test = do
    log "char:"
    log $ show $ parse' char "ABC"

    log "twoChars:"
    log $ show $ parse' twoChars "ABC"

    log "threeChars:"
    log $ show $ parse' threeChars "ABC"
    log $ show $ parse' threeChars "A"

    log "count:"
    log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"
