module Ch11 where

import Prelude (class Ord, Unit, show, negate, discard, otherwise, type (~>), ($), (>))

import Data.List (List(..), (:), foldl)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

reverse :: List ~> List
reverse = foldl (\rl x -> x : rl) Nil

max :: ∀ a. Ord a => a -> a -> a
max x y | x > y     = x
        | otherwise = y

-- implementation using regular recursion (without folds)
-- the first parameter is a default value which is returned when the supplied `List` is empty
-- this is actually very cumbersome to use as it requires the caller to pick a value that is less than any other
findMax :: ∀ a. Ord a => a -> List a -> a
findMax mx Nil = mx
findMax mx (x : xs) = findMax (max mx x) xs

-- version without the default parameter, returns `Nothing` when the list is empty
findMax' :: ∀ a. Ord a => List a -> Maybe a
findMax' Nil = Nothing
-- the book suggests to pass the whole list to `go`, but I do not think it is necessary, as it would
-- compare the first element to itself on the first iteration, which is redundant
findMax' (x : xs) = Just $ go x xs where
    go mx Nil = mx
    go mx (y : ys) = go (max mx y) ys

findMaxFold :: ∀ a. Ord a => List a -> Maybe a
findMaxFold Nil = Nothing
-- eta reduction: (\my y -> max my y) => (\my -> max my) => max
findMaxFold (x : xs) = Just $ foldl max x xs

test :: Effect Unit
test = do
    log "reverse:"
    log $ show $ reverse (10 : 20 : 30 : Nil)

    log "max:"
    log $ show $ max (-1) 99
    log $ show $ max "aa" "z"

    log "findMax:"
    log $ show $ findMax 0 (37 : 311 : -1 : 2 : 84 : Nil)
    log $ show $ findMax "" ("a" : "bbb" : "c" : Nil)

    log "findMax':"
    log $ show $ findMax' (37 : 311 : -1 : 2 : 84 : Nil)
    log $ show $ findMax' ("a" : "bbb" : "c" : Nil)

    log "findMaxFold:"
    log $ show $ findMaxFold (37 : 311 : -1 : 2 : 84 : Nil)
    log $ show $ findMaxFold ("a" : "bbb" : "c" : Nil)


