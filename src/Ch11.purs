module Ch11 where

import Prelude (class Ord, Unit, show, negate, discard, otherwise, type (~>), ($), (>), (+), (<>), (<<<))

import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.List (List(..), (:), singleton)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..), (:|))
import Effect (Effect)
import Effect.Console (log)
import Data.Semiring (class Semiring, zero)

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

findMaxFoldNE :: ∀ a. Ord a => NonEmptyList a -> a
-- unwrap NonEmptyList to a NonEmpty, which consists of a first element then the rest
-- could also use (NonEmptyList (x :| xs), but opted not to; the function below uses it though instead
findMaxFoldNE (NonEmptyList (NonEmpty x xs)) = foldl max x xs

foldl1 :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
-- could also use (NonEmpty x xs) for pattern matching, but using the operator is cleaner and more concise
foldl1 f (x :| xs) = foldl f x xs

-- use `foldl1`, which automatically uses the first element of the `NonEmpty` list as the starting state
findMaxFoldNE' :: ∀ a. Ord a => NonEmptyList a -> a
findMaxFoldNE' (NonEmptyList ne) = foldl1 max ne

sum :: List Int -> Int
sum Nil = 0
sum (x : xs) = x + sum xs

-- tail-recursive version of `sum`, where state gets passed in through the `go` function
sum' :: List Int -> Int
sum' = go 0 where
    go acc Nil      = acc
    go acc (x : xs) = go (acc + x) xs

sumFold :: List Int -> Int
sumFold = foldl (+) 0

-- generic `sum` that is available to all `Foldable`s of `Semiring`s
-- this abstracts away both the collection type and the contained/result type!
sumGeneric :: ∀ f a. Foldable f => Semiring a => f a -> a
sumGeneric = foldl (+) zero

data Tree a = Leaf a | Node (Tree a) (Tree a)

toList :: ∀ a. Tree a -> List a
toList (Leaf x) = singleton x
-- use the `append` operator to combine the two sub-lists together
toList (Node x y) = toList x <> toList y

instance foldableTree :: Foldable Tree where
    -- point-free! (originally: foldr f acc t = foldr f acc (toList t)
    foldr f acc = foldr f acc <<< toList
    foldl f acc = foldl f acc <<< toList
    foldMap f   = foldMap f <<< toList

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

    log "findMaxFoldNE:"
    log $ show $ findMaxFoldNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
    log $ show $ findMaxFoldNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))

    log "findMaxFoldNE':"
    log $ show $ findMaxFoldNE' (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
    log $ show $ findMaxFoldNE' (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))

    log "sum:"
    log $ show $ sum (1 : 2 : 3 : Nil)
    log "sum':"
    log $ show $ sum' (1 : 2 : 3 : Nil)
    log "sumFold:"
    log $ show $ sumFold (1 : 2 : 3 : Nil)
    log "sumGeneric:"
    log $ show $ sumGeneric (1.0 : 2.0 : 3.0 : Nil)
    log $ show $ sumGeneric [1, 2, 3]
    log $ show $ sumGeneric [1.0, 2.0, 3.0]

    log "Tree toList:"
    log $ show $ toList (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))

    log "Tree sum:"
    log $ show $ sumGeneric (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))


