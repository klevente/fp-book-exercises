module Ch11 where

import Prelude (class Ord, Unit, show, negate, discard, otherwise, type (~>), ($), (>))

import Data.List (List(..), (:), foldl)
import Effect (Effect)
import Effect.Console (log)

reverse :: List ~> List
reverse = foldl (\rl x -> x : rl) Nil

max :: ∀ a. Ord a => a -> a -> a
max x y | x > y     = x
        | otherwise = y

test :: Effect Unit
test = do
    log "reverse:"
    log $ show $ reverse (10 : 20 : 30 : Nil)

    log $ show $ max (-1) 99
    log $ show $ max "aa" "z"

