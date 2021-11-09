module Ch11 where

import Prelude (Unit, show, type (~>), ($), discard)

import Data.List (List(..), (:), foldl)
import Effect (Effect)
import Effect.Console (log)

reverse :: List ~> List
reverse = foldl (\rl x -> x : rl) Nil

test :: Effect Unit
test = do
    log "reverse:"
    log $ show $ reverse (10 : 20 : 30 : Nil)

