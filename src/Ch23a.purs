module Ch23a where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log) -- version of `log` that can be used in both `Effect` and `Aff`

test :: Effect Unit
test = do
    log "placeholder"
