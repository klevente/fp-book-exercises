module Ch23a where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_, forkAff, joinFiber, killFiber)
import Effect.Aff.AVar (AVar) -- `Aff` version of `AVar`
import Effect.Aff.AVar as AVar -- import `AVar` functions prefixed, so they do not collide with other functions
import Effect.Class.Console (log) -- version of `log` that can be used in both `Effect` and `Aff`
import Effect.Exception (error)

data TickTock = Tick | Tock
derive instance eqTickTock :: Eq TickTock

clock :: AVar TickTock -> Aff Unit
clock ttAVar = do
    void $ AVar.take ttAVar -- use an `AVar` function, which consumes the value inside the variable (blocking until there is a value inside)
    delay (Milliseconds 1000.0) -- wait for 1 second, this is here because the bomb `Fiber` must have enough time to check if a change has occured
    AVar.put Tock ttAVar -- first, `Tock`

    void $ AVar.take ttAVar
    delay (Milliseconds 1000.0)
    AVar.put Tick ttAVar -- after one second, `Tick`

    clock ttAVar -- recurse to keep the process flowing, the only way to terminate is to call `killFiber`

data BombState = WaitingTick | WaitingTock

bomb :: AVar TickTock -> Int -> Aff Unit
bomb ttAvar detonationCount = go 0 WaitingTick where
    -- use a helper function for keeping track of local state (current iteration `count`, current `BombState`)
    go :: Int -> BombState -> Aff Unit
    go count state = do
        if count == detonationCount then log "Boom!!" -- if the `count` is reached, `log` then exit the `Fiber` as there are no more operations or recursion
        else do
            delay (Milliseconds 500.0) -- wait before reading
            tt <- AVar.read ttAvar -- read the `AVar` without taking it, so the value stays there (blocking until there is a value inside)
            case state of
                WaitingTick -> -- if in `WaitingTick`
                    -- if `Tick` is in the variable, recurse with the same count but `WaitingTock` state
                    if tt == Tick then log "Tick" *> go count WaitingTock
                    -- else just recurse without any state changes
                    else go count state
                WaitingTock ->
                    -- if `Tock` is in the variable, recurse with an incremented count and go back to the `WaitingTick` state
                    if tt == Tock then log "Tock" *> go (count + 1) WaitingTick
                    -- else just recurse without any state changes
                    else go count state


test :: Effect Unit
test = launchAff_ do
    ttAVar <- AVar.empty -- create an empty `AVar` that does not hold any value
    clockFiber <- forkAff $ clock ttAVar
    bombFiber <- forkAff $ bomb ttAVar 3 -- detonation count is 3 seconds
    AVar.put Tick ttAVar -- `put` a value inside the `AVar`, so the two `Fiber`s can proceed as they are blocked until a value comes in

    joinFiber bombFiber -- block until `bombFiber` exits, i.e. when it detonates
     -- kill the `clockFiber`; `killFiber` also requires an error that is passed to the `Fiber`,
     -- which can be accessed by a `Canceler` if one has been attached to the `Fiber` in question
     -- however, since `forkAff` does not support them, this error is simply ignored, but if instead
     -- it was executed using `runAff`, the `Canceler` would get this value so it can work with it
    killFiber (error "Exploded") clockFiber
