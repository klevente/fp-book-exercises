module Ch21 where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

newtype StateT s m a = StateT (s -> m (Tuple a s))

runStateT :: ∀ s m a. StateT s m a -> (s -> m (Tuple a s))
runStateT (StateT f) = f

instance functorStateT :: Functor m => Functor (StateT s m) where
    map:: ∀ a b. (a -> b) -> StateT s m a -> StateT s m b
    -- run `mg` on `s`, which returns a `Tuple a s`, so capture it in a lambda and apply `f` to the pure value, while re-wrapping everything
    map f (StateT mg) = StateT \s -> mg s <#> \(Tuple x s') -> Tuple (f x) s'
    -- monadic version of `functorStateT`, this requires that `m` is a `Monad` so it is more restrictive
    {-map f (StateT mg) = StateT \s -> do
        (Tuple x s') <- mg s
        pure $ Tuple (f x) s'
    -}

instance applyStateT :: Monad m => Apply (StateT s m) where
    apply :: ∀ a b. StateT s m (a -> b) -> StateT s m a -> StateT s m b
    -- for this to work, the result needs to be `join`ed as it is of type `m m (Tuple a s`) (viz. `Tuple (f x) s''`)
    -- because this code `map`s (`<#>`) two times
    -- apply (StateT fmf) (StateT fmx) = StateT \s -> fmf s <#> \(Tuple f s') -> fmx s' <#> \(Tuple x s'') -> Tuple (f x) s''
    apply (StateT fmf) (StateT fmx) = StateT \s -> do
        -- extract `f` by running `fmf` with the current input state `s`
        (Tuple f s') <- fmf s
        -- extract `x` by running `fmx` with the updated state `s'`
        (Tuple x s'') <- fmx s'
        -- run the mapping `f` on `x`, put it in a `Tuple` with the last state `s''` and wrap up in `m` using `pure`
        pure $ Tuple (f x) s''

instance applicativeStateT :: Monad m => Applicative (StateT s m) where
    pure :: ∀ a. a -> StateT s m a
    -- use `m`'s `pure` to wrap the pure value `x` in its context
    pure x = StateT \s -> pure $ Tuple x s



test :: Effect Unit
test = do
    log "placeholder"
