module Ch21 where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, tell)
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

instance bindStateT :: Monad m => Bind (StateT s m) where
    bind :: ∀ a b. StateT s m a -> (a -> StateT s m b) -> StateT s m b
    -- `bind` version of `Bind`, which is more terse as it uses `>>=`
    -- `runStateT` is required as the contained function must return `m (Tuple a s)`, which needs to be unwrapped from
    -- the result of `f x`, which is of type `StateT s m b`, for this, the current state (`s'`) also needs to be passed in
    bind (StateT fmx) f = StateT \s -> fmx s >>= \(Tuple x s') -> runStateT (f x) s'
    -- `do` notation version of the same, which is more verbose but maybe a bit more easy to read
    {-bind (StateT fmx) f = StateT \s -> do
        (Tuple x s') <- fmx s
        runStateT (f x) s'
    -}

instance monadStateT :: Monad m => Monad (StateT s m)

instance monadStateStateT :: Monad m => MonadState s (StateT s m) where
    state :: ∀ a. (s -> Tuple a s) -> StateT s m a
    -- run `f` with the input `s`, wrap the result up in `m`'s context using `pure`
    -- state f = StateT \s -> pure $ f s
    -- point-free version, `$` replaced with `<<<`
    state f = StateT $ pure <<< f

instance monadTransStateT :: MonadTrans (StateT s) where
    lift :: ∀ m a. Monad m => m a -> StateT s m a
    -- lift the provided monadic value `mx` into `StateT` by extracting it's pure value `x` using `<#>` and returning it alongside `s`
    lift mx = StateT \s -> mx <#> \x -> Tuple x s

instance monadAskStateT :: MonadAsk r m => MonadAsk r (StateT s m) where
    ask :: StateT s m r -- `StateT (s -> m (Tuple r s))`
    -- run `ask` on the underlying monad, then wrap the result in a `Tuple` with the input state `s`
    -- ask = StateT \s -> ask <#> \r -> Tuple r s
    -- version using `liftStateT`
    ask = lift ask

instance monadTellStateT :: MonadTell w m => MonadTell w (StateT s m) where
    tell :: w -> StateT s m Unit
    -- tell w = StateT \s -> tell w <#> \_ -> Tuple unit s
    -- point-free version using `liftStateT`
    tell = lift <<< tell

test :: Effect Unit
test = do
    log "placeholder"
