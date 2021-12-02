module Ch21 where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Data.Either (Either)
import Data.Maybe (Maybe)
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

instance monadThrowStateT :: MonadThrow e m => MonadThrow e (StateT s m) where
    throwError :: ∀ a. e -> StateT s m a
    throwError = lift <<< throwError

instance monadErrorStateT :: MonadError e m => MonadError e (StateT s m) where
    catchError :: ∀ a. StateT s m a -> (e -> StateT s m a) -> StateT s m a
    -- execute `fmx` with `s`, leveraging `catchError` of the underlying `Monad` to catch errors
    -- in the event of an error, the lambda will be executed, which replaces the failed `StateT` with
    -- a new one computed using `f` and the error `e`
    -- as `f e` returns a `StateT`, but it is inside a `StateT` lambda, the result must be run by `runStateT`
    -- to unwrap the underlying `Tuple`; for this, a state is also required, of which there is only one, `s`
    -- this means that in the event of an error, any changes to the state by `fmx` is lost if it calls `throwError`,
    -- as `runStateT` will get the original state s` instead of an updated one
    catchError (StateT fmx) f = StateT \s -> catchError (fmx s) \e -> runStateT (f e) s

-- the application's `Monad` stack
type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a
-- type alias for the app `Monad` with concrete types for errors, logs, state and result
type AppM = AppStack String String Int Unit

-- type alias representing the app's result, from back to front: state, log, error handling (error message/pure result)
type StackResult = Tuple (Tuple (Either String Unit) String) Int
-- structured result of the app that is order-independent; as this computation can fail, `result` is wrapped in a `Maybe`
type AppEffects = { log :: String, state :: Int, result :: Maybe Unit }
-- final result of the app, also containing a potential error message wrapped in a `Maybe`
type AppResult = Tuple (Maybe String) AppEffects
-- these 2 types could have been like this: `type AppResult = { log :: String, state :: Int, result :: Either String Unit }`

-- run each level inside the monad stack
runApp :: Int -> AppM -> Effect StackResult
-- by `flip`ping `runState`, the `st` parameter can be applied first, which enables this point-free definition
-- this is because: `runState :: StateT s m a -> s -> m (Tuple a s)` => `flip runState :: s -> StateT s m a -> m (Tuple a s)`
runApp st = flip runStateT st <<< runWriterT <<< runExceptT



test :: Effect Unit
test = do
    log "placeholder"
