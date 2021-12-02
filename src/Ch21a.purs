module Ch21a where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.State.Class (class MonadState, get, put)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Console as Console

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

-- the app's monad stack
type AppM = StateT Int (ExceptT String (WriterT String Effect)) Unit

-- run each level inside the monad stack
runApp :: Int -> AppM -> Effect (Tuple (Either String (Tuple Unit Int)) String)
-- by `flip`ping `runState`, the `st` parameter can be applied first, which enables this point-free definition
-- this is because: `runState :: StateT s m a -> s -> m (Tuple a s)` => `flip runState :: s -> StateT s m a -> m (Tuple a s)`
-- as a last step `results` is `map`ped over the final result to automatically convert it to the record-based format
-- could also have been written like this: `(results <$> _) <<< flip ....`, either way, the result of the computation inside the `Effect` is transformed by `results`
runApp st = runWriterT <<< runExceptT <<< flip runStateT st

-- can be used instead of tell to automatically insert `\n` characters between the logs for increased readibility
log :: ∀ m. MonadTell String m => String -> m Unit
log str = tell $ str <> "\n"

-- as `StateT` is on top of the stack and `ExceptT` is below it, state changes will be lost inside this handler, put the log will be kept
validate :: Int -> AppM
validate n = do
    s <- get
    let x = spy "s in validate" s
    log "HEY!!!!!!!!!!!!!!!!!!" -- this will not be lost on the error case as `WriterT` is below `ExceptT`
    put 10 -- this will be lost on the error case but is kept on success because of `catchError`'s implementation (`s` used in both places)
    when (n == 0) $ void $ throwError "WE CANNOT HAVE A 0 STATE!"

-- takeaway: always (99%) use `ExceptT` on top to keep all side-effects available even after an error,
-- this is achieved by not letting any other `Monad`'s `catchError` implementation to be called, only `ExceptT`'s
-- in the event that a function should roll back all side-effects in case of a failure (1%), a different monad stack
-- can be constructed that has `ExceptT` on the bottom, which essentially throws away all state changes upon an error
-- in the success case, the results would be used to update the original stack's state, while in the error case
-- some error handling logic like `catchError` would be called (or a `when`/`if-else` construct), which rolls back side-effects
app :: AppM
app = do
    log "Starting App..."
    n <- get
    catchError (validate n) (\err -> do -- call `validate`, if it fails, call the supplied lambda
        s <- get
        let x = spy "s in error handler" s
        log $ "We encountered an error: (" <> err <> ")"
        put 100
    )
    s <- get
    let x = spy "s in app" s
    put $ s + 1
    log "Incremented State"
    pure unit

test :: Effect Unit
test = do
    result1 <- runApp 0 app
    Console.log $ show result1
    result2 <- runApp 99 app
    Console.log $ show result2
