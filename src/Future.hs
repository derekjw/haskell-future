module Future (Future, promise, completed, executeIO, execute, complete, onCompleteIO, await) where

import Control.Concurrent.MVar
import Control.Concurrent

data FutureState a = Waiting [a -> IO ()] | Done a

newtype Future a = Future (IO (MVar (FutureState a)))

emptyState = Waiting []

promise :: Future a
promise = Future (newMVar emptyState)

completed :: a -> Future a
completed a = Future (newMVar (Done a))

executeIO :: IO a -> Future a
executeIO a = Future $ do
    m <- newMVar emptyState
    forkIO (a >>= completeMVar m)
    return m

execute :: a -> Future a
execute a = Future $ do
    m <- newMVar emptyState
    forkIO (completeMVar m a)
    return m

completeMVar :: MVar (FutureState a) -> a -> IO ()
completeMVar m a = do
    state <- takeMVar m
    case state of
        Waiting callbacks -> do
            putMVar m (Done a)
            sequence_ (fmap (\b -> b a) callbacks)
            return ()
        Done _ ->
            return ()

complete :: Future a -> a -> IO ()
complete (Future io) a = do
    m <- io
    completeMVar m a

await :: Future a -> IO a
await (Future io) = do
    m <- io
    state <- takeMVar m
    case state of
        Waiting callbacks -> do
            m2 <- newEmptyMVar
            let f = putMVar m2
            putMVar m (Waiting (f : callbacks))
            takeMVar m2
        Done a ->
            return a

onCompleteIO :: (a -> IO ()) -> Future a -> IO ()
onCompleteIO callback (Future io) = do
    m <- io
    state <- takeMVar m
    case state of
        Waiting callbacks ->
            putMVar m (Waiting (callback : callbacks))
        Done a ->
            callback a

instance Functor Future where

    fmap f (Future io) = Future $ do
        m <- io
        state <- takeMVar m
        case state of
            Waiting callbacks -> do
                m' <- newMVar emptyState
                let f' x = completeMVar m' (f x)
                putMVar m (Waiting (f' : callbacks))
                return m'
            Done a -> do
                putMVar m (Done a)
                newMVar (Done (f a))


instance Monad Future where

    (Future io) >>= f = Future $ do
        m <- io
        state <- takeMVar m
        case state of
            Waiting callbacks -> do
                m' <- newMVar emptyState
                let callback x = onCompleteIO (completeMVar m') (f x)
                putMVar m (Waiting (callback : callbacks))
                return m'
            Done a -> do
                putMVar m (Done a)
                let Future result = f a
                result

    return = completed
