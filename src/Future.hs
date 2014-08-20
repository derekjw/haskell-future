module Future (Future, promise, completed, executeIO, execute, complete, onCompleteIO, await, bindIO) where

import Control.Concurrent.MVar
import Control.Concurrent

data FutureState a = Waiting [a -> IO ()] | Done a

newtype Future a = Future (MVar (FutureState a))

emptyState = Waiting []

promise :: IO (Future a)
promise = do
    m <- newMVar emptyState
    return $ Future m

completed :: a -> IO (Future a)
completed a = do
    m <- newMVar $ Done a
    return $ Future m

executeIO :: IO a -> IO (Future a)
executeIO a = do
    m <- newMVar emptyState
    forkIO (a >>= completeMVar m)
    return $ Future m

execute :: a -> IO (Future a)
execute a = do
    m <- newMVar emptyState
    forkIO (completeMVar m a)
    return $ Future m

completeMVar :: MVar (FutureState a) -> a -> IO ()
completeMVar m a = do
    state <- takeMVar m
    case state of
        Waiting [] ->
            putMVar m (Done a)
        Waiting callbacks -> do
            putMVar m (Done a)
            forkIO (sequence_ (fmap (\b -> b a) callbacks))
            return ()
        Done _ -> do
            putMVar m state
            return ()

complete :: Future a -> a -> IO ()
complete (Future m) = completeMVar m

await :: Future a -> IO a
await (Future m) = do
    state <- takeMVar m
    case state of
        Waiting callbacks -> do
            m2 <- newEmptyMVar
            let f = putMVar m2
            putMVar m (Waiting (f : callbacks))
            takeMVar m2
        Done a -> do
            putMVar m state
            return a

-- Not useful (yet)
tryAwait :: Future a -> IO (Maybe a)
tryAwait (Future m) = do
    state <- tryTakeMVar m
    case state of
        Nothing ->
            return Nothing
        Just (Done a) -> do
            putMVar m (Done a)
            return $ Just a
        Just waiting -> do
            putMVar m waiting
            return Nothing

onCompleteIO :: (a -> IO ()) -> Future a -> IO ()
onCompleteIO callback (Future m) = do
    state <- takeMVar m
    case state of
        Waiting callbacks ->
            putMVar m (Waiting (callback : callbacks))
        Done a -> do
            putMVar m state
            callback a

bindIO :: (a -> Future b) -> Future a -> IO (Future b)
bindIO f (Future m) = do
    state <- takeMVar m
    case state of
        Waiting callbacks -> do
            m' <- newMVar emptyState
            let callback x = onCompleteIO (completeMVar m') (f x)
            putMVar m (Waiting (callback : callbacks))
            return $ Future m'
        Done a -> do
            putMVar m state
            return $ f a

{-
instance Functor Future where

    fmap f (Future io) = Future $ do
        m <- io
        state <- takeMVar m
        case state of
            Waiting callbacks -> do
                m' <- newMVar emptyState
                let callback x = completeMVar m' (f x)
                putMVar m (Waiting (callback : callbacks))
                return m'
            Done a -> do
                putMVar m state
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
                putMVar m state
                let Future result = f a
                result

    return = completed
-}