module Control.Concurrent.Future (Future, promise, completed, executeIO,
                                  execute, complete, onCompleteIO, await, await_,
                                  tryAwait, isComplete, bindIO, fmapIO, zipIO) where

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception
import Data.Maybe

data FutureState a = Waiting [(Either SomeException a) -> IO ()] | Done (Either SomeException a)

newtype Future a = Future (MVar (FutureState a))

emptyState :: FutureState a
emptyState = Waiting []

promise :: IO (Future a)
promise = do
    m <- newMVar emptyState
    return $ Future m

completed :: a -> IO (Future a)
completed a = do
    m <- newMVar $ Done (Right a)
    return $ Future m

executeIO :: IO a -> IO (Future a)
executeIO a = do
    m <- newMVar emptyState
    forkIO (a >>= completeMVar m . Right)
    return $ Future m

execute :: a -> IO (Future a)
execute a = do
    m <- newMVar emptyState
    forkIO (completeMVar m (Right a))
    return $ Future m

completeMVar :: MVar (FutureState a) -> (Either SomeException a) -> IO ()
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
complete (Future m) a = completeMVar m (Right a)

await :: Future a -> IO a
await (Future m) = do
    state <- takeMVar m
    case state of
        Waiting callbacks -> do
            m2 <- newEmptyMVar
            let callback = putMVar m2
            putMVar m (Waiting (callback : callbacks))
            value <- takeMVar m2
            either throw return value
        Done a -> do
            putMVar m state
            either throw return a

awaitCatch :: Future a -> IO (Either SomeException a)
awaitCatch (Future m) = do
    state <- takeMVar m
    case state of
        Waiting callbacks -> do
            m2 <- newEmptyMVar
            let callback = putMVar m2
            putMVar m (Waiting (callback : callbacks))
            takeMVar m2
        Done a -> do
            putMVar m state
            return a

await_ :: Future a -> IO ()
await_ future = do
    _ <- await future
    return ()

tryAwait :: Future a -> IO (Maybe a)
tryAwait (Future m) = do
    state <- readMVar m
    case state of
        Done a ->
            either throw (return . Just) a
        _ ->
            return Nothing

isComplete :: Future a -> IO Bool
isComplete future = do
    value <- tryAwait future
    return $ isJust value

onCompleteIO :: (Either SomeException a -> IO ()) -> Future a -> IO ()
onCompleteIO callback (Future m) = do
    state <- takeMVar m
    case state of
        Waiting callbacks ->
            putMVar m (Waiting (callback : callbacks))
        Done a -> do
            putMVar m state
            callback a

zipIO :: Future a -> Future b -> IO (Future (a, b))
zipIO fa fb = bindIO (\a -> fmapIO (\b -> (a, b)) fb) fa

bindIO :: (a -> IO (Future b)) -> Future a -> IO (Future b)
bindIO f (Future m) = do
    state <- takeMVar m
    case state of
        Waiting callbacks -> do
            m' <- newMVar emptyState
            let callback (Right x) = f x >>= onCompleteIO (completeMVar m')
                callback (Left x) = completeMVar m' (Left x)
            putMVar m (Waiting (callback : callbacks))
            return $ Future m'
        Done (Right a) -> do
            putMVar m state
            f a
        Done (Left a) -> do
            putMVar m state
            m' <- newMVar emptyState
            completeMVar m' (Left a)
            return $ Future m'

fmapIO :: (a -> b) -> Future a -> IO (Future b)
fmapIO f (Future m) = do
    state <- takeMVar m
    case state of
        Waiting callbacks -> do
            m' <- newMVar emptyState
            let callback (Right x) = completeMVar m' (Right (f x))
                callback (Left x) = completeMVar m' (Left x)
            putMVar m (Waiting (callback : callbacks))
            return $ Future m'
        Done (Right a) -> do
            putMVar m state
            m' <- newMVar (Done (Right (f a)))
            return $ Future m'
        Done (Left a) -> do
            putMVar m state
            m' <- newMVar emptyState
            completeMVar m' (Left a)
            return $ Future m'
