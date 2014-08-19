-- | Main entry point to the application.
module Main where

import Control.Concurrent.MVar
import Control.Concurrent

data FutureState a = Waiting [(a -> IO ())] | Done a

data Future a = Future (IO (MVar (FutureState a)))

emptyState = Waiting []

promise :: Future a
promise = Future (newMVar emptyState)

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
        Done a -> do
            return a

executeIO :: IO a -> Future a
executeIO a = Future io
    where
        io = do
            m <- (newMVar emptyState)
            forkIO (a >>= (complete m))
            return m

execute :: a -> Future a
execute a = Future io
    where
        io = do
            m <- (newMVar emptyState)
            forkIO (complete m a)
            return m

complete :: MVar (FutureState a) -> a -> IO ()
complete m a = do
    state <- takeMVar m
    case state of
        Waiting callbacks -> do
            putMVar m (Done a)
            sequence_ (fmap (\b -> b a) callbacks)
            return ()
        Done _ ->
            return ()

onCompleteIO :: (a -> IO ()) -> Future a -> IO ()
onCompleteIO callback (Future io) = do
    m <- io
    state <- takeMVar m
    case state of
        Waiting callbacks -> do
            putMVar m (Waiting (callback : callbacks))
        Done a -> do
            callback a

instance Functor Future where

    fmap f (Future io) = Future io'
        where
            io' = do
                m <- io
                state <- takeMVar m
                case state of
                    Waiting callbacks -> do
                        m' <- newMVar emptyState
                        let f' = (\x -> complete m' (f x))
                        putMVar m (Waiting (f' : callbacks))
                        return m'
                    Done a -> do
                        putMVar m (Done a)
                        newMVar (Done (f a))


instance Monad Future where

    (Future io) >>= f = Future io'
        where
            io' = do
                m <- io
                state <- takeMVar m
                case state of
                    Waiting callbacks -> do
                        m' <- newMVar emptyState
                        let callback = (\x -> onCompleteIO (complete m') (f x))
                        putMVar m (Waiting (callback : callbacks))
                        return m'
                    Done a -> do
                        putMVar m (Done a)
                        result
                        where
                            Future result = f a

    return = execute

-- | The main entry point.
main :: IO ()
main = do
    result <- await foobar
    putStrLn result
    where
        foo = executeIO $ putStrLn "foo"
        bar = fmap reverse (execute "rab")
        foobar = foo >> bar
