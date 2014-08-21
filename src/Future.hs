module Future (Future, promise, completed, executeIO, execute, complete, onCompleteIO, await, tryAwait, isComplete, bindIO, fmapIO, zipIO) where

import Control.Concurrent.MVar
import Control.Concurrent
import Data.Maybe

data FutureState a = Waiting [a -> IO ()] | Done a

newtype Future a = Future (MVar (FutureState a))

emptyState :: FutureState a
emptyState = Waiting []

promise :: IO (Future a)
promise =
  do m <- newMVar emptyState
     return $ Future m

completed :: a -> IO (Future a)
completed a =
  do m <- newMVar $ Done a
     return $ Future m

executeIO :: IO a -> IO (Future a)
executeIO a =
  do m <- newMVar emptyState
     forkIO (a >>= completeMVar m)
     return $ Future m

execute :: a -> IO (Future a)
execute a =
  do m <- newMVar emptyState
     forkIO (completeMVar m a)
     return $ Future m

completeMVar :: MVar (FutureState a) -> a -> IO ()
completeMVar m a =
  do state <- takeMVar m
     case state of
       Waiting [] ->
         putMVar m (Done a)
       Waiting callbacks ->
         do putMVar m (Done a)
            forkIO (sequence_ (fmap (\b -> b a) callbacks))
            return ()
       Done _ ->
         do putMVar m state
            return ()

complete :: Future a -> a -> IO ()
complete (Future m) = completeMVar m

await :: Future a -> IO a
await (Future m) =
  do state <- takeMVar m
     case state of
       Waiting callbacks ->
         do m2 <- newEmptyMVar
            let f = putMVar m2
            putMVar m (Waiting (f : callbacks))
            takeMVar m2
       Done a ->
         do putMVar m state
            return a

tryAwait :: Future a -> IO (Maybe a)
tryAwait (Future m) =
  do state <- readMVar m
     case state of
       Done a ->
         return $ Just a
       _ ->
         return Nothing

isComplete :: Future a -> IO Bool
isComplete future =
  do value <- tryAwait future
     return $ isJust value

onCompleteIO :: (a -> IO ()) -> Future a -> IO ()
onCompleteIO callback (Future m) =
  do state <- takeMVar m
     case state of
       Waiting callbacks ->
         putMVar m (Waiting (callback : callbacks))
       Done a ->
         do putMVar m state
            callback a

zipIO :: Future a -> Future b -> IO (Future (a, b))
zipIO fa fb = bindIO (\a -> fmapIO (\b -> (a, b)) fb) fa

bindIO :: (a -> IO (Future b)) -> Future a -> IO (Future b)
bindIO f (Future m) =
  do state <- takeMVar m
     case state of
       Waiting callbacks ->
         do m' <- newMVar emptyState
            let callback x = do fx <- f x
                                onCompleteIO (completeMVar m') fx
            putMVar m (Waiting (callback : callbacks))
            return $ Future m'
       Done a ->
         do putMVar m state
            f a

fmapIO :: (a -> b) -> Future a -> IO (Future b)
fmapIO f (Future m) =
  do state <- takeMVar m
     case state of
       Waiting callbacks ->
         do m' <- newMVar emptyState
            let callback x = completeMVar m' (f x)
            putMVar m (Waiting (callback : callbacks))
            return $ Future m'
       Done a ->
         do putMVar m state
            m' <- newMVar (Done (f a))
            return $ Future m'
