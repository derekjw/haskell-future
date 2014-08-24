module Control.Concurrent.Latch where

import Control.Concurrent.MVar
import Control.Monad

newtype Latch = Latch (MVar ())

newLatch :: IO Latch
newLatch = do
    mvar <- newEmptyMVar
    return $ Latch mvar

tripLatch :: Latch -> IO ()
tripLatch (Latch mvar) = void $ tryPutMVar mvar ()

awaitLatch :: Latch -> IO ()
awaitLatch (Latch mvar) = readMVar mvar
