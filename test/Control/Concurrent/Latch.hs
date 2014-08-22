module Control.Concurrent.Latch where

import Control.Concurrent.MVar

newtype Latch = Latch (MVar ())

newLatch :: IO Latch
newLatch = do
    mvar <- newEmptyMVar
    return $ Latch mvar

tripLatch :: Latch -> IO ()
tripLatch (Latch mvar) = fmap (const ()) $ tryPutMVar mvar ()

awaitLatch :: Latch -> IO ()
awaitLatch (Latch mvar) = readMVar mvar