module Main where

import Future
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
    onCompleteIO putStrLn foo
    onCompleteIO putStrLn bar
    void(await foobar)
    where
        foo = executeIO $ do
            threadDelay 2000000
            return "foo is slow"
        bar = execute "bar is fast"
        foobar = foo >> bar
