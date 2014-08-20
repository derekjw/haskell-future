module Main where

import Future
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
    foo <- executeIO $ do
        putStrLn "do the foo"
        threadDelay 2000000
        return "foo is slow"
    bar <- execute "bar is fast"
    onCompleteIO putStrLn foo
    onCompleteIO putStrLn bar
    foobar <- foo `zipIO` bar
    result <- await foobar
    print result
