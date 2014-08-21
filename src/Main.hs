module Main where

import Control.Concurrent.Future
import Control.Concurrent

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
    await foobar >>= print
