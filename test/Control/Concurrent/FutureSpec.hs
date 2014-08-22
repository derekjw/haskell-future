module Control.Concurrent.FutureSpec where

import Control.Concurrent.Future
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "future" $ do

        it "executes pure code" $ do
            foo <- execute "foo"
            foo' <- await foo
            foo' `shouldBe` "foo"

        it "executes IO code" $ do
            foo <- executeIO $ return "foo"
            foo' <- await foo
            foo' `shouldBe` "foo"
