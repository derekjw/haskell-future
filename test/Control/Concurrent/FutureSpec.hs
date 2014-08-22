module Control.Concurrent.FutureSpec where

import Control.Concurrent.Future
import Control.Concurrent.Latch
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

        it "executes in parallel" $ do
            latch <- newLatch
            foo <- executeIO $ do
                awaitLatch latch
                return "foo"
            bar <- execute "bar"
            barResult <- await bar
            fooComplete1 <- isComplete foo
            tripLatch latch
            fooResult <- await foo
            fooComplete2 <- isComplete foo
            (fooResult, barResult, fooComplete1, fooComplete2) `shouldBe` ("foo", "bar", False, True)