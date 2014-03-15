module HSPoker.HandSpec where
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import HSPoker.Card
import HSPoker.Hand
import Data.Maybe


main :: IO()
main = hspec spec

spec = do
    describe "handFromString" $ do
        it "should return list with two cards for string" $ do
            handFromString "AsKs" `shouldBe` Just [Card Ace Spades, Card King Spades]

        it "should return Nothing for invalid hand" $ do
            handFromString "invalid" `shouldBe` Nothing

        it "should return Nothing for one valid card" $ do
            handFromString "Kc" `shouldBe` Nothing
