module HSPoker.HandSpec where
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import HSPoker.Card
import HSPoker.Hand
import Data.Maybe
import Data.List


main :: IO()
main = hspec spec

filterAllHands r1 r2 = [ h | h <- allhands, rank (h!!0) == r1, rank (h!!1) == r2] 

spec = do
    describe "handFromString" $ do
        it "should return list with two cards for string" $ do
            handFromString "AsKs" `shouldBe` Just [Card Ace Spades, Card King Spades]

        it "should return Nothing for invalid hand" $ do
            handFromString "invalid" `shouldBe` Nothing

        it "should return Nothing for one valid card" $ do
            handFromString "Kc" `shouldBe` Nothing

    describe "allhands" $ do
        it "has length (52*51)/2" $ do
            length allhands `shouldBe` (( 52 * 51 ) `div` 2)

    describe "handsFromRange" $ do
        it "should return all the combinations for the string \"AK\"" $ do
            fmap (sort) (handsFromRange "AK") `shouldBe` Just (sort $ filterAllHands Ace King)
        it "should return all the combinations for the string \"AA\"" $ do
            fmap (sort) (handsFromRange "AA") `shouldBe` Just (sort $ filterAllHands Ace Ace)

        it "should return nothing for an illegal string \"gg\"" $ do
            handsFromRange "gg" `shouldBe` Nothing 

