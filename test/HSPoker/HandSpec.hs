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

        it "should return (4*51) - 6 hands for \"AX\"" $ do
            -- minus 6 because eg. AcAh is equivalent of AhAc
            length (fromJust (handsFromRange "AX")) `shouldBe` (4 * 51) - 6

        it "should return (4*51) - 6 hands for \"X2\"" $ do
            -- minus 6 because eg. 2c2h is equivalent of 2h2c
            length (fromJust (handsFromRange "X2")) `shouldBe` (4 * 51) - 6

        it "should return the same amount of hands for \"X8\" and \"8X\"" $ do
            length (fromJust (handsFromRange "8X")) `shouldBe` length (fromJust (handsFromRange "X8"))

        it "should return nothing for the illegal string \"1\"" $ do
            handsFromRange "1" `shouldBe` Nothing

        it "should return nothing for the illegal string \"\"" $ do
            handsFromRange "" `shouldBe` Nothing

        it "should have length (52*51)/2 for \"XX\"" $ do
            length (fromJust $ handsFromRange "XX") `shouldBe` (( 52 * 51 ) `div` 2)

        it "should return [[Card Ace Spades, Card King Spades]] hands for \"AsKs\"" $ do
            handsFromRange "AsKs" `shouldBe` Just [[Card Ace Spades, Card King Spades]]

        it "should return 13*12/2 hands for \"XcXc\"" $ do
            length (fromJust $ handsFromRange "XcXc") `shouldBe` ((13*12) `div` 2)
