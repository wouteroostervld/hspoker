import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Pokerhand
import Data.Maybe

main :: IO()
main = hspec $ do
    describe "cards" $ do
        it "has length 52" $
            length cards `shouldBe` 52

    describe "rank" $ do
        it "returns Ace for Card Ace Clubs" $ do
            rank ( Card Ace Clubs ) `shouldBe` Ace

    describe "showRank" $ do
        it "shows the right string for every rank" $ do
            map (showRank) [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ] `shouldBe`
                [ "2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A" ] 

    describe "suit" $ do
        it "returns Club for Card Ace Clubs" $ do
            suit ( Card Ace Clubs ) `shouldBe` Clubs

    describe "showSuit" $ do
        it "shows the right string for every suit" $ do
            map (showSuit) [ Clubs, Diamonds, Spades, Hearts ] `shouldBe` [ "c", "d", "s", "h" ]

    describe "showCard" $ do
        it "shows Ac for Card Ace Clubs" $ do
            showCard ( Card Ace Clubs ) `shouldBe` "Ac"

        it "shows Kc for Card King Clubs" $ do
            showCard ( Card King Clubs ) `shouldBe` "Kc"

        it "shows Kd for Card King Diamonds" $ do
            showCard ( Card King Diamonds ) `shouldBe` "Kd"

        it "showCard 2s for Card Two Spades" $ do
            showCard ( Card Two Spades ) `shouldBe` "2s"

        it "shows (showRank r ++ showSuit s ) for arbitrary Card r s" $ do
            [ showRank r ++ showSuit s | r <- [ Two .. ], s <- [ Clubs .. ] ] `shouldBe` [ showCard ( Card r s )| r <- [ Two .. ], s <- [ Clubs .. ] ]

    describe "suitFromString" $ do
        it "should return the right type" $ do
            map ( fromJust . suitFromString ) [ "c", "d", "h", "s" ] `shouldBe` [ Clubs, Diamonds, Hearts, Spades ] 

        it "should return Nothing for non-existent suit" $ do
            suitFromString "x" `shouldBe` Nothing
