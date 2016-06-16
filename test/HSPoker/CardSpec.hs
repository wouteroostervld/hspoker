module HSPoker.CardSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import HSPoker.Card
import Data.Maybe

main :: IO()
main = hspec $ spec

spec = do
    describe "cards" $ do
        it "has length 52" $
            length cards `shouldBe` 52

    describe "rank" $ do
        it "returns Ace for Card Ace Clubs" $ do
            rank ( Card Ace Clubs ) `shouldBe` Ace

    describe "showRank" $ do
        it "shows the right string for every rank" $ do
            map (showRank) [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ] `shouldBe`
                [ '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A' ] 

    describe "suit" $ do
        it "returns Club for Card Ace Clubs" $ do
            suit ( Card Ace Clubs ) `shouldBe` Clubs

    describe "showSuit" $ do
        it "shows the right string for every suit" $ do
            map (showSuit) [ Clubs, Diamonds, Spades, Hearts ] `shouldBe` [ 'c', 'd', 's', 'h' ]

    describe "showCard" $ do
        it "shows Ac for Card Ace Clubs" $ do
            showCard ( Card Ace Clubs ) `shouldBe` "Ac"

        it "shows Kc for Card King Clubs" $ do
            showCard ( Card King Clubs ) `shouldBe` "Kc"

        it "shows Kd for Card King Diamonds" $ do
            showCard ( Card King Diamonds ) `shouldBe` "Kd"

        it "showCard 2s for Card Two Spades" $ do
            showCard ( Card Two Spades ) `shouldBe` "2s"

        it "shows (showRank r : showSuit s : [] ) for arbitrary Card r s" $ do
            [ showRank r : showSuit s : [] | r <- [ Two .. ], s <- [ Clubs .. ] ] `shouldBe` [ showCard ( Card r s )| r <- [ Two .. ], s <- [ Clubs .. ] ]

    describe "suitFromChar" $ do
        it "should return the right type" $ do
            map ( fromJust . suitFromChar ) [ 'c', 'd', 'h', 's' ] `shouldBe` [ Clubs, Diamonds, Hearts, Spades ] 

        it "should return Nothing for non-existent suit" $ do
            suitFromChar 'x' `shouldBe` Nothing

    describe "rankFromChar" $ do
        it "should return correspondent rank for char" $ do
            map (fromJust . rankFromChar . showRank) [Two ..] `shouldBe` [Two ..]
        
    describe "cardFromString" $ do
        it "should return correspondent card for string" $ do
            map (fromJust . cardFromString . showCard) cards `shouldBe` cards

    describe "cardsFromString" $ do
        it "should return correspondent cards for string" $ do
            ( cardsFromString ( concat ( map (showCard) cards ) ) ) `shouldBe`  (Just cards)

        it "should be able to handle empty strings" $ do
            cardsFromString "" `shouldBe` (Just [])
            
        it "should be able to handle odd strings" $ do
            cardsFromString "woot!woot!" `shouldBe` Nothing 

