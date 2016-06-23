{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Data.Monoid (mconcat)
import Safe
import HSPoker.Card

main = scotty 3000 $ do
  get "/hand/:cards_str" $ do
    cards <- param "cards_str"
    json $ cardsFromString cards
           >>= hand
           >>= return . ( fmap showCard ) . unHand
  get "/handRank/:cards_str" $ do
    cards <- param "cards_str"
    json $ cardsFromString cards
           >>= hand
           >>= return . show . handRank
  get "/handCriticalRanks/:cards_str" $ do
    cards <- param "cards_str"
    json $ cardsFromString cards
           >>= hand
           >>= criticalRanks
           >>= return . (fmap showRank)
  get "/handStreetRank/:cards_str" $ do
    cards <- param "cards_str"
    json $ cardsFromString cards
           >>= hand
           >>= streetRank
           >>= return . showRank


    
