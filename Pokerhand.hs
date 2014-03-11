module Pokerhand where
import Data.Maybe

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace  deriving (Enum, Eq, Ord, Show)
data Suit = Clubs | Hearts | Diamonds | Spades deriving (Enum,Eq,Show)
data Card = Card Rank Suit deriving (Show)
rank (Card r _) = r
suit (Card _ s) = s
cards = [ Card r s | r <- [Two .. ], s <- [ Clubs .. ] ]

showSuit Clubs = 'c'
showSuit Diamonds = 'd'
showSuit Spades = 's'
showSuit Hearts = 'h'

showRank Two = '2'
showRank Three = '3'
showRank Four = '4'
showRank Five = '5'
showRank Six = '6'
showRank Seven = '7'
showRank Eight = '8'
showRank Nine = '9'
showRank Ten = 'T'
showRank Jack = 'J'
showRank Queen = 'Q'
showRank King = 'K'
showRank Ace = 'A'

showCard (Card r s ) = showRank r : showSuit s : []

suitFromChar 's' = Just Spades
suitFromChar 'c' = Just Clubs
suitFromChar 'd' = Just Diamonds
suitFromChar 'h' = Just Hearts
suitFromChar _ = Nothing 
