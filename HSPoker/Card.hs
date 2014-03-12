module HSPoker.Card where
import Data.Maybe

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace  deriving (Enum, Eq, Ord, Show)
data Suit = Clubs | Hearts | Diamonds | Spades deriving (Enum,Eq,Show)
data Card = Card Rank Suit deriving (Eq, Show)
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

rankFromChar '2' = Just Two
rankFromChar '3' = Just Three
rankFromChar '4' = Just Four
rankFromChar '5' = Just Five
rankFromChar '6' = Just Six
rankFromChar '7' = Just Seven
rankFromChar '8' = Just Eight
rankFromChar '9' = Just Nine
rankFromChar 'T' = Just Ten
rankFromChar 'J' = Just Jack
rankFromChar 'Q' = Just Queen
rankFromChar 'K' = Just King
rankFromChar 'A' = Just Ace
rankFromChar _ = Nothing 

cardFromString ([]) = ( Nothing, [] )
cardFromString (a:[]) = ( Nothing, [a] )
cardFromString (a:b:xs) 
    | isJust r && isJust s = ( Just ( Card (fromJust r) (fromJust s)), xs)
    | otherwise = ( Nothing, a : b : xs )
    where r = rankFromChar a
          s = suitFromChar b 

cardsFromString_ a b
    | card == Nothing = ( a, remaining )
    | remaining == [] = ( a ++ [fromJust card], remaining )
    | otherwise = cardsFromString_ ( a ++ [fromJust card]) remaining 
    where result = cardFromString b
          card = fst result
          remaining = snd result

cardsFromString = cardsFromString_ []
