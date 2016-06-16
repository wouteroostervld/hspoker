module HSPoker.Card where

import Control.Applicative
import Data.Maybe
import Data.List
import System.Random
import System.Random.Shuffle

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace  deriving (Enum, Eq, Ord, Show)
data Suit = Clubs | Hearts | Diamonds | Spades deriving (Ord, Eq, Enum, Show)
data Card = Card Rank Suit deriving (Show)

instance Eq Card where
    (Card r _) == (Card r' _) = r == r'

instance Ord Card where
    compare (Card r _) (Card r' _) = compare r r'

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

cardFromString:: String -> Maybe Card
cardFromString (r:s:[]) =
    case (rankFromChar r, suitFromChar s) of
            (Just rank, Just suit) -> Just (Card rank suit)
            otherwise              -> Nothing
cardFromString _ = Nothing

cardsFromString_:: String -> [Maybe Card]
cardsFromString_ "" = []
cardsFromString_ xs = cardFromString (take 2 xs) : cardsFromString_ (drop 2 xs)

cardsFromString:: String -> Maybe [Card]
cardsFromString s = sequence $ cardsFromString_ s

-- comb 2 [1,2,3] = [[1,2],[1,3],[2,3]]
comb:: Integral n => n -> [a] -> [[a]]
comb 0 _ = [[]]
comb _ [] = []
comb n (x:xs) = (fmap (x:) (comb (n-1) xs)) ++ (comb n xs)

newtype Hand = Hand [Card] deriving Show
hands:: [Card] -> [Hand]
hands cs = fmap ((Hand).sort) (comb 5 cs)
look (Hand h) = h

hand:: [Card] -> Hand
hand cs = last $ sort $ hands cs

groupByRank:: [Card] -> [(Int, Rank)]
groupByRank cs = csByCount `zip` (fmap rank $ fmap head $ csByGroup) 
    where csByGroup = group $ sort cs
          csByCount = fmap length csByGroup

groupBySuit:: [Card] -> [(Int, Suit)]
groupBySuit cs = csByCount `zip` (fmap head $ csByGroup)
    where csByGroup = group $ sort $ fmap suit cs
          csByCount = fmap length csByGroup

data HandRank = HighCard
              | OnePair
              | TwoPair
              | ThreeOfAKind
              | Straight
              | Flush
              | FullHouse
              | FourOfAKind
              | StraightFlush
              | RoyalFlush deriving (Ord, Eq, Enum, Show)

successive:: [Card] -> Bool
successive [] = False
successive (_:[]) = True
successive (a:b:xs) 
    | (rank a) == Ace = False -- Edge case no succ for Ace
    | otherwise = succ (rank a) == rank b && successive (b:xs)

handRank:: Hand -> HandRank
handRank (Hand h)
    | successive h && (length (groupBySuit h)) == 1 && (rank (last h)) == Ace = RoyalFlush
    | rank(head h) == Two && rank (last h) == Ace && successive (take 4 h) && (length (groupBySuit h)) == 1 = StraightFlush
    | successive h && (length (groupBySuit h)) == 1 = StraightFlush
    | (fst.last.sort.groupByRank) h == 4 = FourOfAKind
    | fmap fst ((sort.groupByRank) h) == [2,3] = FullHouse
    | (length.groupBySuit) h == 1 = Flush
    | rank(head h) == Two && rank (last h) == Ace && successive (take 4 h) = Straight
    | successive h = Straight
    | fmap fst ((sort.groupByRank) h) == [1,1,3] = ThreeOfAKind
    | fmap fst ((sort.groupByRank) h) == [1,2,2] = TwoPair
    | fmap fst ((sort.groupByRank) h) == [1,1,1,2] = OnePair
    | otherwise = HighCard

criticalRanks:: Hand -> [Rank]
criticalRanks (Hand h)
    = fmap snd $ (reverse.sort.groupByRank) h

instance Eq Hand where
    (==) (Hand h1) (Hand h2) 
        | hr1 == hr2 = h1 == h2
        | otherwise = False
        where hr1 = handRank (Hand h1)
              hr2 = handRank (Hand h2)

instance Ord Hand where
    compare h1 h2
        | hr1 /= hr2 = compare hr1 hr2
        | otherwise = compare (criticalRanks h1) (criticalRanks h2)
        where hr1 = handRank h1
              hr2 = handRank h2

shuffledCards:: IO [Card]
shuffledCards = shuffle' cards (length cards) <$> mkStdGen <$> randomIO

main = do
        cs <- shuffledCards
        mapM_ print cs
        
