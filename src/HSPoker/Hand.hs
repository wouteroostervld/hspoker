module HSPoker.Hand where
import HSPoker.Card
import Data.Maybe

handFromString s | length cards == 2 = Just cards
                 | otherwise = Nothing
                 where (cards, _) = cardsFromString s

allhands = [ [c1,c2] | c1 <- cards, c2 <- cards, c1 > c2 ]

handsFromRange "AK" = Just [ [Card Ace s1, Card King s2] | s1 <- [ Clubs .. ], s2 <- [ Clubs ..] ] 
handsFromRange "AA" = Just [ [Card Ace s1, Card Ace s2] | s1 <- [ Clubs .. ], s2 <- [ Clubs ..], s1 > s2 ] 
