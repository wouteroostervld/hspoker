module HSPoker.Hand where
import HSPoker.Card
import Data.Maybe

handFromString s | length cards == 2 = Just cards
                 | otherwise = Nothing
                 where (cards, _) = cardsFromString s

allhands = [ [c1,c2] | c1 <- cards, c2 <- cards, c1 > c2 ]

handHasRanks r1 r2 h = (rank (h!!0) == r1) && (rank (h!!1) == r2) 
handsFromRange (x:y:xs) | test = Just $ filter (handHasRanks (fromJust rank1) (fromJust rank2)) allhands
                        | otherwise = Nothing
                where   rank1 = rankFromChar x
                        rank2 = rankFromChar y
                        test = isJust rank1 && isJust rank2 && rank1 >= rank2


