module HSPoker.Hand where
import HSPoker.Card
import Data.Maybe

handFromString s | length cards == 2 = Just cards
                 | otherwise = Nothing
                 where (cards, _) = cardsFromString s

allhands = [ [c1,c2] | c1 <- cards, c2 <- cards, c1 > c2 ]

filterOnRank i r l = filter (\h -> rank (h!!i) == r ) l

filterOnRankFirst r l = filterOnRank 0 r l
filterOnRankSecond r l = filterOnRank 1 r l

handsFromRange (x:y:xs) | test = Just $ filterOnRankSecond (fromJust rank2) $ filterOnRankFirst (fromJust rank1) allhands
                        | otherwise = Nothing
                where   rank1 = rankFromChar x
                        rank2 = rankFromChar y
                        test = isJust rank1 && isJust rank2 && rank1 >= rank2


