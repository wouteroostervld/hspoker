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

handsFromRange [] = Nothing
handsFromRange (x:[]) = Nothing
handsFromRange (x:xs) | isJust rank1 = suitOrSecond rank1 xs
                      | x == 'X' = wildCardSecond xs
                      | otherwise = Nothing
                      where rank1 = rankFromChar x

suitOrSecond rank1 (x:xs) | test = Just $ filterOnRankSecond (fromJust rank2) $ filterOnRankFirst (fromJust rank1) allhands
                          | x == 'X' = Just $ filterOnRankFirst (fromJust rank1) allhands
                          where rank2 = rankFromChar x
                                suit1 = suitFromChar x
                                test = isJust rank1 && isJust rank2 && rank1 >= rank2 && xs == []

wildCardSecond (x:xs) | isJust rank2 = Just $ filterOnRankSecond (fromJust rank2) allhands
                      | x == 'X' = Just allhands
                      where rank2 = rankFromChar x
