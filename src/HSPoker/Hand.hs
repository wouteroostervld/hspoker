module HSPoker.Hand where
import HSPoker.Card
import Data.Maybe

handFromString s | length cards == 2 = Just cards
                 | otherwise = Nothing
                 where (cards, _) = cardsFromString s

allhands = [ [c1,c2] | c1 <- cards, c2 <- cards, c1 > c2 ]

filterOnRank i r l = filter (\h -> rank (h!!i) == r ) l
filterOnSuit i r l = filter (\h -> suit (h!!i) == r ) l

filterOnRankFirst r l = filterOnRank 0 r l
filterOnRankSecond r l = filterOnRank 1 r l
filterOnSuitFirst r l = filterOnSuit 0 r l
filterOnSuitSecond r l = filterOnSuit 1 r l

handsFromRange [] = Nothing
handsFromRange (x:[]) = Nothing
handsFromRange (x:xs) = firstRank (x:xs)

firstRank (x:xs) | isJust rank1 = firstSuitOrSecondRank (filterOnRankFirst (fromJust rank1) allhands) xs
                 | x == 'X' = firstSuitOrSecondRank allhands xs
                 | otherwise = Nothing
                 where rank1 = rankFromChar x

firstSuitOrSecondRank hands [] = Nothing
firstSuitOrSecondRank hands (x:xs) | isJust suit1 = secondRank (filterOnSuitFirst (fromJust suit1) hands) xs
                                   | otherwise = secondRank hands (x:xs)
                                   where suit1 = suitFromChar x

secondRank hands [] = Nothing
secondRank hands (x:xs) | isJust rank2 = secondSuitOrEnd (filterOnRankSecond (fromJust rank2) hands) xs
                        | x == 'X' = secondSuitOrEnd hands xs
                        where rank2 = rankFromChar x

secondSuitOrEnd hands [] = Just hands
secondSuitOrEnd hands (x:xs) | xs == [] && isJust suit2 = Just $ filterOnSuitSecond (fromJust suit2) hands
                             where suit2 = suitFromChar x
