module Backtracking
    ( findLastCommonAncestor
    , backtrackTo
    ) where

-- Finds the common ancestor closest to the end of both input lists. Returns
-- `Nothing` if no ancestor is found.
findLastCommonAncestor :: (Eq a) => [a] -> [a] -> Maybe a
findLastCommonAncestor l1 l2 =
    (maybeFst . safeLast . takeWhile tupleEq) $ zip l1 l2
    where
        tupleEq (n1, n2) = n1 == n2

        maybeFst Nothing = Nothing
        maybeFst (Just (n1, _)) = Just n1

-- Backtracks to the given destination from the end of the given list. Returns
-- empty if the destination doesn't exist, otherwise returns the steps to reach
-- it from the end.
backtrackTo :: (Eq a) => a -> [a] -> [a]
backtrackTo _ [] = []
backtrackTo dest l
    | dest `notElem` l = []
    | l == [dest] = [dest]
    | otherwise = ((++ [dest]) . tail . takeWhile (/= dest) . reverse) l

-- Get the last element of the list. Returns `Nothing` if the list is empty.
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast l = Just $ last l
