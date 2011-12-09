{-# LANGUAGE NoMonomorphismRestriction #-}

module G where

import Data.Graph
import Data.List
import Data.Tree

import ISO

-- | Triples as a string
--triples = "rick plays guitar rick codes haskell rick does zen ralph travels often ralph goes far"
triples :: [Char]
--triples = "rick plays guitar ralph codes haskell dean does zen" -- 1, p
triples = "rick plays guitar rick codes haskell dean does zen" -- 2, p
--triples = "rick plays guitar ralph codes haskell ralph does zen" -- 3, p
--triples = "rick knows guitar rick knows haskell dean does zen" -- 4,?
--triples = "rick plays guitar rick does haskell dean does zen" -- 5,p
--triples = "rick plays guitar ralph codes guitar dean does zen" -- 6,p
--triples = "rick plays guitar ralph codes haskell dean does haskell" -- 7,p
--triples = "rick plays guitar rick plays guitar rick plays guitar" -- 8,p

-- | Triples as natural numbers
tns :: [N]
tns = map (as nat string) $ words triples

-- |Recover triples as string from their representation as natural numbers
triples' :: [Char]
triples' = unwords $ map (as string nat) tns

-- |Find the indices including repeating elements
ndcs :: [N] -> [N] -> [[Int]]
{-# INLINE ndcs #-}
ndcs [] _ = []
ndcs xs _ | length xs == 1 = []
ndcs (x:xs) ys = elemIndices x ys : ndcs xs ys

-- |Pair successive list elements as a list of pairs
pr' :: [Int] -> [(Int, Int)]
{-# INLINE pr' #-}
pr' [] = []
pr' xs | length xs == 1 = []
pr' (x:xs) = (x,head xs) : pr' xs

-- |Pair the successor of each list element (except last) with the first element in the list
pr :: [Int] -> [(Int, Int)]
{-# INLINE pr #-}
pr xs = zip xs' xs''
    where xs'' = if ((<) (maximum xs) (length tns - 1)) then (map (+1) xs) else (map (+1) (init xs))
          xs'  = replicate (length xs) $ (!!) (xs) 0

-- |Reduce to directed graph
r2dig :: [[Int]] -> [(Int, Int)]
{-# INLINE r2dig #-}
r2dig xs | length r == 0 = (concat $ map pr $ l)
          | length l == 0 = (pr' $ concat $ r)
          | otherwise = (concat $ map pr $ l) ++ (pr' $ concat $ r)
          where
           (l,r) = splitAt (length $ findIndices (>1) ls) xs
             where
              ls = map length xs

-- |Build the graph from the possibly repeating indices of the codes
g :: Graph
g = buildG (0, length tns - 1) $ r2dig $ ndcs (nub tns) tns

-- |Recover numeric codes from the indices
recover :: [Int] -> [N] -> [N]
recover [] _    = []
recover (p:ps) ys = (!!) ys p : recover ps ys

-- |Decode the numeric codes into the original text from a function on a list of vertices
decode :: (Integral a) => (b -> [a]) -> b -> [N] -> [String]
decode f x ys = map (as string nat) $ recover cs ys where cs = map fromIntegral $ f x

t1 :: [String]
t1 = decode topSort g tns

t2 :: [String]
t2 = decode vertices g tns
