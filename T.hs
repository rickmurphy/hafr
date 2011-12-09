module T where

import Test.QuickCheck
import ISO
import G

import Data.Graph
import Data.List

-- describe selection bias, proving math

-- | Configure the runs
args = Args {maxSuccess = 100, replay = Nothing, maxDiscard = 100, maxSize = 100, chatty = True}

-- |Encapsulate string 
newtype Word = Word {unWord :: String}
    deriving (Show)

-- |Restricts char to allowable characters 
instance Arbitrary Word where
  arbitrary = sized $ \ n -> do
                k <- choose (0,n)
                chrs <- sequence [ elements
                                    (['A' .. 'Z'] ++
                                     ['a' .. 'z'] ++
                                     ['0' .. '9'])
                                | _ <- [0..k]
                                ]
                return $ Word chrs

-- |Equivalence for restricted type  
instance Eq Word where
 x == y = True

-- |Failing test on [Char]. --Exception: 'ISO.hs:(264,1)-(266,29): Non-exhaustive patterns in function from_base' (after 4 tests and 3 shrinks): "\243"
prop_eq_iso_c :: [Char] -> Property
prop_eq_iso_c xs = (length xs > 0) ==> as string nat ((as nat string) xs) == xs where types = xs :: [Char]

-- |Verbose with args
qc = verboseCheckWith args prop_eq_iso_c

-- |Successful test of encode and recover using restricted type, no graph
prop_eq_iso_c' :: Word -> Bool
prop_eq_iso_c' xs = as string nat (as nat string $ unWord xs) == unWord xs where types = arbitrary :: Gen Word

-- -- |Verbose with args
qc' = verboseCheckWith args prop_eq_iso_c' 

-- |Successful test of encode and recover using restricted type, with graph
prop_eq_iso_c'' :: [Word] -> Bool
prop_eq_iso_c'' xs = (map unWord xs)
 == decode vertices g (map (as nat string) (map unWord xs))
     where g = buildG (0, length ns - 1) $ r2dig $ ndcs (nub ns) ns
               where types = arbitrary :: Gen [Word]
                     ns = map (as nat string) (map unWord xs)

-- |Verbose with args
qc'' = verboseCheckWith args prop_eq_iso_c''