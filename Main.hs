{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
module Main where

import System.Time

import qualified R4 as R

import qualified Data.Text as T

import Data.List
import Data.Graph

import Control.Applicative

import ISO
import G

-- |Count the subjects, properties and objects
size :: R.RDF -> Int
size (R.RDF [])   = 0
size (R.RDF xs)   = length xs + length pss + length (map objects pss)
                     where pss = concat $ map props xs

-- |Subjects from RDF
subjects :: R.RDF -> [R.Subject]
subjects (R.RDF ss) = ss

-- |Properties associated with a Subject 
props :: R.Subject -> [R.Property]
props (R.Subject (n :: T.Text) ps) = ps 

-- |Objects assocaited with a property
objects :: R.Property -> [R.RDFObject]
objects (R.Property (n :: T.Text) os) = os

-- |Encode the Subjects
encode :: R.Subject -> [N]
encode (R.Subject x ps) = (as nat string) (T.unpack x) : concatMap encode' ps

-- |Encode the properties
encode' :: R.Property -> [N]
encode' (R.Property x os) = (as nat string) (T.unpack x) : map encode'' os

-- |Encode the objects
encode'' :: R.RDFObject -> N
encode'' os' = case os' of
 R.URI x -> (as nat string) (T.unpack x)
 R.BNode x -> (as nat string) (T.unpack x)
 (R.Literal x _ _) -> (as nat string) (T.unpack x)

-- |Graphs with empty differences are identical
prop_match :: Eq a => [a] -> [a] -> Bool 
prop_match gs hs = gs \\ hs == []

prop_match' :: R.RDF -> R.RDF -> Bool
prop_match' (R.RDF xs@g) (R.RDF ys@h) =
 prop_match xs ys &&
 prop_match xs' ys' &&
 prop_match xs'' ys''
 where xs' = concat $ map props xs
       ys' = concat $ map props ys 
       xs'' = map objects xs'
       ys'' = map objects ys'

-- alt syntax above
--e (R.RDF g) (R.RDF h) = length (concat (zipWith (\\) [g] [h])) == 0

instance Eq R.RDF where
 R.RDF ss == R.RDF ss' = True

instance Eq R.Subject where
 R.Subject t p == R.Subject t' p' = True

instance Eq R.Property where
 R.Property t o == R.Property t' o' = True

instance Eq R.RDFObject where
 R.Literal x y z == R.Literal x' y' z' = True
 R.URI x == R.URI x' = True
 R.BNode x == R.BNode x' = True

main = do
         ts <- getClockTime >>= (\(TOD sec _) -> return sec)
         ns <- (fmap subjects $ R.load "aniem.json") >>= return . concat . map encode
         g <- return $ buildG (0, length ns  - 1) $ r2dig $ ndcs (nub ns) ns
         --ns `par` (g `pseq` decode vertices g ns) `seq` return () -- 0 converted
         decode vertices g ns `seq` return ()
         te <- getClockTime >>= (\(TOD sec _) -> return sec)
         print (te - ts)
         return ()