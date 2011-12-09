{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module R4 where

import Control.Monad (mzero)

import qualified Data.ByteString as B (readFile)
import qualified Data.Map as M (toList)
import qualified Data.Text as T (Text)
import qualified Data.Vector as V (toList)

import Data.Aeson
import qualified Data.Aeson.Types as J (Parser)
import Data.Attoparsec (parse, Result(..))

type RDFValue = T.Text
type Lang = T.Text
type DTyp = T.Text

data RDFObject = Literal RDFValue Lang (Maybe DTyp) | URI RDFValue | BNode RDFValue  deriving (Show)

data Property  = Property T.Text [RDFObject] deriving (Show)
data Subject   = Subject T.Text [Property] deriving (Show)

newtype RDF    = RDF [Subject] deriving Show

instance FromJSON RDFObject where
  parseJSON (Object o) = do
    typ <- o .: "type"
    val <- o .: "value"
    lang <- o .:? "lang"
    dtype <- o .:? "datatype"

    return $ case (typ :: T.Text) of
      "literal" -> Literal val (maybe "en" id lang) dtype
      "bnode"   -> BNode val
      _         -> URI val
  parseJSON _ = mzero


instance FromJSON [Property] where
  parseJSON (Object o) = mapM getRdfObject (M.toList o)
    where
      getRdfObject :: (T.Text, Value) -> J.Parser Property
      getRdfObject (propname, rdfobjs) = do
        rdfobjs' <- parseJSON rdfobjs -- :: J.Parser [RDFObject]
        return $ Property propname rdfobjs'
  parseJSON _ = mzero


instance FromJSON RDF where
  parseJSON (Object o) = fmap RDF $ mapM getSubject (M.toList o)
    where
      getSubject :: (T.Text, Value) -> J.Parser Subject
      getSubject (subjname, props) = do
        props' <- parseJSON props -- :: J.Parser [Property]
        return $ Subject subjname props'
  parseJSON _ = mzero

load :: FilePath -> IO RDF
load path = do
   str <- B.readFile path
   case parse (fmap fromJSON json) str of
    Done _ (Success rdf) -> return $ (rdf :: RDF)
    _ -> error "failed load"