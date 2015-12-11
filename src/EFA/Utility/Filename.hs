module EFA.Utility.Filename where

import EFA.Utility(Caller, ModuleName(..), FunctionName, genCaller)

import EFA.Flow.Part.Index (State)
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified Data.List as List
import Data.Time.Clock (UTCTime)
import Data.Char (isSpace)


modul :: ModuleName
modul = ModuleName "EFA.Utility.Filename"

nc :: FunctionName -> Caller
nc = genCaller modul

class Filename a where
  filename :: a -> String


class FilenameList a where
  filenameFromList :: [a] -> String

instance FilenameList a => Filename [a] where
  filename = filenameFromList



instance FilenameList Char where
  filenameFromList = fillSpaces

instance Filename State where
  filename = fillSpaces . show

instance Filename UTCTime where
  filename = fillSpaces . show

fillSpaces :: String -> String
fillSpaces = map (\c -> if isSpace c then '_' else c)


instance (Filename a, Filename b) => Filename (a, b) where
  filename (x, y) = filename x ++ "-" ++ filename y

instance Filename Double where
  filename = show

instance FilenameList Double where
  filenameFromList = filenameList


instance (Filename node) => Filename (TopoIdx.Position node) where
  filename (TopoIdx.Position f t) = filename f ++ "->" ++ filename t


instance (Filename node) => FilenameList (TopoIdx.Position node) where
  filenameFromList = filenameList

filenameList :: (Filename name) => [name] -> String
filenameList =
    ('[':) . (++ "]") . List.intercalate "_" . map filename
