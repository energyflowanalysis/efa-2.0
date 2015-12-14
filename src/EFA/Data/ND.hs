{- |
This is an attempt to create something similar to the @repa@ package
but in the future we should replace it by @repa@.
-}
module EFA.Data.ND where

import qualified EFA.Reference.Base as Ref
import EFA.Utility(Caller,merror,ModuleName(..))

import qualified Data.FixedLength as FL
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Foldable (fold)
import Data.Maybe (fromMaybe, listToMaybe)

import qualified Prelude as P
import Prelude


m :: ModuleName
m = ModuleName "Space"

type TwoMore f = NonEmpty.T (NonEmpty.T f)

type Data dim a = dim a

instance (FL.C dim, Ref.ToData a) => Ref.ToData (FL.Wrap dim a) where
  toData (FL.Wrap xs) =
    Ref.DataMap "Dim.Data" $ fold $ FL.Wrap $
    FL.zipWith (\i x -> Map.singleton (show i) (Ref.toData x)) FL.indicesInt xs

data Idx = Idx Int deriving (Show,Eq,Ord)

lookup :: (FL.C dim) => Caller -> Data dim a -> Idx -> a
lookup caller xs idx =
  fromMaybe (merror caller m "lookup" $ "Index out of Bounds: " ++ show idx) $
  lookupMaybe xs idx

lookupMaybe :: (FL.C dim) => Data dim a -> Idx -> Maybe a
lookupMaybe xs (Idx idx) =
  listToMaybe $ drop idx $ FL.toList xs


newtype Point f a = Point (f a) deriving Show

pointFromList :: dim a -> Point dim a
pointFromList = Point

pointToList :: Point dim a -> dim a
pointToList (Point xs) = xs

mapPoint :: (FL.C dim) => (a -> b) -> Point dim a -> Point dim b
mapPoint f (Point xs) = Point $ FL.map f xs
