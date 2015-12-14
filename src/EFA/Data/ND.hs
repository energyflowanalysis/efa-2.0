{- |
This is an attempt to create something similar to the @repa@ package
but in the future we should replace it by @repa@.
-}
module EFA.Data.ND where

import qualified EFA.Reference.Base as Ref
import EFA.Utility(Caller,merror,ModuleName(..))

import qualified Type.Data.Num.Unary as Unary

import qualified Data.FixedLength as FL
import qualified Data.Map as Map
import Data.Foldable (fold)
import Data.Maybe (fromMaybe, listToMaybe)


m :: ModuleName
m = ModuleName "Space"

type TwoMore f = Unary.Succ (Unary.Succ f)

type Data = FL.T

instance (Unary.Natural dim, Ref.ToData a) => Ref.ToData (FL.T dim a) where
  toData xs =
    Ref.DataMap "Dim.Data" $ fold $
    FL.zipWith (\i x -> Map.singleton (show i) (Ref.toData x)) FL.indicesInt xs

data Idx = Idx Int deriving (Show,Eq,Ord)

lookup :: (Unary.Natural dim) => Caller -> Data dim a -> Idx -> a
lookup caller xs idx =
  fromMaybe (merror caller m "lookup" $ "Index out of Bounds: " ++ show idx) $
  lookupMaybe xs idx

lookupMaybe :: (Unary.Natural dim) => Data dim a -> Idx -> Maybe a
lookupMaybe xs (Idx idx) =
  listToMaybe $ drop idx $ FL.toList xs


newtype Point n a = Point (FL.T n a) deriving Show

pointFromList :: FL.T dim a -> Point dim a
pointFromList = Point

pointToList :: Point dim a -> FL.T dim a
pointToList (Point xs) = xs

mapPoint :: (Unary.Natural dim) => (a -> b) -> Point dim a -> Point dim b
mapPoint f (Point xs) = Point $ FL.map f xs
