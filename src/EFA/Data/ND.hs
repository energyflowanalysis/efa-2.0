{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module EFA.Data.ND where

import EFA.Utility(Caller,genCaller,merror,ModuleName(..))
import qualified EFA.Reference.Base as Ref

import qualified Data.Map as Map
import Data.Maybe.HT (toMaybe)
import Data.Maybe (fromMaybe)

import Prelude hiding (map, head, tail)
import qualified Prelude as P


m:: ModuleName
m = ModuleName "Space"

data Dim0
data Succ a
type Succ2 a = Succ (Succ a)

type Dim1 = Succ Dim0
type Dim2 = Succ Dim1
type Dim3 = Succ Dim2
type Dim4 = Succ Dim3
type Dim5 = Succ Dim4
type Dim6 = Succ Dim5
type Dim7 = Succ Dim6
type Dim8 = Succ Dim7
type Dim9 = Succ Dim8
type Dim10 = Succ Dim9

data Data dim a = Data [a] deriving (Show,Eq)

instance Ref.ToData a => Ref.ToData (Data dim a) where
  toData (Data xs) = Ref.DataMap "Dim.Data" $ Map.fromList $ 
                     P.zip (P.map show [(0::Integer)..]) $ P.map Ref.toData xs 

instance Functor (Data dim) where
  fmap f (Data xs) = Data (fmap f xs)

data Idx = Idx Int deriving (Show,Eq,Ord)

map :: (a -> b) -> Data dim a -> Data dim b
map f (Data xs) = Data (P.map f xs)

imap :: (Idx -> a -> b) -> Data dim a -> Data dim b
imap f (Data xs) = Data $ P.zipWith f (P.map Idx [0..]) xs

len :: Data dim a -> Int
len (Data xs) = length xs

fromList :: Dimensions dim => Caller -> [a] -> Data dim a
fromList caller xs =
  let
    dim = Data xs
  in if len dim == num dim
     then dim
     else merror caller m "fromList"
          "list length doesn't match dimension"

class Dimensions dim where
  switchDim ::
    f Dim0 -> (forall dimi. Dimensions dimi => f (Succ dimi)) -> f dim

instance Dimensions Dim0 where switchDim f _ = f
instance Dimensions dim => Dimensions (Succ dim) where switchDim _ f = f

newtype DimNum a dim = DimNum {getDimNum :: Data dim a -> Int}

num :: Dimensions dim => Data dim a -> Int
num =
  getDimNum $
  switchDim
    (DimNum $ const 0)
    (DimNum $ succ . num . tail (genCaller m "num"))


tail :: Caller -> Data (Succ dim) a -> Data dim a
tail caller (Data []) = merror caller m "tail" "no dimension left"
tail _ (Data (_:xs)) = Data xs

head :: Caller -> Data (Succ dim) a -> a
head caller (Data []) = merror caller m "head" "no first dimension"
head _ (Data (x:_)) = x


cons :: a -> Data dim a -> Data (Succ dim) a
cons x (Data xs) = Data $ x:xs

toList :: Data dim a -> [a]
toList (Data xs) = xs

lookup :: Caller -> Data dim a -> Idx -> a
lookup caller xs idx =
  fromMaybe (merror caller m "lookup" $ "Index out of Bounds: " ++ show idx) $
  lookupMaybe xs idx


lookupMaybe :: Data dim a -> Idx -> Maybe a
lookupMaybe (Data xs) (Idx idx) =
  toMaybe (idx >= 0 && idx < length xs) (xs !! idx)

data Point dim a = Point (Data dim a) deriving Show

pointFromList :: [a] -> Point dim a
pointFromList xs = Point $ Data xs

pointToList :: Point dim a -> [a]
pointToList (Point (Data xs)) = xs

mapPoint :: (a -> b) -> Point dim a -> Point dim b
mapPoint f (Point (Data xs)) = Point $ Data $ P.map f xs
