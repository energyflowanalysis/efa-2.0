{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module EFA.Data.ND where

import EFA.Utility(Caller,merror,ModuleName(..))
import qualified EFA.Reference.Base as Ref

import qualified Data.Map as Map
import Data.Maybe.HT (toMaybe)
import Data.Maybe (fromMaybe)

import Prelude hiding (map)
import qualified Prelude as P


m:: ModuleName
m = ModuleName "Space"

data Dim1
data Succ a

-- | apply nested functor to the functor argument
type family SubDim a

type instance SubDim Dim1 = Dim1
type instance SubDim (Succ a) = a

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

class Dimensions dim where num :: Data dim a -> Int

instance Dimensions Dim1 where num _ = 1
instance Dimensions Dim2 where num _ = 2
instance Dimensions Dim3 where num _ = 3
instance Dimensions Dim4 where num _ = 4
instance Dimensions Dim5 where num _ = 5
instance Dimensions Dim6 where num _ = 6
instance Dimensions Dim7 where num _ = 7
instance Dimensions Dim8 where num _ = 8
instance Dimensions Dim9 where num _ = 9
instance Dimensions Dim10 where num _ = 10



dropFirst :: Caller -> Data dim a -> Data (SubDim dim) a
dropFirst caller (Data []) = merror caller m "dropFirst" "no dimension left"
dropFirst _ (Data (_:xs)) = Data xs

getFirst :: Caller -> Data dim a -> a
getFirst caller (Data []) = merror caller m "getFirst" "no first dimension"
getFirst _ (Data (x:_)) = x


append :: a -> Data (SubDim dim) a -> Data dim a
append x (Data xs) = Data $ x:xs

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
