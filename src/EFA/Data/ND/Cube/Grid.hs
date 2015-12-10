{-# LANGUAGE FlexibleContexts #-}

module EFA.Data.ND.Cube.Grid where

--import qualified Prelude as P
--import Prelude hiding (map)

import EFA.Utility(Caller,ModuleName(..),(|>),FunctionName, genCaller)
import qualified EFA.Data.Vector as DV
--import qualified EFA.Reference.Base as Ref

import qualified EFA.Data.ND as ND
import qualified EFA.Data.Axis.Strict as Axis
--import EFA.Data.Axis(Strict(..))

import qualified Data.Map as Map
import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold

m :: ModuleName
m = ModuleName "Grid"

nc :: FunctionName -> Caller
nc = genCaller m

-- newtype Idx = Idx {getInt :: Int} deriving Show

type Grid typ dim label vec a = ND.Data dim (Axis.Axis typ label vec a)

--instance Ref.ToData Grid where
--  toData (Grid vec) = Ref.SingleData "Grid" $ toData vec

type DimIdx dim = ND.Data dim Axis.Idx

newtype LinIdx = LinIdx {getInt:: Int} deriving (Show,Eq)

toLinear ::
 (DV.Storage vec a, DV.Length vec)=>
  Grid typ dim label vec a -> DimIdx dim -> LinIdx
toLinear axes indices = LinIdx $
  Fold.foldl (\cum (ax, Axis.Idx idx) -> cum * Axis.len ax + idx) 0 $
  ND.zip axes indices

fromLinear ::
  (DV.Storage vec a, DV.Length vec) =>
  Grid typ dim label vec a -> LinIdx -> DimIdx dim
fromLinear axes (LinIdx idx) =
  fmap Axis.Idx $ snd $
  Trav.mapAccumR (\r ax -> divMod r (Axis.len ax)) idx axes

create ::
  (ND.Dimensions dim,
   Ord a,
   DV.Zipper vec,
   DV.Storage vec a,
   DV.Storage vec Bool,
   DV.Singleton vec) =>
            Caller -> [(label,vec a)] -> Grid typ dim label vec a
create caller xs = ND.fromList newCaller
                         $ map (\(label,vec) -> Axis.fromVec newCaller label vec) xs
  where newCaller = caller |> (nc "create")


-- | generate a vector as linear listing of all coordinates in a grid
toVector ::
  (DV.Walker vec,
   DV.Storage vec (ND.Data dim a),
   DV.Singleton vec,
   DV.Storage vec [a],
   DV.Storage vec a,
   DV.Storage vec (vec [a]),
   DV.FromList vec) =>
  Grid typ dim label vec a ->
  vec (ND.Data dim a)
toVector = DV.map ND.Data . g
  where
    g (ND.Data []) = DV.singleton []
    g (ND.Data (ax:axes)) =
      DV.concat $ DV.toList $
      DV.map (\x -> DV.map (x:) $ g $ ND.Data axes) $
      Axis.getVec ax

-- | Get Sizes of alle Axes
sizes ::
  (DV.Storage vec a, DV.Length vec) =>
  Grid typ dim label vec a -> ND.Data dim Int
sizes = ND.map Axis.len


-- | Remove axes of specified dimensions
extract ::
  Caller ->
  Grid typ dim label vec a ->
  ND.Data dim2 ND.Idx ->
  Grid typ dim2 label vec a
extract caller grid dims = ND.map f dims
  where f dim = ND.lookup (caller |> nc "extract") grid dim

-- | Generate a complete index room, but restrain index for dimension to be reduced to the specified value
reductionIndexVector ::
  (DV.Walker vec,DV.Storage vec LinIdx,DV.Length vec,
   DV.Storage vec (vec [Axis.Idx]),
   DV.Storage vec [Axis.Idx],
   DV.Storage vec (ND.Data dim Axis.Idx),
   DV.Singleton vec,
   DV.Storage vec Axis.Idx,
   DV.Storage vec a,
   DV.FromList vec) =>
  Grid typ dim label vec a -> Map.Map ND.Idx Axis.Idx -> vec LinIdx
reductionIndexVector axes dimensionsToReduce = DV.map (toLinear axes) $ toVector $ ND.imap f axes
  where f dim axis@(Axis.Axis l _) = case Map.lookup dim dimensionsToReduce of
          Just index -> Axis.Axis l $ DV.fromList $ [index]
          Nothing -> Axis.imap (\index _ -> index) axis