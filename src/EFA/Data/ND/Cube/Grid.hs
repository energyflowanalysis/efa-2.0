{-# LANGUAGE FlexibleContexts #-}

module EFA.Data.ND.Cube.Grid where

import qualified EFA.Data.Vector as DV
import qualified EFA.Data.ND as ND
import qualified EFA.Data.Axis.Strict as Axis
import EFA.Utility(Caller,ModuleName(..),(|>),FunctionName, genCaller)

import qualified Data.FixedLength as FL
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
 (FL.C dim, DV.Storage vec a, DV.Length vec)=>
  Grid typ dim label vec a -> DimIdx dim -> LinIdx
toLinear axes indices = LinIdx $
  Fold.foldl (\cum (ax, Axis.Idx idx) -> cum * Axis.len ax + idx) 0 $
  FL.Wrap $ FL.zipWith (,) axes indices

fromLinear ::
  (FL.C dim, DV.Storage vec a, DV.Length vec) =>
  Grid typ dim label vec a -> LinIdx -> DimIdx dim
fromLinear axes (LinIdx idx) =
  FL.unwrap $ fmap Axis.Idx $ snd $
  Trav.mapAccumR (\r ax -> divMod r (Axis.len ax)) idx $ FL.Wrap axes

create ::
  (FL.C dim, Ord a,
   DV.Zipper vec,
   DV.Storage vec a,
   DV.Storage vec Bool,
   DV.Singleton vec) =>
  Caller -> ND.Data dim (label,vec a) -> Grid typ dim label vec a
create caller = FL.map (uncurry $ Axis.fromVec newCaller)
  where newCaller = caller |> (nc "create")


-- | generate a vector as linear listing of all coordinates in a grid
toVector ::
  (FL.C dim,
   DV.Storage vec (ND.Data dim a),
   DV.Storage vec a,
   DV.FromList vec) =>
  Grid typ dim label vec a ->
  vec (ND.Data dim a)
toVector =
  DV.fromList . map FL.unwrap .
  Trav.traverse (DV.toList . Axis.getVec) . FL.Wrap

-- | Get Sizes of alle Axes
sizes ::
  (FL.C dim, DV.Storage vec a, DV.Length vec) =>
  Grid typ dim label vec a -> ND.Data dim Int
sizes = FL.map Axis.len


-- | Remove axes of specified dimensions
extract ::
  (FL.C dim, FL.C dim2) =>
  Caller ->
  Grid typ dim label vec a ->
  ND.Data dim2 ND.Idx ->
  Grid typ dim2 label vec a
extract caller grid =
  FL.map (ND.lookup (caller |> nc "extract") grid)

-- | Generate a complete index room, but restrain index for dimension to be reduced to the specified value
reductionIndexVector ::
  (FL.C dim,
   DV.Walker vec,DV.Storage vec LinIdx,DV.Length vec,
   DV.Storage vec (ND.Data dim Axis.Idx),
   DV.Singleton vec,
   DV.Storage vec Axis.Idx,
   DV.Storage vec a,
   DV.FromList vec) =>
  Grid typ dim label vec a -> Map.Map ND.Idx Axis.Idx -> vec LinIdx
reductionIndexVector axes dimensionsToReduce =
    DV.map (toLinear axes) $ toVector $ FL.zipWith f FL.indicesInt axes
  where f dim axis@(Axis.Axis l _) =
          case Map.lookup (ND.Idx dim) dimensionsToReduce of
            Just index -> Axis.Axis l $ DV.singleton index
            Nothing -> Axis.imap (\index _ -> index) axis
