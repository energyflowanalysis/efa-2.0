{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module EFA.Data.ND.Cube.Map where

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Data.Vector as DV
import qualified EFA.Data.ND as ND
import qualified EFA.Data.ND.Cube.Grid as Grid
import qualified EFA.Data.Axis.Strict as Strict
import EFA.Data.ND.Cube.Grid (Grid)
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Data(Nil,(:>))
import qualified EFA.Signal.Data as SD
import qualified EFA.Reference.Base as Ref
import EFA.Data.Interpolation as Interp

import EFA.Utility(Caller,merror,(|>),ModuleName(..),FunctionName, genCaller)
import EFA.Utility.Trace(mytrace)

import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import qualified Data.FixedLength as FL
import qualified Data.NonEmpty as NonEmpty
import Data.Maybe(fromMaybe)


import Prelude hiding (zipWith, map, foldl, lookup)



modul :: ModuleName
modul = ModuleName "Cube.Map"

nc :: FunctionName -> Caller
nc = genCaller modul

data Cube typ dim label vec a b = Cube {
  getGrid :: Grid typ dim label vec a,
  getData :: Data typ dim vec b }

instance
  (FL.C dim, Show label, Show (vec a), Show (vec b)) =>
    Show (Cube typ dim label vec a b) where
  showsPrec p (Cube grid dat) =
    showParen (p>10) $
      showString "Cube " . FL.showsPrec 11 grid .
      showString " " . showsPrec 11 dat

instance (FL.C dim, Show label, Ref.ToData (vec a), Ref.ToData (vec b)) =>
         Ref.ToData (Cube typ dim label vec a b) where
  toData (Cube grid dat) =
    Ref.DoubleType "Cube" (Ref.toData $ FL.Wrap grid) (Ref.toData dat)

data Data typ (dim :: * -> *) vec a =
  Data { getVector :: vec a} deriving (Show,Eq)

instance (Ref.ToData (vec a)) => Ref.ToData (Data typ dim vec a) where
  toData (Data vec) = Ref.SingleType "Cube.Data" $ Ref.toData vec

lookupLin ::
  (DV.LookupMaybe vec b, Show (vec b)) =>
  Caller -> Cube typ dim label vec a b -> Grid.LinIdx -> b
lookupLin caller (Cube _ (Data vec)) (Grid.LinIdx idx) =
  fromMaybe e $ DV.lookupMaybe vec idx
  where e = merror caller modul "lookupLinear"
            $ "linear Index out of Bounds - Index : " ++ show idx ++"- Vector: "++ show vec

lookupLinUnsafe ::
  DV.LookupUnsafe vec b =>
  Cube typ dim label vec a b -> Grid.LinIdx -> b
lookupLinUnsafe (Cube _ (Data vec)) (Grid.LinIdx idx) = DV.lookupUnsafe vec idx

lookup ::
  (FL.C dim,
   DV.LookupMaybe vec b,
   DV.Storage vec a, Show (vec b),
   DV.Length vec) =>
  Caller -> Grid.DimIdx dim -> Cube typ dim label vec a b -> b
lookup caller idx cube@(Cube grid _) =
  lookupLin (caller |> nc "lookup") cube $ Grid.toLinear grid idx

checkVector ::
  (FL.C dim, DV.Storage vec b, DV.Length vec,DV.Storage vec a) =>
  Grid typ dim label vec a -> vec b -> Bool
checkVector grid vec =
  DV.length vec == Fold.product (FL.Wrap $ FL.map Strict.len grid)


create ::
  (Ord a,
   DV.Zipper vec,
   DV.Storage vec Bool,
   DV.Storage vec a,
   DV.Singleton vec,
   DV.Storage vec b,
   DV.Length vec,
   FL.C dim
  ) =>
  Caller -> dim (label,vec a) -> vec b -> Cube typ dim label vec a b
create caller xs vec =
  let grid = Grid.create caller xs
  in  if checkVector grid vec
       then Cube grid (Data vec)
       else merror caller modul "create"
            "Vector doesn't contain the right number of elements"


generateWithGrid ::
  (FL.C dim,
   DV.Walker vec,
   DV.Storage vec b,
   DV.Storage vec (ND.Data dim a),
   DV.Storage vec (vec [a]),
   DV.Storage vec a,
   DV.Storage vec [a],
   DV.Singleton vec,
   DV.FromList vec) =>
   (ND.Data dim a -> b) -> Grid typ dim label vec a -> Cube typ dim label vec a b
generateWithGrid f grid =  Cube grid $ Data $ DV.map f $ Grid.toVector grid

map ::
  (FL.C dim,
   DV.Walker vec,
   DV.Storage vec c,
   DV.Storage vec b) =>
  (b -> c) -> Cube typ dim label vec a b -> Cube typ dim label vec a c
map f (Cube grid (Data vec)) = (Cube grid $ Data $ DV.map f vec)


mapWithGrid ::
 (FL.C dim,
  DV.Walker vec,
  DV.Storage vec c,DV.Storage vec (vec [a]), DV.FromList vec,
  DV.Zipper vec,
  DV.Storage vec b,
  DV.Storage vec (ND.Data dim a),
  DV.Storage vec a,
  DV.Storage vec [a],
  DV.Singleton vec,
  DV.Storage vec (ND.Data dim a, b)) =>
  (ND.Data dim a -> b -> c) ->
  Cube typ dim label vec a b -> Cube typ dim label vec a c
mapWithGrid f (Cube grid (Data vec)) =
  Cube grid $ Data $ DV.zipWith f (Grid.toVector grid) vec

foldl ::(DV.Walker vec, DV.Storage vec b)=>
 (acc -> b -> acc) -> acc -> Cube typ dim label vec a b -> acc
foldl f acc (Cube _ (Data vec)) = DV.foldl f acc vec

foldlWithGrid ::
  (FL.C dim,
   DV.Walker vec,
   DV.Storage vec (vec [a]),
   DV.FromList vec,
   DV.Zipper vec,
   DV.Storage vec b,
   DV.Storage vec (ND.Data dim a),
   DV.Storage vec (ND.Data dim a, b),
   DV.Walker (Strict.Axis typ label vec),
   DV.Storage (Strict.Axis typ label vec) a,
   DV.Storage (Strict.Axis typ label vec) (vec [a]),
   DV.Storage vec a,
   DV.Storage vec [a],
   DV.Singleton vec,
   DV.FromList (Strict.Axis typ label vec)) =>
  (acc -> (ND.Data dim a, b) -> acc) -> acc -> Cube typ dim label vec a b -> acc
foldlWithGrid f acc (Cube grid (Data vec)) =
  DV.foldl f acc $ DV.zip (Grid.toVector grid) vec

zipWith ::
  (DV.Zipper vec,
   DV.Storage vec d,
   DV.Storage vec c,
   DV.Storage vec b,
   Eq (Grid typ dim label vec a)) =>
  Caller ->
  (b -> c -> d) ->
  Cube typ dim label vec a b ->
  Cube typ dim label vec a c ->
  Cube typ dim label vec a d
zipWith caller f (Cube grid (Data vec)) (Cube grid1 (Data vec1)) =
  if grid == grid1 then Cube grid $ Data $ DV.zipWith f vec vec1
                   else merror caller modul "zipWith" "Grid differ"

--getData :: Cube typ dim label vec a b -> vec b
--getData (Cube _ dat) = dat

-- | Removes the first Dimension
getSubCube ::
  (FL.C dim,
   DV.Storage vec b, DV.Slice vec,
   DV.Storage vec a, DV.Length vec) =>
  Cube typ (NonEmpty.T dim) label vec a b ->
  Strict.Idx -> Cube typ dim label vec a b
getSubCube (Cube grid (Data vec)) (Strict.Idx idx) =
    Cube (NonEmpty.tail grid) $ Data subVec
  where subVec = DV.slice startIdx l vec
        startIdx = mytrace 0 "getSubCube" "startIdx" $ idx*l
        l = mytrace 0 "getSubCube" "l" $
            Strict.len $ NonEmpty.head grid

newtype Interpolate label vec typ a b dim =
  Interpolate {
    runInterpolate ::
      Cube typ (NonEmpty.T dim) label vec a b ->
      ND.Data (NonEmpty.T dim) a -> Interp.Val b
  }

interpolate ::
  (FL.C dim,
   Ord a,Arith.Constant b,DV.LookupMaybe vec b,
   DV.LookupUnsafe vec a,Show (vec b),(Show (vec a)),
   DV.Storage vec a,Show label,
   DV.Length vec,
   DV.Find vec,
   DV.Storage vec b, DV.Slice vec) =>
  Caller ->
  ((a,a) -> (b,b) -> a -> Interp.Val b) ->
  Cube typ (NonEmpty.T dim) label vec a b ->
  ND.Data (NonEmpty.T dim) a ->
  Interp.Val b
interpolate caller interpFunction =
  runInterpolate $
  FL.switch
    (Interpolate $ interpolate1 caller interpFunction)
    (Interpolate $ interpolateSucc caller interpFunction)

interpolate1 ::
  (Ord a,Arith.Constant b,DV.LookupMaybe vec b,
   DV.LookupUnsafe vec a,Show (vec b),(Show (vec a)),
   DV.Storage vec a,Show label,
   DV.Length vec,
   DV.Find vec,
   DV.Storage vec b, DV.Slice vec) =>
  Caller ->
  ((a,a) -> (b,b) -> a -> Interp.Val b) ->
  Cube typ FL.N1 label vec a b ->
  ND.Data FL.N1 a ->
  Interp.Val b
interpolate1 caller interpFunction cube coordinates =
  interpolateGen interpFunction cube coordinates $ \idx ->
    Interp.Inter $
    lookup (caller |> nc "interpolate1") (NonEmpty.singleton idx) cube

interpolateSucc ::
  (FL.C dim,
   Ord a,Arith.Constant b,DV.LookupMaybe vec b,
   DV.LookupUnsafe vec a,Show (vec b),(Show (vec a)),
   DV.Storage vec a,Show label,
   DV.Length vec,
   DV.Find vec,
   DV.Storage vec b, DV.Slice vec) =>
  Caller ->
  ((a,a) -> (b,b) -> a -> Interp.Val b) ->
  Cube typ (ND.TwoMore dim) label vec a b ->
  ND.Data (ND.TwoMore dim) a ->
  Interp.Val b
interpolateSucc caller interpFunction cube coordinates =
  interpolateGen interpFunction cube coordinates $ \idx ->
    interpolate (caller |> nc "interpolateSucc") interpFunction
      (getSubCube cube idx) (NonEmpty.tail coordinates)

interpolateGen ::
  (FL.C dim,
   Ord a, Arith.Constant b,
   DV.LookupUnsafe vec a,
   DV.Storage vec a, Show (vec a),
   DV.Storage vec b, Show (vec b),
   DV.Length vec, DV.Find vec,
   Show label) =>
  ((a,a) -> (b,b) -> a -> Interp.Val b) ->
  Cube typ (NonEmpty.T dim) label vec a b ->
  ND.Data (NonEmpty.T dim) a ->
  (Strict.Idx -> Val b) ->
  Interp.Val b
interpolateGen interpFunction cube coordinates f = Interp.combine3 y1 y2 y
  where
    axis = mytrace 0 "interpolate" "axis" $ NonEmpty.head $
           getGrid $ mytrace 0 "interpolate" "cube" cube
    x = NonEmpty.head coordinates
    ((idx1,idx2),(x1,x2)) = Strict.getSupportPoints axis x
    y1 = f idx1; y2 = f idx2
    y = interpFunction (x1,x2) (Interp.unpack y1,Interp.unpack y2) x

dimension :: FL.C dim => Cube typ dim label vec a b -> Int
dimension (Cube grid _) = length $ FL.toList grid

to2DSignal ::
    (FL.C dim,
     DV.Walker vec,
     DV.Storage vec (vec b),
     DV.Storage vec b,
     DV.Slice vec,
     DV.Storage vec a,
     DV.Length vec) =>
    Cube typ (NonEmpty.T dim) label vec a b ->
    Sig.TC Sig.Signal t (SD.Data (vec :> vec :> Nil) b)
to2DSignal (Cube grid (Data vec)) =
  Sig.TC $ SD.Data $ DV.imap f $ Strict.getVec axis
      where
        axis = NonEmpty.head grid
        l = Strict.len axis
        f idx _ = DV.slice startIdx l vec
          where
            startIdx = mytrace 0 "getSubCube" "startIdx" $ idx*l

extract ::
  (FL.C dim, FL.C dim2,
   DV.Walker vec,
   DV.Storage vec a,
   DV.Storage vec Strict.Idx,DV.LookupUnsafe vec b,
   DV.FromList vec,
   DV.Storage vec b, DV.Storage vec Grid.LinIdx,
   DV.Storage vec (ND.Data dim Strict.Idx),
   DV.Storage vec [Strict.Idx],
   DV.Storage vec (vec [Strict.Idx]),
   DV.Length vec,
   DV.Singleton vec) =>
  Caller ->
  Cube typ dim label vec a b ->
  ND.Data dim2 (ND.Idx) ->
  Map.Map ND.Idx Strict.Idx ->
  Cube typ dim2 label vec a b
extract caller cube@(Cube grid _) dims2Keep dims2Drop = Cube newGrid (Data newVec)
  where newGrid = Grid.extract newCaller grid dims2Keep
        newCaller =  caller |> nc "extractCube"
        indexVec = Grid.reductionIndexVector grid dims2Drop
        newVec = DV.map (lookupLinUnsafe cube) indexVec

