import EFA.Data.ND.Cube.Map (Cube)
import qualified EFA.Data.ND.Cube.Map as Cube

import qualified EFA.Utility.FixedLength as FL
import EFA.Utility.FixedLength (end, (!:))

import EFA.Utility (genCaller,ModuleName(ModuleName))
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Data.Axis.Strict as Axis
import qualified EFA.Data.ND as ND
import qualified EFA.Data.ND.Cube.Grid as Grid
import EFA.Data.ND.Cube.Grid(Grid)
--import qualified EFA.Data.Vector.Sweep as Sweep
--import EFA.Data.Vector.Sweep (Sweep)
import EFA.Data.Vector.Type (Edge)
--import qualified Data.NonEmpty as NonEmpty
--import qualified EFA.Data.Collection as Collection
--import qualified EFA.Data.Signal as Signal
--import qualified EFA.Data.Type.Physical as Phys
--import qualified EFA.Data.Type.Efa as Efa
--import qualified EFA.Data.Type as Type
--import qualified EFA.Data.Record as Record


import qualified EFA.Data.Interpolation as Interp

import qualified Data.Vector as V
import qualified Data.Map as Map


interpFunction ::
  (Double, Double) -> (Double, Double) ->
  Double -> Interp.Val Double
interpFunction = Interp.dim1 "main" Interp.Linear Interp.ExtrapLinear

dat1 :: a -> ND.Data FL.N1 a
dat1 x = x!:end

dat2 :: a -> a -> ND.Data FL.N2 a
dat2 x y = x!:y!:end

main :: IO()
main = do
  let caller = genCaller (ModuleName "Main") "main"
  let x, y :: V.Vector String
      x =  V.fromList ["x1", "x2"]
      y =  V.fromList ["y1","y2"]
  let z =  V.fromList ["z11","z12","z21","z22"]
  let o :: Cube Edge FL.N2 String V.Vector String String
      o = Cube.create caller (dat2 ("x",x) ("y",y)) z
  let z11 = Cube.lookupLin caller o (Grid.LinIdx 0)
  let z12 = Cube.lookupLin caller o (Grid.LinIdx 1)
  let z21 = Cube.lookupLin caller o (Grid.LinIdx 2)
  let z22 = Cube.lookupLin caller o (Grid.LinIdx 3)
  let z11' = Cube.lookup caller (fmap Axis.Idx $ dat2 0 0) o
  let z12' = Cube.lookup caller (fmap Axis.Idx $ dat2 0 1) o
  let z21' = Cube.lookup caller (fmap Axis.Idx $ dat2 1 0) o
  let z22' = Cube.lookup caller (fmap Axis.Idx $ dat2 1 1) o

  let x1 = V.fromList [1,2]
  let y1 = V.fromList [3,4]
  let z1 = V.fromList [11,12,21,22]
  let sys1 :: Grid Edge FL.N2 String V.Vector Double
      sys1 = Grid.create caller (dat2 ("x",x1) ("y",y1))
  let o1 :: Cube Edge FL.N2 String V.Vector Double Double
      o1 = Cube.create caller (dat2 ("x",x1) ("y",y1)) z1
  let zInt = Cube.interpolate caller interpFunction o1 (dat2 1 3)
  let zInt2 = Cube.interpolate caller interpFunction o1 (dat2 1 4)
  let zInt3 = Cube.interpolate caller interpFunction o1 (dat2 2 3)
  let zInt4 = Cube.interpolate caller interpFunction o1 (dat2 2 4)
  let zInt5 = Cube.interpolate caller interpFunction o1 (dat2 1.5 3)
  let zInt6 = Cube.interpolate caller interpFunction o1 (dat2 1.5 4)
  let zInt7 = Cube.interpolate caller interpFunction o1 (dat2 1 3.5)
  let zInt8 = Cube.interpolate caller interpFunction o1 (dat2 2 3.5)
  let zInt9 = Cube.interpolate caller interpFunction o1 (dat2 1.5 3.5)

  let sig :: Sig.UTSignal2 V.Vector V.Vector Double
      sig = Cube.to2DSignal o1
  let vec = Cube.getData o1
  let cube = Cube.mapWithGrid (,) o1
  let genCube = Cube.generateWithGrid id sys1
  let subCube =
         Cube.extract caller cube (dat1 (ND.Idx 0))
            (Map.singleton (ND.Idx 1) (Axis.Idx 0))

  print o
  print z11
  print z12
  print z21
  print z22
  print z11'
  print z12'
  print z21'
  print z22'

  print ""
  print zInt
  print zInt2
  print zInt3
  print zInt4
  print zInt5
  print zInt6
  print zInt7
  print zInt8
  print zInt9

  print sig
  print vec
  print cube
  print genCube
  print subCube
