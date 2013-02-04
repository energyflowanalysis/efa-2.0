module Main where

import qualified Data.Map as M
import Data.Monoid (mempty)
import Data.Ratio ((%))

import EFA.Example.Utility (recAbs, edgeVar, makeEdges, (=<>), constructSeqTopo)

import qualified EFA.Graph.Draw as Draw

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Equation.System as EqGen
import qualified EFA.Graph as Gr
import EFA.Equation.Variable (Index)
import qualified EFA.Equation.Variable as V

import EFA.Symbolic.SumProduct (Term)
import qualified EFA.Symbolic.SumProduct as T
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Env as Env
import qualified EFA.Report.FormatValue as FV
import qualified EFA.Report.Format as F
import qualified EFA.Equation.Result as R
import qualified EFA.Symbolic.TransformEquation as TransEq
import EFA.Utility.Async

sec0 :: Idx.Section
sec0 = Idx.Section 0

node0, node1, node2, node3 :: Node.Node
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom $ Node.Node 0


topoDreibein :: TD.Topology Node.Node
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Sink),
              (node2, TD.Crossing),
              (node3, TD.Storage)]
        es = [(node0, node2), (node1, node2), (node2, node3)]


{-
Use new Term type here since it simplifies automatically.
-}
given :: EqGen.EquationSystem Node.Node s (Term (Index Node.Node))
given =
   Idx.DTime recAbs Idx.initSection =<>
   Idx.DTime recAbs sec0 =<>

   Idx.Storage recAbs (Idx.SecNode sec0 node3) =<>

   edgeVar (Idx.Power recAbs) sec0 node3 node2 =<>
   edgeVar (Idx.X recAbs) sec0 node2 node3 =<>

   edgeVar (Idx.Eta recAbs) sec0 node3 node2 =<>
   edgeVar (Idx.Eta recAbs) sec0 node0 node2 =<>
   edgeVar (Idx.Eta recAbs) sec0 node2 node1 =<>

   mempty

n :: Idx.Eta Node.Node
n = edgeVar (Idx.Eta recAbs) sec0 node2 node1

n2 :: Idx.Eta Node.Node
n2 = edgeVar (Idx.Eta recAbs) sec0 node2 node2

n3 :: Idx.Eta Node.Node
n3 = edgeVar (Idx.Eta recAbs) sec0 node2 node3

en :: Idx.Energy Node.Node
en = edgeVar (Idx.Energy recAbs) sec0 node2 node3

 
x = T.Sum (M.fromList [(T.Product (M.fromList
  [ (T.Atom (V.Energy en), 1),
--    (T.Atom (V.Eta n), 1),
--    (T.Atom (V.Eta n3), 1),

    (T.Atom (V.Eta n2), -1)]),1)])


main :: IO ()
main = do

  let seqTopo = constructSeqTopo topoDreibein [1]
      env = EqGen.solve given seqTopo
      R.Determined e = (Env.energyMap env) M.! en

      demap = M.map (fmap TransEq.diff) (Env.energyMap env)
      smap = M.map (fmap TransEq.diff) (Env.storageMap env)
      xmap = M.map (const R.Undetermined) (Env.xMap env)
      ymap = M.map (const R.Undetermined) (Env.yMap env)
      memap = M.map (const R.Undetermined) (Env.maxenergyMap env)
      etamap = M.map (const R.Undetermined) (Env.etaMap env)

      denv = env { Env.energyMap = demap,
                   Env.storageMap = smap,
                   Env.etaMap = etamap,
                   Env.xMap = xmap,
                   Env.yMap = ymap,
                   Env.maxenergyMap = memap }

  --putStrLn (F.unASCII $ FV.formatValue $ x)
  --putStrLn (F.unASCII $ FV.formatValue $ TransEq.diff x)

  concurrentlyMany_ [
    Draw.sequFlowGraphAbsWithEnv seqTopo env,
    Draw.sequFlowGraphAbsWithEnv seqTopo denv ]

