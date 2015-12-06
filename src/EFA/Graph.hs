module EFA.Graph (
   -- * types
   Graph,
   CG.LabeledNode,
   CG.LabeledEdge,
   CG.Edge(CG.from, CG.to),
   CG.DirEdge(CG.DirEdge),
   CG.UndirEdge(CG.UndirEdge), -- CG.undirEdge,
   UnDirEdge, unDirEdge,
   CG.EitherEdge(CG.EDirEdge,CG.EUndirEdge), eDirEdge, eUnDirEdge,

   -- * construction
   CG.empty, CG.fromList, CG.fromMap,

   -- * extract large portions of the graph
   CG.graphMap,
   CG.nodeLabels, CG.nodeSet, CG.nodes, CG.nodeEdges,
   CG.edgeLabels, CG.edgeSet, CG.edges,

   -- * queries
   CG.isEmpty,
   CG.lookupNode, CG.lookupEdge,
   CG.adjacentEdges,
   CG.isLoop,
   CG.pathExists,
   CG.isConsistent,

   -- * manipulate labels
   CG.mapNode, CG.mapNodeWithKey,
   CG.mapEdge, CG.mapEdgeWithKey,
   CG.mapNodeWithInOut, CG.InOut,
   CG.filterEdgeWithKey,
   CG.traverseNode, CG.traverseEdge, CG.traverse,

   -- * combine graphs
   CG.checkedZipWith,
   CG.union,

   -- * manipulate indices
   CG.reverse,
   CG.reverseEdge,
   CG.mapKeys,
   CG.mapMaybeEdgeKeys,
   CG.mapEdgeKeys,

   -- * insertion and removal
   CG.deleteNode, CG.deleteNodeSet, CG.deleteEdge,
   CG.insertNode, CG.insertEdge, CG.insertEdgeSet,
   ) where

import qualified Data.Graph.Comfort as CG


type
   Graph node edge nodeLabel edgeLabel =
      CG.Graph edge node edgeLabel nodeLabel

type UnDirEdge = CG.UndirEdge

unDirEdge :: (Ord node) => node -> node -> UnDirEdge node
unDirEdge = CG.undirEdge

eDirEdge :: node -> node -> CG.EitherEdge node
eDirEdge x y = CG.EDirEdge $ CG.DirEdge x y

eUnDirEdge :: (Ord node) => node -> node -> CG.EitherEdge node
eUnDirEdge x y = CG.EUndirEdge $ unDirEdge x y
