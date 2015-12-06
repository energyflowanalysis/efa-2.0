module EFA.Graph (
   -- * types
   Graph,
   CG.LabeledNode,
   CG.LabeledEdge,
   CG.Edge(CG.from, CG.to),
   CG.DirEdge(CG.DirEdge),
   CG.UndirEdge(CG.UndirEdge), CG.undirEdge,
   CG.EitherEdge(CG.EDirEdge,CG.EUndirEdge), eDirEdge, eUndirEdge,

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

eDirEdge :: node -> node -> CG.EitherEdge node
eDirEdge x y = CG.EDirEdge $ CG.DirEdge x y

eUndirEdge :: (Ord node) => node -> node -> CG.EitherEdge node
eUndirEdge x y = CG.EUndirEdge $ CG.undirEdge x y
