{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Equation.Record where

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Mix as Mix

import EFA.Equation.Arithmetic (Sum, (~-), Constant, zero)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified Type.Data.Num.Unary as Unary

import qualified Data.Accessor.Basic as Accessor
import qualified Data.FixedLength as FixedLength
import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.NonEmpty ((!:))
import Data.Map (Map)
import Control.Category ((.))
import Control.Applicative (Applicative, pure, (<*>), liftA2, liftA3)
import Data.Traversable (Traversable, traverse, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid ((<>))
import Data.Tuple.HT (mapFst)

import Prelude hiding (lookup, (.))


type Indexed rec = RecIdx.Record (ToIndex rec)


newtype Absolute a = Absolute {unAbsolute :: a} deriving (Show, Eq)

instance FormatValue a => FormatValue (Absolute a) where
   formatValue (Absolute a) = formatValue a

instance Functor Absolute where
   fmap f (Absolute a) = Absolute $ f a

instance Applicative Absolute where
   pure a = Absolute a
   Absolute f <*> Absolute a = Absolute $ f a

instance Foldable Absolute where
   foldMap = foldMapDefault

instance Traversable Absolute where
   sequenceA (Absolute a) = fmap Absolute a


data Delta a = Delta {delta, before, after :: a} deriving (Show, Eq)

deltaConst :: Constant a => a -> Delta a
deltaConst x = Delta {before = x, after = x, delta = zero}

deltaCons :: Sum a => a -> a -> Delta a
deltaCons b a = Delta {before = b, after = a, delta = a~-b}

instance FormatValue a => FormatValue (Delta a) where
   formatValue rec =
      Format.list $
         Format.assign (Format.literal "delta")  (formatValue $ delta rec) :
         Format.assign (Format.literal "before") (formatValue $ before rec) :
         Format.assign (Format.literal "after")  (formatValue $ after rec) :
         []

instance Functor Delta where
   fmap f (Delta d b a) = Delta (f d) (f b) (f a)

instance Applicative Delta where
   pure a = Delta a a a
   Delta fd fb fa <*> Delta d b a = Delta (fd d) (fb b) (fa a)

instance Foldable Delta where
   foldMap = foldMapDefault

instance Traversable Delta where
   sequenceA (Delta d b a) = liftA3 Delta d b a


data ExtDelta rec a = ExtDelta {getExtDelta :: Delta (rec a)} deriving (Show, Eq)

accessExtDelta :: Accessor.T (ExtDelta rec a) (Delta (rec a))
accessExtDelta = Accessor.fromWrapper ExtDelta getExtDelta

extDeltaCons :: Sum (rec a) => rec a -> rec a -> ExtDelta rec a
extDeltaCons b a = ExtDelta $ Delta {before = b, after = a, delta = a~-b}

extBefore, extDelta, extAfter :: ExtDelta rec a -> rec a
extBefore = before . getExtDelta
extDelta  = delta  . getExtDelta
extAfter  = after  . getExtDelta

instance FormatValue (rec a) => FormatValue (ExtDelta rec a) where
   formatValue (ExtDelta rec) =
      let assign idx sel =
             Format.assign
                (Format.recordDelta idx Format.empty)
                (formatValue $ sel rec)
      in  Format.list $
             assign RecIdx.Delta  delta  :
             assign RecIdx.Before before :
             assign RecIdx.After  after  :
             []

instance Functor rec => Functor (ExtDelta rec) where
   fmap f (ExtDelta d) = ExtDelta (fmap (fmap f) d)

instance (Applicative rec) => Applicative (ExtDelta rec) where
   pure a = ExtDelta $ pure $ pure a
   ExtDelta fd <*> ExtDelta d = ExtDelta (liftA2 (<*>) fd d)

instance (Foldable rec) => Foldable (ExtDelta rec) where
   foldMap f (ExtDelta d) = foldMap (foldMap f) d

instance (Traversable rec) => Traversable (ExtDelta rec) where
   sequenceA (ExtDelta d) = fmap ExtDelta $ traverse sequenceA d


data Mix dir n a =
   Mix {total :: a, mix :: FixedLength.T (Unary.Succ n) a} deriving (Show, Eq)

type SinkMix = Mix Mix.Sink
type SourceMix = Mix Mix.Source

instance (Unary.Natural n, FormatValue a) => FormatValue (Mix dir n a) where
   formatValue rec =
      Format.mix (formatValue $ total rec) $ fmap formatValue $ mix rec

instance (Unary.Natural n) => Functor (Mix dir n) where
   fmap f (Mix s m) = Mix (f s) (FixedLength.map f m)

instance (Unary.Natural n) => Applicative (Mix dir n) where
   pure a = Mix a $ FixedLength.repeat a
   Mix fs fm <*> Mix s m = Mix (fs s) (FixedLength.zipWith ($) fm m)

instance (Unary.Natural n) => Foldable (Mix dir n) where
   foldMap f (Mix s m) = f s <> foldMap f m

instance (Unary.Natural n) => Traversable (Mix dir n) where
   sequenceA (Mix s m) = liftA2 Mix s $ FixedLength.sequenceA m



newtype
   ExtMix dir n rec a =
      ExtMix {getExtMix :: Mix dir n (rec a)} deriving (Show, Eq)

type ExtSinkMix = ExtMix Mix.Sink
type ExtSourceMix = ExtMix Mix.Source

accessExtMix :: Accessor.T (ExtMix dir n rec a) (Mix dir n (rec a))
accessExtMix = Accessor.fromWrapper ExtMix getExtMix

instance
   (FormatValue (rec a), Unary.Natural n, FormatValue a) =>
      FormatValue (ExtMix dir n rec a) where
   formatValue (ExtMix rec) =
      Format.mix (formatValue $ total rec) $ fmap formatValue $ mix rec

instance (Unary.Natural n, Functor rec) => Functor (ExtMix dir n rec) where
   fmap f (ExtMix m) = ExtMix (fmap (fmap f) m)

instance (Unary.Natural n, Applicative rec) => Applicative (ExtMix dir n rec) where
   pure a = ExtMix (pure (pure a))
   ExtMix fm <*> ExtMix m = ExtMix (liftA2 (<*>) fm m)

instance (Unary.Natural n, Foldable rec) => Foldable (ExtMix dir n rec) where
   foldMap f (ExtMix m) = foldMap (foldMap f) m

instance (Unary.Natural n, Traversable rec) => Traversable (ExtMix dir n rec) where
   sequenceA (ExtMix m) = fmap ExtMix $ traverse sequenceA m


type family FromIndex idx :: * -> *

class
   (Ord (ToIndex rec), Functor rec, Format.Record (ToIndex rec),
    rec ~ FromIndex (ToIndex rec)) =>
      C rec where
   type ToIndex rec :: *
   access :: (idx ~ ToIndex rec) => idx -> Accessor.T (rec a) a


type instance FromIndex RecIdx.Absolute = Absolute

instance C Absolute where
   type ToIndex Absolute = RecIdx.Absolute
   access RecIdx.Absolute = Accessor.fromWrapper Absolute unAbsolute


type instance FromIndex RecIdx.Delta = Delta

instance C Delta where
   type ToIndex Delta = RecIdx.Delta
   access idx =
      case idx of
         RecIdx.Delta  -> Accessor.fromSetGet (\a d -> d{delta  = a}) delta
         RecIdx.Before -> Accessor.fromSetGet (\a d -> d{before = a}) before
         RecIdx.After  -> Accessor.fromSetGet (\a d -> d{after  = a}) after


type instance FromIndex (RecIdx.ExtDelta idx) = ExtDelta (FromIndex idx)

instance C rec => C (ExtDelta rec) where
   type ToIndex (ExtDelta rec) = RecIdx.ExtDelta (ToIndex rec)

   access (RecIdx.ExtDelta idx sub) =
      access sub . access idx . accessExtDelta


type instance
   FromIndex (RecIdx.Mix dir (FixedLength.Index (Unary.Succ n))) = Mix dir n

instance (Mix.Direction dir, Unary.Natural n) => C (Mix dir n) where
   type ToIndex (Mix dir n) =
           RecIdx.Mix dir (FixedLength.Index (Unary.Succ n))
   access idx =
      case idx of
         RecIdx.MixTotal -> Accessor.fromSetGet (\a m -> m{total = a}) total
         RecIdx.MixComponent k ->
            Accessor.fromSetGet
               (\a m -> m{mix = FixedLength.update (const a) k $ mix m})
               (FixedLength.index k . mix)


type instance
   FromIndex (RecIdx.ExtMix dir (FixedLength.Index (Unary.Succ n)) idx) =
      ExtMix dir n (FromIndex idx)

instance
   (Mix.Direction dir, Unary.Natural n, C rec) =>
      C (ExtMix dir n rec) where
   type ToIndex (ExtMix dir n rec) =
           RecIdx.ExtMix dir (FixedLength.Index (Unary.Succ n)) (ToIndex rec)

   access (RecIdx.ExtMix idx sub) =
      access sub . access idx . accessExtMix


class C rec => IndexSet rec where
   indices :: (idx ~ ToIndex rec) => rec idx

instance IndexSet Absolute where
   indices = Absolute RecIdx.Absolute

instance IndexSet Delta where
   indices =
      Delta {delta = RecIdx.Delta, before = RecIdx.Before, after = RecIdx.After}

instance IndexSet rec => IndexSet (ExtDelta rec) where
   indices =
      ExtDelta $ liftA2 (fmap . RecIdx.ExtDelta) indices (pure indices)

instance (Mix.Direction dir, Unary.Natural n) => IndexSet (Mix dir n) where
   indices =
      Mix {
         total = RecIdx.MixTotal,
         mix = FixedLength.map RecIdx.MixComponent FixedLength.indices
      }

instance
   (Mix.Direction dir, Unary.Natural n, IndexSet rec) =>
      IndexSet (ExtMix dir n rec) where
   indices =
      ExtMix $ liftA2 (fmap . RecIdx.ExtMix) indices (pure indices)


class C rec => Assigns rec where
   assigns :: rec a -> NonEmpty.T [] (ToIndex rec, a)

instance Assigns Absolute where
   assigns (Absolute x) = NonEmpty.singleton (RecIdx.Absolute, x)

instance Assigns Delta where
   assigns r = (RecIdx.Before, before r) !: (RecIdx.Delta, delta r) : []

instance Assigns rec => Assigns (ExtDelta rec) where
   assigns (ExtDelta r) =
      NonEmptyC.append
         (fmap (mapFst (RecIdx.ExtDelta RecIdx.Before)) $ assigns (before r))
         (fmap (mapFst (RecIdx.ExtDelta RecIdx.Delta))  $ assigns (delta r))


{- |
Return only delta terms.
-}
assignDeltaMap :: Assigns rec => rec a -> Map (ToIndex rec) a
assignDeltaMap =
   Map.fromListWith (error "assignDeltaMap: duplicate terms") .
   NonEmpty.tail . assigns

{- |
This method fetches only the before and delta components,
in contrast to Fold.toList.
-}
summands :: Assigns rec => rec a -> NonEmpty.T [] a
summands = fmap snd . assigns
