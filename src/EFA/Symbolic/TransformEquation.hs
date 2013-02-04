

module EFA.Symbolic.TransformEquation where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Ratio

import EFA.Symbolic.SumProduct
import qualified EFA.Equation.Variable as V
import qualified EFA.Graph.Topology.Index as Idx

import Debug.Trace

data T a = A a
         | R Rational
         | Recip (T a)
         | T a :+ T a
         | T a :* T a deriving (Show)

instance Num (T a) where
         (+) = (:+)
         (*) = (:*)
         negate = ((R (-1)) :*)

instance Fractional (T a) where
         a / b = a :* (Recip b)
         recip = Recip
         fromRational = R


expandTerm :: (Ord a, Show a) => Term a -> T a
expandTerm t = evaluate f t
  where f x = A x

reduceTerm :: (Ord a, Show a) => T a -> Term a
reduceTerm (A x) = Atom x
reduceTerm (R x) = fromRational x
reduceTerm (Recip t) = recip (reduceTerm t)
reduceTerm (s :+ t) = reduceTerm s + reduceTerm t
reduceTerm (s :* t) = reduceTerm s * reduceTerm t


energyToEnergy :: T (V.Index a) -> T (V.Index a)
energyToEnergy (A (V.Power (Idx.Power a b c))) = A (V.Energy (Idx.Energy a b c))
energyToEnergy (Recip t) = recip (energyToEnergy t)
energyToEnergy (s :+ t) = energyToEnergy s + energyToEnergy t
energyToEnergy ((A (V.DTime _)) :* t) = energyToEnergy t
energyToEnergy (t :* (A (V.DTime _))) = energyToEnergy t
energyToEnergy (s :* t) = energyToEnergy s * energyToEnergy t
energyToEnergy x = x

-- id == reduceTerm . expandTerm ?
fromTermToTerm :: (Ord a, Show a) => Term a -> Term a
fromTermToTerm = reduceTerm . expandTerm




delta :: (Show a) => T (V.Index a) -> T (V.Index a)
delta (R x) = R 0
delta (A (V.Energy (Idx.Energy a b c))) =
  A (V.DEnergy (Idx.DEnergy a b c))
delta (A (V.Storage (Idx.Storage a b))) =
  A (V.DStorage (Idx.DStorage a b))
delta (A (V.Power (Idx.Power a b c))) =
  A (V.DPower (Idx.DPower a b c))
delta (A (V.X (Idx.X a b c))) =
  A (V.DX (Idx.DX a b c))
delta (A (V.Eta (Idx.Eta a b c))) =
  A (V.DEta (Idx.DEta a b c))
delta (s :+ t) = delta s + delta t
delta ((Recip s) :* t) = ((Recip ds) * t) + (ds * t * n) + (ds * dt * n)
  where ds = delta s
        dt = delta t
        n = Recip (ds * s) + (s * s)
delta (t :* (Recip s)) = (t * (Recip ds)) + (ds * t * n) + (ds * dt * n)
  where ds = delta s
        dt = delta t
        n = Recip (ds * s) + (s * s)
delta (s :* t) = (ds * t) + (s * dt) + (ds * dt)
  where ds = delta s
        dt = delta t
delta t = error (show t)

diff :: (Ord a, Show a) => Term (V.Index a) -> Term (V.Index a)
diff = reduceTerm . delta . energyToEnergy . expandTerm
