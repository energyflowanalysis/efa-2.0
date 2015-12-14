module EFA.Equation.Unknown where

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Mix as Mix
import EFA.Equation.Result (Result(Undetermined))

import qualified Type.Data.Num.Unary as Unary

import Control.Applicative (Applicative, pure)


class Unknown a where
   unknown :: a


instance Unknown (Result a) where
   unknown = Undetermined

instance Unknown a => Unknown (Record.Absolute a) where
   unknown = pure unknown

instance Unknown a => Unknown (Record.Delta a) where
   unknown = pure unknown

instance (Applicative rec, Unknown a) => Unknown (Record.ExtDelta rec a) where
   unknown = pure unknown

instance
   (Mix.Direction dir, Unary.Natural n, Unknown a) =>
      Unknown (Record.Mix dir n a) where
   unknown = pure unknown

instance
   (Mix.Direction dir, Unary.Natural n, Applicative rec, Unknown a) =>
      Unknown (Record.ExtMix dir n rec a) where
   unknown = pure unknown
