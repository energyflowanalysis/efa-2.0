module EFA.Example.Record.SourceSinkMix where

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record

import Type.Data.Num.Unary.Literal (U1)

import qualified Data.FixedLength as FL



type MultiMix = Record.ExtSourceMix U1 (Record.SinkMix U1)

idxMixTotal :: RecIdx.Mix dir pos
idxMixTotal = RecIdx.MixTotal

idxMix0 :: RecIdx.Mix dir (FL.Index (FL.GE1 n))
idxMix0 = RecIdx.MixComponent FL.i0

idxMix1 :: RecIdx.Mix dir (FL.Index (FL.GE2 n))
idxMix1 = RecIdx.MixComponent FL.i1

idxMultiMix ::
   RecIdx.SourceMix pos0 -> RecIdx.SinkMix pos1 -> idx ->
   RecIdx.Record (RecIdx.ExtSourceMix pos0 (RecIdx.SinkMix pos1)) idx
idxMultiMix a b =
   RecIdx.Record (RecIdx.ExtMix a b)

idxMultiMixTotal ::
   idx -> RecIdx.Record (RecIdx.ExtSourceMix pos0 (RecIdx.SinkMix pos1)) idx
idxMultiMixTotal = idxMultiMix idxMixTotal idxMixTotal
