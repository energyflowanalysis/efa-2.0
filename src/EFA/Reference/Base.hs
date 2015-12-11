{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module EFA.Reference.Base where

import EFA.Utility(Caller,
                   -- merror,(|>),
                   ModuleName(..),FunctionName, genCaller)

import qualified System.Path as Path

import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.List as List
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV


modul :: ModuleName
modul = ModuleName "Test.Reference.Base"

nc :: FunctionName -> Caller
nc = genCaller modul

{-
tmpFolder :: DirPath Abs
tmpFolder p = p +++ DirPath [Directory "tmp"]


genTestPath ::  Group -> Test -> DirPath Rel
genTestPath (Test name) = DirPath fromString $ filename name
-}
newtype Ref a = Ref a

type Type = String
type Value = String
type Name = String

data Test = Test Path.RelDir (Map.Map Path.RelFile Data) deriving (Eq,Show)

data Data = DataMap Type (Map.Map String Data)
            | StringData Type String
            | SingleType Type Data  
            | DoubleType Type Data Data  
            | TripleType Type Data Data  Data
            | QuadrupleType Type Data  Data  Data Data
            | NumData Type Double deriving (Eq,Show,Read)

-- | A Class to convert a Datatype to its Reference Cousin

class ToData a where
  toData :: a -> Data

instance ToData Double where
  toData x = NumData "Double" x

instance ToData Int where
  toData x = StringData "Int" $ show x

instance ToData Integer where
  toData x = StringData "Integer" $ show x

instance (ToData a, ToData b) => ToData (a,b) where
  toData (x,y) = DataMap "(,)" $ list2Map [toData x,toData y]

instance (ToData a, ToData b, ToData c) => ToData (a,b,c) where
  toData (x,y,z) = DataMap "(,,)" $ list2Map [toData x,toData y,toData z]

instance ToData [Char] where
  toData x = StringData "String" x

instance ToData (Ratio.Ratio Integer) where
  toData x = StringData "Ratio" (show x)

list2Map :: [a] -> Map.Map String a
list2Map xs = (Map.fromList $ zip (map show [(0::Integer)..]) xs)

instance (ToData a) => ToData [a] where
  toData xs = DataMap "List" $ list2Map $ map toData xs

instance (Show a, ToData b) => ToData (Map.Map a b) where
  toData m = DataMap "Data.Map" $ Map.mapKeys show $ Map.map toData m

instance (ToData a) => ToData (V.Vector a) where
  toData vec = DataMap "Data.Vector" $ list2Map $ map toData $ V.toList vec

instance (ToData a,UV.Unbox a) => ToData (UV.Vector a) where
  toData vec =  DataMap "Data.Vector.Unboxed" $ list2Map $ map toData $ UV.toList vec

-- checkVersusRef:: (Eq a) => Ref a -> a -> Bool
-- checkVersusRef (Ref x)  x1 = x==x1


-- diffTest :: Ref (Test) -> Test -> String
-- diffTest (Ref (Test label m)) (Test label1 m1) = if label == label1 && m == m1 then "Passed" else "NOT PASSED"


diff :: String -> Ref Data -> Data -> String

diff label (Ref (DataMap rt rv)) (DataMap t v) = label ++": " ++ result ++ "\n"
  where result = case (rt==t,rv==v) of
          (True,True) -> "OK"
          (False,True) -> "Types Differ - Ref:"  ++ show rt ++ "Test: " ++ show t
          (True,False) -> "Values Differ - Ref: \n " ++ diffMap (Ref rv) v
          (False,False) -> "Types and Values Differ - Types: Ref:" ++ show rt ++ "Test: " ++ show t ++
                           diffMap (Ref rv) v

diff label (Ref (StringData rt rv)) (StringData t v) = label ++": " ++ result ++ "\n"
  where result = case (rt==t,rv==v) of
          (True,True) -> "OK"
          (False,True) -> "Types Differ - Ref:"  ++ show rt ++ "Test: " ++ show t
          (True,False) -> "Values Differ - Ref: " ++ show rv ++ "Test: " ++ show v
          (False,False) -> "Types and Values Differ - Types: Ref:" ++ show rt ++ "Test: " ++ show t ++
                        "Value: Ref: " ++ show rv ++ "Test: " ++ show v

diff label (Ref (NumData rt rv)) (NumData t v) = label ++": " ++ result ++ "\n"
  where
    result = case (rt==t,rv==v) of
      (True,True) -> "OK"
      (False,True) -> "Types Differ - Ref:"  ++ show rt ++ "Test: " ++ show t
      (True,False) -> "Values Differ - Ref: " ++ show rv ++ "Test: " ++ show v
      (False,False) -> "Types and Values Differ - Types: Ref:" ++ show rt ++ "Test: " ++ show t ++
                    "Value: Ref: " ++ show rv ++ "Test: " ++ show v

diff _ _ _ = "Ref Data Types don't match \n"


diffMap :: Ref (Map.Map String Data) -> Map.Map String Data -> String
diffMap  r n =
  diffCommons r n ++
  showMissing r n ++ "\n" ++
  showNew  r n ++ "\n"

diffCommons :: Ref (Map.Map String Data) -> Map.Map String Data -> String
diffCommons (Ref rm) m =
  List.intercalate "\n" $
  zipWith f (Map.toList rm) (Map.elems m)
  where f (label,x) y = diff label (Ref x) y

showMissing :: Ref (Map.Map String Data) -> Map.Map String Data -> String
showMissing (Ref rv) v  =
  "Missing Elemets: \n" ++ (List.intercalate "\n" $ map show $ Map.toList $ Map.difference rv v)

showNew ::Ref (Map.Map String Data) -> Map.Map String Data -> String
showNew  (Ref rv) v =
  "New Elements: \n" ++ (List.intercalate "\n" $ map show $ Map.toList $ Map.difference v rv)

