{-# LANGUAGE DeriveGeneric #-}

module VectorDecisionTreeRegressor where

import GHC.Generics (Generic)
import System.Random
import System.Random.Shuffle
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Either
import Control.Parallel.Strategies

newtype VectorSolutions = VectorSolutions { unVectorSolutions :: Vector Double}

newtype VectorIndexes = VectorIndexes { unVectorIndexes :: Vector Int }
  deriving (Show,Generic)

instance NFData VectorIndexes

newtype VectorTargets = VectorTargets { unVectorTargets :: Vector Double }
 
newtype PartitionValue = PartitionValue { unPartitionValue :: Double }
  deriving (Show, Eq, Ord, Generic)

instance NFData PartitionValue

newtype PartitionIndex = PartitionIndex { unPartitionIndex :: Int }  
  deriving (Show, Eq, Ord, Generic) 

instance NFData PartitionIndex

newtype AmountOfSolutions = AmountOfSolutions { unAmountOfSolutions :: Int }

data Tree = Node Tree PartitionIndex PartitionValue Tree | Leaf VectorIndexes Double
  deriving (Show,Generic)

instance NFData Tree

newtype MaximumLeafSize = MaximumLeafSize { unMaximumLeafSize :: Int }

newtype AmountOfFeatures = AmountOfFeatures { unAmountOfFeatures :: Int }

newtype NumOfFeatures = NumOfFeatures { unNumOfFeatures :: Int } 

newtype MaxDepth = MaxDepth { unMaxDepth :: Int }

newtype CurrentDepth = CurrentDepth { unCurrentDepth :: Int }

constructTree :: VectorSolutions -> VectorTargets -> AmountOfSolutions -> MaximumLeafSize -> AmountOfFeatures -> NumOfFeatures -> MaxDepth -> CurrentDepth -> StdGen -> VectorIndexes -> Tree
constructTree vs vt aos mls aof nof md cd gen vi =
  if unMaxDepth md == unCurrentDepth cd
    then Leaf vi (calculateRegression vt vi)
    else case createBestSplit gen nof aof aos mls vs vi vt of
      (_, leaf@(Leaf _ _)) -> leaf
      (nextGen, Node (Leaf l _) i v (Leaf r _)) ->
        let (genLeft, genRight) = splitGen nextGen
            left = constructTree vs vt aos mls aof nof md (CurrentDepth (unCurrentDepth cd + 1)) genLeft l
            right = constructTree vs vt aos mls aof nof md (CurrentDepth (unCurrentDepth cd + 1)) genRight r
        in Node left i v right

calculateValue :: VectorSolutions -> AmountOfSolutions -> Int -> Tree -> Double
calculateValue (VectorSolutions vs) (AmountOfSolutions aos) ix tree  = 
  case tree of
    Leaf _ avg -> avg
    Node l (PartitionIndex pix) (PartitionValue pval) r ->
       if vs ! (aos * pix + ix) < pval
         then calculateValue (VectorSolutions vs) (AmountOfSolutions aos) ix l
         else calculateValue (VectorSolutions vs) (AmountOfSolutions aos) ix r

calculateRegression :: VectorTargets -> VectorIndexes -> Double
calculateRegression (VectorTargets vt) (VectorIndexes vi) =
  if V.length vi == 0
    then 0.0
    else (V.foldl' (\sum ix -> sum + vt ! ix) 0.0 vi) / fromIntegral (V.length vi)

{-# INLINE calculateRegression #-}

createBestSplit :: StdGen -> NumOfFeatures -> AmountOfFeatures -> AmountOfSolutions -> MaximumLeafSize -> VectorSolutions -> VectorIndexes -> VectorTargets -> (StdGen, Tree)
createBestSplit gen nof aof aos mls vs vi vt = 
  let (nextGen, residuals) = calculateResidualsForSomeSplits gen nof aof aos vs vi vt
      (smallestIx, smallestVal) = findSmallestResidual residuals
  in if V.length (unVectorIndexes vi) < unMaximumLeafSize mls
       then (nextGen,  Leaf vi (calculateRegression vt vi))
       else (nextGen, partitionTree vt vs vi aos smallestVal smallestIx )
      
findSmallestResidual :: [(PartitionIndex, PartitionValue, Double)] -> (PartitionIndex, PartitionValue)
findSmallestResidual (x:xs) = findSmallestResidualHelper xs x 

findSmallestResidualHelper :: [(PartitionIndex, PartitionValue, Double)] -> (PartitionIndex, PartitionValue, Double) -> (PartitionIndex, PartitionValue)
findSmallestResidualHelper [] (pix, pval, _) = (pix, pval) 
findSmallestResidualHelper ((pix, pval, res) : xs) (bix, bpval, bres) =  
  if res < bres
    then findSmallestResidualHelper xs (pix, pval, res)
    else findSmallestResidualHelper xs (bix, bpval, bres)

calculateResidualsForSomeSplits :: StdGen -> NumOfFeatures -> AmountOfFeatures -> AmountOfSolutions -> VectorSolutions -> VectorIndexes -> VectorTargets ->  (StdGen, [(PartitionIndex, PartitionValue,  Double)])
calculateResidualsForSomeSplits gen nof aof aos vs vi vt = 
  let (nextGen, somePartitionValues) = getSomePartitionValues gen nof aof aos vs vi
      residuals = map (\(pix, pv) -> (pix, pv, getResidualForSplit vs vi vt aos pix pv)) $ concatMap combineValues $ Map.toList somePartitionValues
  in (nextGen, residuals)

combineValues :: (PartitionIndex, [PartitionValue]) -> [(PartitionIndex, PartitionValue)]
combineValues (pix, pvals)= map (\pv -> (pix, pv)) pvals 

getSomePartitionValues :: StdGen -> NumOfFeatures -> AmountOfFeatures -> AmountOfSolutions -> VectorSolutions -> VectorIndexes -> (StdGen, Map PartitionIndex [PartitionValue])
getSomePartitionValues gen nof aof aos vs vi =
  let (nextGen, partIndexes) = selectFeatures gen nof aof
  in (nextGen, Map.fromList $ map (\ pix -> (pix, getPartitionValuesForIndex vs vi aos pix) ) partIndexes)

selectFeatures :: StdGen -> NumOfFeatures -> AmountOfFeatures -> (StdGen, [PartitionIndex])
selectFeatures gen (NumOfFeatures nof) (AmountOfFeatures aof) = 
  let (nextGen, genToUse) = splitGen gen
  in (nextGen, map PartitionIndex $ take nof $ shuffle' [0..aof - 1] aof genToUse)

getPartitionValuesForIndex :: VectorSolutions -> VectorIndexes -> AmountOfSolutions -> PartitionIndex -> [PartitionValue]
getPartitionValuesForIndex (VectorSolutions vs) (VectorIndexes vi) (AmountOfSolutions aos) (PartitionIndex pix) = flip getMiddlePoints [] $ Set.toList $ Set.fromList $ map (\ix -> PartitionValue (vs ! (aos * pix + ix))) $ V.toList vi

getMiddlePoints :: [PartitionValue] -> [PartitionValue] -> [PartitionValue]
getMiddlePoints listOfValues previous =
  case listOfValues of
    [] -> previous 
    x:[] -> previous 
    x:y:xs -> getMiddlePoints (y:xs) $ (PartitionValue ((unPartitionValue x+ unPartitionValue y)/2.0) ) : previous

getResidualForSplit :: VectorSolutions -> VectorIndexes -> VectorTargets -> AmountOfSolutions -> PartitionIndex -> PartitionValue -> Double
getResidualForSplit vs vi vt aos pix pv =
  let tree = partitionTree vt vs vi aos pv pix
  in calculateResiduals vt tree

calculateResiduals :: VectorTargets -> Tree -> Double
calculateResiduals vt (Node left _ _ right) = calculateResiduals vt left + calculateResiduals vt right
calculateResiduals (VectorTargets vt) (Leaf indexes average) = V.foldl' (\sum ix -> sum + (vt ! ix - average) ^ (2 :: Int)) 0.0 (unVectorIndexes indexes )
--V.sum $ V.map (\ix -> (vt ! ix - average) ^ (2 :: Int)) (unVectorIndexes indexes )
{-# INLINE calculateResiduals #-}

partitionTree :: VectorTargets -> VectorSolutions -> VectorIndexes -> AmountOfSolutions -> PartitionValue -> PartitionIndex -> Tree
partitionTree vt vs vi aos pv pix =
  let (leftLeaf, rightLeaf) = partitionSolutions vs vi aos pv pix
  in Node (Leaf leftLeaf (calculateRegression vt leftLeaf)) pix pv (Leaf rightLeaf (calculateRegression vt rightLeaf))

partitionSolutions :: VectorSolutions -> VectorIndexes -> AmountOfSolutions -> PartitionValue -> PartitionIndex -> (VectorIndexes, VectorIndexes)
partitionSolutions vs vi aos pv pix = 
  let (rightVector, leftVector) = V.unstablePartition (\ix -> partitionSolutionsHelper vs ix aos pv pix) $ unVectorIndexes vi
  in (VectorIndexes leftVector, VectorIndexes rightVector) 

partitionSolutionsHelper :: VectorSolutions -> Int -> AmountOfSolutions -> PartitionValue -> PartitionIndex -> Bool 
partitionSolutionsHelper (VectorSolutions vs) ix (AmountOfSolutions aos) (PartitionValue pv) (PartitionIndex pix) =
  vs ! (aos * pix + ix) < pv

{-# INLINE partitionSolutionsHelper #-}

