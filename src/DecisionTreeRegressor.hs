{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module DecisionTreeRegressor where

import GHC.Generics (Generic)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Either
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random
import System.Random.Shuffle
import Control.Parallel.Strategies

data Solution = Solution { values :: Vector Double, target :: Double }
  deriving (Show, Generic)

instance NFData Solution

data Tree = Node Tree PartitionIndex PartitionValue Tree | Leaf (Vector Solution) Double
  deriving (Show, Generic)

instance NFData Tree

newtype PartitionIndex = PartitionIndex { unPartitionIndex :: Int }
  deriving (Eq, Ord, Show, Generic)

instance NFData PartitionIndex

newtype PartitionValue = PartitionValue { unPartitionValue :: Double } 
  deriving (Eq, Ord, Show, Generic)

instance NFData PartitionValue

newtype MaximumLeafSize = MaximumLeafSize { unMaximumLeafSize :: Int }

newtype MaxDepth = MaxDepth { unMaxDepth :: Int }

newtype CurrentDepth = CurrentDepth { unCurrentDepth :: Int }

newtype NumOfFeatures = NumOfFeatures { unNumberOfFeatures :: Int }

constructTree :: NumOfFeatures -> MaximumLeafSize -> MaxDepth -> CurrentDepth -> StdGen -> Vector Solution -> Tree
constructTree nof mls maxDepth (CurrentDepth cd) gen solutions =
  if unMaxDepth maxDepth == cd
    then Leaf solutions (calculateRegression solutions)
    else case createBestSplit gen nof mls solutions of
      (_, leaf@(Leaf _ _)) -> leaf
      (nextGen, Node (Leaf l _) i v (Leaf r _)) -> 
        let (genLeft, genRight) = splitGen nextGen 
            leftSide = constructTree nof mls maxDepth (CurrentDepth (cd + 1)) genLeft l
            rightSide = constructTree nof mls maxDepth (CurrentDepth (cd + 1)) genRight r
        in Node leftSide i v rightSide

calculateValue :: Vector Double -> Tree -> Double
calculateValue _ (Leaf _ average) = average 
calculateValue solution (Node left index partitionValue right) =
  if solution ! unPartitionIndex index < unPartitionValue partitionValue
    then calculateValue solution left
    else calculateValue solution right

calculateRegression :: Vector Solution -> Double
calculateRegression solutions  = 
  let targetValues = map target $ V.toList solutions 
      (totalSum, totalItems) = foldr (\value (sumOfValues, total) -> (sumOfValues + value, total + 1)) ((0.0, 0) :: (Double, Int)) targetValues
  in case totalItems of
    0 -> 0.0
    _ -> totalSum / fromIntegral totalItems 

createBestSplit :: StdGen -> NumOfFeatures -> MaximumLeafSize -> Vector Solution -> (StdGen, Tree)
createBestSplit gen nof (MaximumLeafSize mls) solutions = 
  if ((V.length solutions) < mls)
    then (gen, Leaf solutions (calculateRegression solutions))
    else let (nextGen, allResiduals) = calculateAllResidualsForSplits gen nof solutions
         in if Map.null allResiduals
              then (nextGen, Leaf solutions (calculateRegression solutions))
              else case findSmallest allResiduals of
                     Nothing -> (nextGen, Leaf solutions (calculateRegression solutions))
                     Just (pix, pVal) -> (nextGen, partitionTree solutions pix pVal)

findSmallest :: Map (PartitionIndex, PartitionValue) Double -> Maybe (PartitionIndex, PartitionValue)
findSmallest mapOfValues = 
  case Map.assocs mapOfValues of
    [] -> Nothing
    (x:xs) -> Just $ fst $ foldr (\((pix, pVal), res) (bestPart, bestRes) -> if res < bestRes then ((pix, pVal), res) else (bestPart, bestRes)) x xs

calculateAllResidualsForSplits :: StdGen -> NumOfFeatures -> Vector Solution -> (StdGen, Map (PartitionIndex, PartitionValue) Double)
calculateAllResidualsForSplits gen nof solutions =
  let (nextGen, allPartitionValues) = getSomePartitionValues gen nof solutions
      allPartitionTuples = concatMap extractAllToTuple $ Map.toList allPartitionValues
      residualOperation = map (\(pix, pVal) -> ((pix, pVal), getResidualForSplit solutions pix pVal)) allPartitionTuples
      results = residualOperation `using` parList rdeepseq 
  in (nextGen, Map.fromList results)

extractAllToTuple :: (PartitionIndex, Set PartitionValue) -> [(PartitionIndex, PartitionValue)]
extractAllToTuple (pix, pvSet) = zip (repeat pix) (Set.toList pvSet)

getSomePartitionValues :: StdGen -> NumOfFeatures -> Vector Solution -> (StdGen, Map PartitionIndex (Set PartitionValue))
getSomePartitionValues gen nof solutions =
  if V.length solutions == 0
    then (gen, Map.empty)
    else let 
             (nextGen, somePartitionIndexes) = selectFeatures gen nof $ solutions ! 0
             somePartitionValues = map (\pix -> (pix, getPartitionValues solutions pix)) somePartitionIndexes
         in (nextGen, Map.fromList somePartitionValues)

selectFeatures :: StdGen -> NumOfFeatures -> Solution -> (StdGen, [PartitionIndex])
selectFeatures gen (NumOfFeatures nof) sol = 
  let (nextGen, genToUse) = splitGen gen
  in (nextGen, map PartitionIndex $ take nof $ shuffle' [0..V.length (values sol) - 1] (V.length (values sol)) genToUse)

getPartitionValues :: Vector Solution -> PartitionIndex -> Set PartitionValue 
getPartitionValues solutions (PartitionIndex partitionIndex) =
  let initialSet = Set.fromList $ map (\vector -> values vector ! partitionIndex) $ V.toList solutions 
      uniquesAsList = Set.toList initialSet 
  in getPartitionValuesHelper uniquesAsList Set.empty 

getPartitionValuesHelper :: [Double] -> Set PartitionValue -> Set PartitionValue 
getPartitionValuesHelper [] previous = previous
getPartitionValuesHelper (_:[]) previous = previous
getPartitionValuesHelper (x:y:xs) previous =
  getPartitionValuesHelper (y:xs) $ Set.insert (PartitionValue ((x + y) / 2.0)) previous

getResidualForSplit :: Vector Solution -> PartitionIndex -> PartitionValue -> Double
getResidualForSplit solutions partitionIndex partitionValue =
  let newLeaf = partitionTree solutions partitionIndex partitionValue
  in calculateResiduals newLeaf

calculateResiduals :: Tree -> Double
calculateResiduals (Node left _ _ right) = calculateResiduals left + calculateResiduals right
calculateResiduals (Leaf solutions value) = V.sum $ V.map (\sol -> (target sol - value) ^ (2 :: Int)) solutions 

partitionTree :: Vector Solution -> PartitionIndex -> PartitionValue -> Tree
partitionTree solutions indexToPartition partitionValue =
  let (leftLeaf, rightLeaf) = (\(l,r) -> (V.fromList l, V.fromList r)) $ partitionSolutions solutions partitionValue indexToPartition
  in Node (Leaf leftLeaf (calculateRegression leftLeaf)) indexToPartition partitionValue (Leaf rightLeaf (calculateRegression rightLeaf))

partitionSolutions :: Vector Solution -> PartitionValue -> PartitionIndex -> ([Solution], [Solution])
partitionSolutions solutions partitionValue indexToPartition = 
  partitionEithers $ map (partitionHelper partitionValue indexToPartition) $ V.toList solutions 

partitionHelper :: PartitionValue -> PartitionIndex -> Solution -> Either Solution Solution
partitionHelper (PartitionValue partitionValue) (PartitionIndex indexToPartition) solutionToPartition = 
  if values solutionToPartition ! indexToPartition < partitionValue
    then Left solutionToPartition
    else Right solutionToPartition

