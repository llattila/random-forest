module VectorRandomForestRegressor where

import System.Random
import VectorDecisionTreeRegressor 
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import Control.Parallel.Strategies

newtype AmountOfTrees = AmountOfTrees { unAmountOfTrees :: Int }

getSolutionSubset :: StdGen -> VectorIndexes -> (StdGen, VectorIndexes)
getSolutionSubset gen solutions =
  let amountOfSolutions = V.length (unVectorIndexes solutions)
      (genToUse, genToReturn) = splitGen gen
      solutionsToUseIndexes = VectorIndexes $ V.fromList $ take amountOfSolutions $ randomRs (0, amountOfSolutions - 1) genToUse
  in (genToReturn, solutionsToUseIndexes)

createTreeDataSets :: StdGen -> AmountOfTrees -> VectorIndexes -> [(StdGen, VectorIndexes)]
createTreeDataSets gen (AmountOfTrees aot) solutions = createTreeDataSetsHelper aot gen solutions []

createTreeDataSetsHelper :: Int -> StdGen -> VectorIndexes -> [(StdGen ,VectorIndexes)] -> [(StdGen, VectorIndexes)]
createTreeDataSetsHelper 0 _ _ previous = previous
createTreeDataSetsHelper n gen solutions previous =
  let (genToUse, genForTuple) = splitGen gen
      (nextGen, nextSolution) = getSolutionSubset genToUse solutions
  in createTreeDataSetsHelper (n-1) nextGen solutions ((genForTuple, nextSolution) : previous)

createRandomForest ::  VectorSolutions -> VectorIndexes -> VectorTargets -> AmountOfTrees -> AmountOfSolutions -> MaximumLeafSize -> AmountOfFeatures -> NumOfFeatures -> MaxDepth -> StdGen -> [Tree]
createRandomForest vs vi vt aot aos mls aof nof md gen =
  let gensAndSolutions = createTreeDataSets gen aot vi 
      treeConstruction =  map (uncurry (constructTree vs vt aos mls aof nof md (CurrentDepth 0))) gensAndSolutions 
  in treeConstruction `using` parList rdeepseq

calculateVectorEstimate :: [Tree] -> VectorSolutions -> AmountOfSolutions -> Vector Double
calculateVectorEstimate trees solutions aos = V.map (calculateEstimate trees solutions aos) $ V.fromList $ [0..unAmountOfSolutions aos - 1] 

calculateEstimate :: [Tree] -> VectorSolutions -> AmountOfSolutions -> Int -> Double
calculateEstimate trees vs aos ix =
  let (sumOfValues, totalTrees) = foldr (\tree (sumOfVals, amount) -> (sumOfVals + calculateValue vs aos ix tree, amount + 1)) ((0.0, 0) :: (Double, Int)) trees
  in case totalTrees of
    0 -> 0.0
    _ -> sumOfValues / fromIntegral (totalTrees) 
