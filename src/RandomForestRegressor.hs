module RandomForestRegressor where

import System.Random
import DecisionTreeRegressor 
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Control.Parallel.Strategies

newtype AmountOfTrees = AmountOfTrees { unAmountOfTrees :: Int }

getSolutionSubset :: StdGen -> Vector Solution -> (StdGen, Vector Solution)
getSolutionSubset gen solutions =
  let amountOfSolutions = V.length solutions
      (genToUse, genToReturn) = splitGen gen
      solutionsToUseIndexes = take amountOfSolutions $ randomRs (0, amountOfSolutions - 1) genToUse
      solutionsToUse = V.fromList $ map (\ix -> solutions ! ix) solutionsToUseIndexes 
  in (genToReturn, solutionsToUse)

createTreeDataSets :: StdGen -> AmountOfTrees -> Vector Solution -> [(StdGen, Vector Solution)]
createTreeDataSets gen (AmountOfTrees aot) solutions = createTreeDataSetsHelper aot gen solutions []

createTreeDataSetsHelper :: Int -> StdGen -> Vector Solution -> [(StdGen ,Vector Solution)] -> [(StdGen, Vector Solution)]
createTreeDataSetsHelper 0 _ _ previous = previous
createTreeDataSetsHelper n gen solutions previous =
  let (genToUse, genForTuple) = splitGen gen
      (nextGen, nextSolution) = getSolutionSubset genToUse solutions
  in createTreeDataSetsHelper (n-1) nextGen solutions ((genForTuple, nextSolution) : previous)

createRandomForest :: AmountOfTrees -> NumOfFeatures -> MaximumLeafSize -> MaxDepth -> StdGen -> Vector Solution -> [Tree]
createRandomForest aot nof mls md gen solutions =
  let gensAndSolutions = createTreeDataSets gen aot solutions
      treeConstruction =  map (uncurry (constructTree nof mls md (CurrentDepth 0))) gensAndSolutions 
  in treeConstruction `using` parList rdeepseq

calculateVectorEstimate :: [Tree] -> Vector (Vector Double) -> Vector Double
calculateVectorEstimate trees solutions = V.map (calculateEstimate trees) solutions

calculateEstimate :: [Tree] -> Vector Double -> Double
calculateEstimate trees sol =
  let (sumOfValues, totalTrees) = foldr (\tree (sumOfVals, amount) -> (sumOfVals + calculateValue sol tree, amount + 1)) ((0.0, 0) :: (Double, Int)) trees
  in case totalTrees of
    0 -> 0.0
    _ -> sumOfValues / fromIntegral (totalTrees) 
