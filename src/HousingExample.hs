{-# LANGUAGE OverloadedStrings #-}

module HousingExample where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Directory (doesFileExist)
import DecisionTreeRegressor
import RandomForestRegressor
import System.Random
import qualified VectorDecisionTreeRegressor as VDTR
import qualified VectorRandomForestRegressor as VRFR

data HousingPrice = HousingPrice
  { index :: Int,
    medInc :: Double,
    houseAge :: Double,
    aveRooms :: Double,
    aveBedrms :: Double,
    population :: Double,
    aveOccup :: Double,
    latitude :: Double,
    longitude :: Double,
    medHouseVal :: Double
  }
  deriving (Show, Eq)

instance FromNamedRecord HousingPrice where
  parseNamedRecord record =
    HousingPrice
      <$> record .: "Index"
      <*> record .: "MedInc"
      <*> record .: "HouseAge"
      <*> record .: "AveRooms"
      <*> record .: "AveBedrms"
      <*> record .: "Population"
      <*> record .: "AveOccup"
      <*> record .: "Latitude"
      <*> record .: "Longitude"
      <*> record .: "MedHouseVal"

type ErrorMsg = String
-- type synonym to handle CSV contents
type CsvData = (Header, V.Vector HousingPrice)

-- Function to read the CSV
parseCsv :: FilePath -> IO (Either ErrorMsg CsvData)
parseCsv filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then decodeByName <$> BL.readFile filePath
    else return $ Left "The file does not exist"

readDataAndCreateRandomForest :: FilePath -> IO ([Tree], Vector Solution)
readDataAndCreateRandomForest fp = do
  csv <- parseCsv fp
  case csv of
    Left err -> do
      putStrLn err
      return ([], V.empty)
    Right csvData -> do
      gen <- getStdGen
      return $ (createRandomForestFromCSV csvData gen, convertCsvDataToSolutions csvData) 

calculateTrees :: IO ()
calculateTrees = do
  (trees, solutions) <- readDataAndCreateRandomForest "housing.csv"
  let solutionEstimates = calculateVectorEstimate trees $ V.map values solutions
      solutionTargets = V.map target solutions
      zipped = V.toList $ V.zip solutionEstimates solutionTargets
  mapM_ (\toWrite -> appendFile "results" (show toWrite ++ "\n")) zipped

runModel :: IO ()
runModel = do
  trees <- createVectorRFR "housing.csv"
  mapM_ print trees

createVectorRFR :: FilePath -> IO [VDTR.Tree]
createVectorRFR fp = do
  csv <- parseCsv fp
  case csv of
    Left err -> do
      putStrLn err
      return []
    Right csvData -> do
      gen <- getStdGen
      pure $ createVectorRandomForestFromCSV csvData gen
 
createVectorRandomForestFromCSV :: CsvData -> StdGen -> [VDTR.Tree]
createVectorRandomForestFromCSV csvData gen =
  let (solutionsVector, indexVector, targetVector) = convertCsvDataToVectors csvData
  in VRFR.createRandomForest solutionsVector indexVector targetVector (VRFR.AmountOfTrees 8) (VDTR.AmountOfSolutions (VU.length (VDTR.unVectorIndexes indexVector)))  (VDTR.MaximumLeafSize 20)  (VDTR.AmountOfFeatures 8) (VDTR.NumOfFeatures 3) (VDTR.MaxDepth 10) gen

createRandomForestFromCSV :: CsvData -> StdGen -> [Tree] 
createRandomForestFromCSV csvData gen =
  let solutions = convertCsvDataToSolutions csvData
  in createRandomForest (AmountOfTrees 10) (NumOfFeatures 3) (MaximumLeafSize 20) (MaxDepth 10) gen solutions

convertCsvDataToSolutions :: CsvData -> Vector Solution
convertCsvDataToSolutions (_, housingPrices) = V.map convertHousingPriceToSolution housingPrices 

convertHousingPriceToSolution :: HousingPrice -> Solution
convertHousingPriceToSolution hp =
  Solution (V.fromList [medInc hp,
    houseAge hp,
    aveRooms hp,
    aveBedrms hp,
    population hp,
    aveOccup hp,
    latitude hp,
    longitude hp]) (medHouseVal hp)

convertCsvDataToVectors :: CsvData -> (VDTR.VectorSolutions, VDTR.VectorIndexes, VDTR.VectorTargets)
convertCsvDataToVectors (_, housingPrices) = 
  let targetVector = VDTR.VectorTargets $ VU.fromList $ map (\hp -> medHouseVal hp) $ V.toList housingPrices
      indexVector = VDTR.VectorIndexes $ VU.generate (VU.length (VDTR.unVectorTargets targetVector)) id 
      solutionsVector = VDTR.VectorSolutions $ VU.concat [
                        VU.fromList $ map medInc $ V.toList housingPrices, 
                        VU.fromList $ map houseAge $ V.toList housingPrices, 
                        VU.fromList $ map aveRooms $ V.toList housingPrices, 
                        VU.fromList $ map aveBedrms $ V.toList housingPrices, 
                        VU.fromList $ map population $ V.toList housingPrices, 
                        VU.fromList $ map aveOccup $ V.toList housingPrices, 
                        VU.fromList $ map latitude $ V.toList housingPrices, 
                        VU.fromList $ map longitude $ V.toList housingPrices] 

  in (solutionsVector, indexVector, targetVector)
