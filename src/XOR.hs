{-# LANGUAGE BangPatterns #-}
module XOR where

import Neet
import Neet.Species

import Data.Monoid

import qualified Data.Map as Mp
import qualified Data.List as L
import Flow

import System.Random
import Data.Functor.Identity

import Data.List (intercalate)


allQuestions :: [[Double]]
allQuestions =
    [[a, b, c, d, e, f, g, h]
    | a <- [0,1]
    , b <- [0,1]
    , c <- [1,0]
    , d <- [0,1]
    , e <- [1,0]
    , f <- [1,0]
    , g <- [0,1]
    , h <- [0,1]
    ]

data Config = Config
    { inputSize     :: Int
    , outputSize    :: Int
    , populationSize:: Int
    }

createEvaluator :: GenScorer Double
createEvaluator =
    GS evaluate id (const False)

initPop :: IO Population
initPop = do
    seed <- randomIO
    return $ newPop seed (PS 150 8 1 defParams Nothing Nothing)

train = trainOnce (pureTrain createEvaluator)

runTest :: Int -> Population -> IO Population
runTest 0 p = return p
runTest n pop = 
  let
    !newPop = runIdentity $ train pop
  in
    do
      print (n, length $ getAllIndividuals newPop)
      runTest (n-1) newPop


expectedAnswer :: [Double] -> Bool
expectedAnswer [] = True
expectedAnswer (i1:is) = xor (booleanize i1) (expectedAnswer is)
    where
        booleanize d = d /= 0
        xor True a = not a
        xor False a = a

try :: Genome -> [Double] -> Double
try g sample = head $ pushThrough net sample
    where
        net = mkPhenotype g

evaluate :: Genome -> Double
evaluate g =
    tail allQuestions
        |> map (\q -> (try g q, expectedAnswer q))
        |> map (\(a, expAns) -> if expAns then abs(1-a) else a)
        |> (negate . sum)

getAllIndividuals :: Population -> [Genome]
getAllIndividuals p =
    popSpecs p
        |> Mp.elems
        |> concatMap specOrgs

bigXorTest :: IO ()
bigXorTest = do
    print $ head allQuestions
    pop <- initPop
    bestPop <- runTest 100 pop
    print $ try (bestG bestPop) $ head allQuestions
    where
        bestG pop = getAllIndividuals pop
            |> map (\g -> (evaluate g, g))
            |> L.sortOn fst
            |> L.last
            |> snd
