module Main where

import System.Environment
import Prelude hiding (union)
import Data.Graph
import Control.Monad
import Data.Function
import Data.Monoid
import Data.List hiding (union)
import Data.Set hiding (filter, take, drop)

data Orientation = H | V deriving (Show, Eq)

data Photo =
  Photo Int Orientation (Set String)

instance Eq Photo where
  (==) (Photo id _ _) (Photo id1 _ _) = id == id1

instance Show Photo where
  show (Photo id _ _) = show id

data Slide =
  Ver Int Int (Set String)
  | Hor Int (Set String)

instance Eq Slide where
  (==) (Ver a b _) (Ver c d _) = a == c && b == d
  (==) (Hor a _) (Hor b _) = a == b
  (==) _ _  = False

instance Show Slide where
  show (Ver p1 p2 _) = intercalate " " (fmap show [p1, p2])
  show (Hor p _) = show p

getPhoto :: Int -> IO Photo
getPhoto id = parsePhoto id <$> getLine
  where
    parsePhoto id ('H'  : rest) = Photo id H $ parseTags rest
    parsePhoto id ('V'  : rest) = Photo id V $ parseTags rest
    parseTags = fromList . tail . words

isV (Photo _ V _) = True
isV _ = False

pairs (x:y:xs) = (x, y) : pairs xs
pairs _ = []

slideTags (Hor _ tags) = tags
slideTags (Ver _ _ tags) = tags

sortByTagAmt = sortBy (\a b -> compare (length $ slideTags a) (length $ slideTags b))

hor (Photo id _ tags) = Hor id tags
ver (Photo id _ tags) (Photo id2 _ tags2) = Ver id id2 (union tags tags2)

gibSlides vs hs =
  (fmap hor hs) <> (uncurry ver <$> pairs vs)

scoreTransition a b =
  let [ta, tb] = fmap slideTags [a, b]
  in minimum $ length <$> [intersection ta tb, difference ta tb, difference tb ta]

greedyPair sf score slide best [] = best
greedyPair sf score slide best (curr : rest)
  = let ns = sf slide curr in
    case ns > score of
      True -> greedyPair sf ns slide curr rest
      False -> greedyPair sf score slide best rest

chunksOf a [] = []
chunksOf a lst = take a lst : chunksOf a (drop a lst)

scoreVerPair (Photo _ _ tags) (Photo _ _ tags1) =
  0 - length (intersection tags tags1)

optimisePairs cmp (x : y : xs) =
  let matching = greedyPair cmp (cmp x y) x y xs in
    x : matching : optimisePairs cmp (filter (/= matching) (y : xs))
optimisePairs _ a = a

chunkedOptimisePairs scorer chunkSize lst =
  concat (fmap (optimisePairs scorer) $ chunksOf chunkSize lst)

main :: IO ()
main = do
  [slideChunkSize , verChunkSize] <- fmap (fmap (read :: String -> Int)) getArgs
  lines <- fmap read getLine
  photos <- forM [0..lines - 1] getPhoto
  let (vs, hs) = Data.List.partition isV photos
  let betterVerts = chunkedOptimisePairs scoreVerPair verChunkSize vs
  let slides = sortByTagAmt $ gibSlides betterVerts hs
  let result = chunkedOptimisePairs scoreTransition slideChunkSize slides
  putStrLn $ show $ length result
  forM_ result $ putStrLn . show
