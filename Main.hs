{-# LANGUAGE TupleSections #-}
module Main where

import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (sort, sortOn, insert)
import Data.Tuple (swap)

type FreqMap = Map Char Int

type CodeMap = Map Char Code

data Bit = On | Off
  deriving Show

type Code = [Bit]

type Weight = Int

data HTree
  = Leaf Weight Char
  | Fork Weight HTree HTree
  deriving Eq

instance Ord HTree where
  compare x y = compare (weight x) (weight y)

weight :: HTree -> Int
weight htree = case htree of
  Leaf w _ -> w
  Fork w _ _ -> w

countFrequency :: String -> FreqMap
countFrequency = Map.fromListWith (+) . fmap (,1)

buildTree :: FreqMap -> HTree
buildTree = build . sort . fmap (\(c,w) -> Leaf w c) . Map.toList
  where
  build trees = case trees of
    [] -> error "empty trees"
    [x] -> x
    (x:y:rest) -> build $ insert (merge x y) rest

  merge x y = Fork (weight x + weight y) x y

buildCodes :: HTree -> CodeMap
buildCodes = Map.fromList . go []
  where
    go :: Code -> HTree -> [(Char, Code)]
    go prefix tree = case tree of
      Leaf _ char -> [(char, reverse prefix)]
      Fork _ left right ->
        go (On : prefix) left
        ++
        go (Off : prefix) right

encode :: String -> (HTree, [Bit])
encode str = (htree, encoded)
  where
  encoded = concatMap (codemap Map.!) str
  codemap = buildCodes htree
  htree = buildTree $ countFrequency str

decode :: HTree -> [Bit] -> String
decode codemap bits = go [] codemap bits
  where
    go acc tree xs = case (tree, xs) of
      (Leaf _ char, []) -> reverse (char : acc)
      (Fork{}, []) -> error "bad decoding"
      (Leaf _ char, rest) -> go (char:acc) codemap rest
      (Fork _ left _ , On  : rest) -> go acc left rest
      (Fork _ _ right, Off : rest) -> go acc right rest

compress :: FilePath -> FilePath -> IO ()
compress src _ =
  let
    (htree, encoded) = encode src
    decoded = decode htree encoded
  in
  do
    print encoded
    putStrLn decoded
    putStrLn $ "Success: " ++ show (decoded == src)

decompress :: FilePath -> FilePath -> IO ()
decompress _ _ = putStrLn "decompress"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["compress", src, dst] -> compress src dst
    ["decompress", src, dst] -> decompress src dst
    _ -> error $ unlines
      [ "Invalid arguments. Expected one of:"
      , "   compress FILE FILE"
      , "   decompress FILE FILE"
      ]
