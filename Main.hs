{-# LANGUAGE TupleSections #-}
module Main where

import System.Environment (getArgs)
import Control.Monad (replicateM, forM_)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (sort, insert)
import Data.Word (Word8)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import Data.Binary.Put (Put)
import qualified Data.Binary.Put as Put
import Data.Bits (shiftR)

type FreqMap = Map Char Int

type CodeMap = Map Char Code

data Bit = One | Zero
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
        go (One : prefix) left
        ++
        go (Zero : prefix) right

encode :: String -> (FreqMap, [Bit])
encode str = (freqMap, encoded)
  where
  freqMap = countFrequency str
  codemap = buildCodes $ buildTree freqMap
  encoded = concatMap (codemap Map.!) str

serialize :: FreqMap -> [Bit] -> Builder
serialize freqmap bytes = Put.execPut $ do
  serializeFreqMap freqmap
  Put.putWord8 (fromIntegral paddingLen)
  foldMap serializeByte $ chunksOf 8 (padding ++ bytes)
  where
  paddingLen = 8 - (length bytes `mod` 8)
  padding = replicate paddingLen Zero

  -- takes a list with 8 bits
  serializeByte :: [Bit] -> Put
  serializeByte bs = go 0 bs
    where
    go :: Word8 -> [Bit] -> Put
    go acc bits = case bits of
      [] -> Put.putWord8 acc
      One : rest -> go (acc * 2 + 1) rest
      Zero: rest -> go (acc * 2) rest

-- | Split a list in chunks
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
 where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n = l `c` splitter (drop i l) c n
  build g = g (:) []

serializeFreqMap :: FreqMap -> Put
serializeFreqMap freqMap = do
  Put.putInt32be $ fromIntegral $ Map.size freqMap
  forM_ (Map.toList freqMap) $ \(char, freq) -> do
    Put.putWord8 $ c2w char
    Put.putInt32be $ fromIntegral freq

deserializeFreqMap :: Get FreqMap
deserializeFreqMap = do
  len <- Get.getInt32be
  entries <- replicateM (fromIntegral len) $ do
    char <- w2c <$> Get.getWord8
    freq <- Get.getInt32be
    return (char, fromIntegral freq)
  return $ Map.fromList entries

deserialize :: ByteString -> String
deserialize bs = flip Get.runGet bs $ do
  freqMap <- deserializeFreqMap
  padding <- fromIntegral <$> Get.getWord8
  word8s <- BS.unpack <$> Get.getRemainingLazyByteString
  let bits = concatMap toBits word8s
  return $ decode freqMap $ drop padding bits
  where
    toBits :: Word8 -> [Bit]
    toBits w = snd
      $ getBit
      $ getBit
      $ getBit
      $ getBit
      $ getBit
      $ getBit
      $ getBit
      $ getBit (w, [])
      where
        getBit (word, acc) =
          let bit = if even word then Zero else One
          in
          ( word `shiftR` 1, bit : acc )

decode :: FreqMap -> [Bit] -> String
decode freqMap bits = go [] htree bits
  where
    htree = buildTree freqMap
    go acc tree xs = case (tree, xs) of
      (Leaf _ char, []) -> reverse (char : acc)
      (Fork{}, []) -> error "bad decoding"
      (Leaf _ char, rest) -> go (char:acc) htree rest
      (Fork _ left _ , One  : rest) -> go acc left rest
      (Fork _ _ right, Zero : rest) -> go acc right rest

compress :: FilePath -> FilePath -> IO ()
compress src dst = do
  content <- readFile src
  let (freqMap, bits) = encode content
  Builder.writeFile dst (serialize freqMap bits)
  putStrLn "Done."

decompress :: FilePath -> FilePath -> IO ()
decompress src dst = do
  bs <- BS.readFile src
  writeFile dst (deserialize bs)
  putStrLn "Done."

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
