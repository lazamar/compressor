{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (replicateM, forM_, unless)
import Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import Data.Binary.Put (Put)
import qualified Data.Binary.Put as Put
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (sort, insert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Process
import System.TimeIt (timeIt)

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
      go (One : prefix) left ++
      go (Zero : prefix) right

encode :: FreqMap -> String -> [Bit]
encode freqMap str = encoded
  where
  codemap = buildCodes $ buildTree freqMap
  encoded = concatMap codeFor str
  codeFor char = codemap Map.! char

decode :: FreqMap -> [Bit] -> String
decode freqMap bits = go 1 htree bits
  where
  htree = buildTree freqMap
  total = sum $ Map.elems freqMap
  go count tree xs = case (tree, xs) of
    (Leaf _ char, rest)
      | count == total -> [char]
      | otherwise -> char : go (count + 1) htree rest
    (Fork _ left _ , One  : rest) -> go count left rest
    (Fork _ _ right, Zero : rest) -> go count right rest
    (Fork{}, []) -> error "bad decoding"

serialize :: FreqMap -> [Bit] -> ByteString
serialize freqmap bits = Put.runPut $ do
  serializeFreqMap freqmap
  write False 0 0 bits
  where
  write
    :: Bool   -- ^ are we writing the end marker
    -> Int    -- ^ bits filled in current byte
    -> Word8  -- ^ byte being filled
    -> [Bit]  -- ^ remaining bits
    -> Put
  write end n w bs
    | n == 8 = do
      Put.putWord8 w
      unless end $ write end 0 0 bs
    | otherwise =
      case bs of
        (One : rest) -> write end (n + 1) (w * 2 + 1) rest
        (Zero : rest) -> write end (n + 1) (w * 2) rest
        [] -> write True n w $ replicate (8 - n) Zero -- pad with zeroes

  serializeFreqMap :: FreqMap -> Put
  serializeFreqMap freqMap = do
    Put.putInt64be $ fromIntegral $ Map.size freqMap
    forM_ (Map.toList freqMap) $ \(char, freq) -> do
      Put.putWord8 (c2w char)
      Put.putInt64be $ fromIntegral freq

deserialize :: ByteString -> (FreqMap, [Bit])
deserialize bs = flip Get.runGet bs $ do
  freqMap <- deserializeFreqMap
  offset <- fromIntegral <$> Get.bytesRead
  let chars = drop offset $ BS.unpack bs
      bits = concatMap toBits chars
  return (freqMap, bits)
  where
  toBits :: Char -> [Bit]
  toBits char = getBit 0 (c2w char)

  getBit :: Int -> Word8 -> [Bit]
  getBit n word =
    if n == 8
      then []
      else bit : getBit (n + 1) (word * 2)
    where
      -- Test the leftmost bit. The byte 10000000 is the number 128.
      -- Anything less than 128 has a zero on the leftmost bit.
      bit = if word < 128 then Zero else One

  deserializeFreqMap :: Get FreqMap
  deserializeFreqMap = do
    len <- Get.getInt64be
    entries <- replicateM (fromIntegral len) $ do
      char <- Get.getWord8
      freq <- Get.getInt64be
      return (w2c char, fromIntegral freq)
    return $ Map.fromList entries

compress :: FilePath -> FilePath -> IO ()
compress src dst = do
  freqMap <- countFrequency . BS.unpack <$> BS.readFile src
  content <- BS.unpack <$> BS.readFile src
  let bits = encode freqMap content
  BS.writeFile dst (serialize freqMap bits)
  putStrLn "Done."

decompress :: FilePath -> FilePath -> IO ()
decompress src dst = do
  bs <- BS.readFile src
  let (freqMap, bits) = deserialize bs
      str = decode freqMap bits
  BS.writeFile dst (BS.pack str)
  putStrLn "Done."

test :: FilePath -> IO ()
test src = do
  let mid = src <> ".compressed"
      out = src <> ".decompressed"
  putStrLn "Compressing"
  timeIt $ compress src mid
  putStrLn "Decompressing"
  timeIt $ decompress mid out
  callProcess "diff" ["-s", src, out]
  callProcess "rm" [mid]
  callProcess "rm" [out]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["compress", src, dst] -> compress src dst
    ["decompress", src, dst] -> decompress src dst
    ["test", src] -> test src
    _ -> error $ unlines
      [ "Invalid arguments. Expected one of:"
      , "   compress FILE FILE"
      , "   decompress FILE FILE"
      ]
