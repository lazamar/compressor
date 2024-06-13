{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (replicateM, forM_, unless)
import Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import Data.Binary.Put (Put)
import qualified Data.Binary.Put as Put
import Data.Bits (shiftR)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (chr)
import Data.List (sort, insert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word8)
import System.Environment (getArgs)

_END_OF_INPUT :: Char
_END_OF_INPUT = chr 300

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
buildTree
  = build
  . sort
  . fmap (\(c,w) -> Leaf w c)
  . Map.toList
  . Map.insert _END_OF_INPUT 1
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

encode :: FreqMap -> String -> [Bit]
encode freqMap str = encoded
  where
  codemap = buildCodes $ buildTree freqMap
  encoded = concatMap codeFor str ++ codeFor _END_OF_INPUT
  codeFor char = codemap Map.! char

serialize :: FreqMap -> [Bit] -> Builder
serialize freqmap bits = Put.execPut $ do
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

-- | Split a list in chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = ys : chunksOf n zs
    where (ys,zs) = splitAt n xs

serializeFreqMap :: FreqMap -> Put
serializeFreqMap freqMap = do
  Put.putInt64be $ fromIntegral $ Map.size freqMap
  forM_ (Map.toList freqMap) $ \(char, freq) -> do
    Put.putWord8 (c2w char)
    Put.putInt64be $ fromIntegral freq

deserializeFreqMap :: Get FreqMap
deserializeFreqMap = do
  len <- Get.getInt64be
  entries <- replicateM (fromIntegral len) $ do
    char <- Get.getWord8
    freq <- Get.getInt64be
    return (w2c char, fromIntegral freq)
  return $ Map.fromList entries

deserialize :: ByteString -> ByteString
deserialize = Get.runGet $ do
  freqMap <- deserializeFreqMap
  chars <- BS.unpack <$> Get.getRemainingLazyByteString
  let bits = concatMap toBits chars
  return $ decode freqMap bits
  where
    toBits :: Char -> [Bit]
    toBits char = snd
      $ getBit
      $ getBit
      $ getBit
      $ getBit
      $ getBit
      $ getBit
      $ getBit
      $ getBit (c2w char, [])
      where
        getBit (word, acc) =
          let bit = if even word then Zero else One
          in
          ( word `shiftR` 1, bit : acc )

decode :: FreqMap -> [Bit] -> ByteString
decode freqMap bits = BS.pack $ go [] htree bits
  where
    htree = buildTree freqMap
    go acc tree xs = case (tree, xs) of
      (Leaf _ char, rest)
        | char == _END_OF_INPUT -> reverse acc
        | otherwise -> go (char:acc) htree rest
      (Fork{}, []) -> error "bad decoding"
      (Fork _ left _ , One  : rest) -> go acc left rest
      (Fork _ _ right, Zero : rest) -> go acc right rest

compress :: FilePath -> FilePath -> IO ()
compress src dst = do
  freqMap <- countFrequency . BS.unpack <$> BS.readFile src
  content <- BS.unpack <$> BS.readFile src
  let bits = encode freqMap content
  Builder.writeFile dst (serialize freqMap bits)
  putStrLn "Done."

decompress :: FilePath -> FilePath -> IO ()
decompress src dst = do
  bs <- BS.readFile src
  BS.writeFile dst (deserialize bs)
  putStrLn "Done."

test :: FilePath -> IO ()
test src = do
  bytes <- BS.readFile src
  let content = BS.unpack bytes
      freqMap = countFrequency content
      bs = Builder.toLazyByteString
        $ Put.execPut
        $ serializeFreqMap freqMap
      freqMap' = Get.runGet deserializeFreqMap bs

      keys = Set.toList $ Map.keysSet freqMap <> Map.keysSet freqMap'

      keysRemoved = filter (\k -> not $ Map.member k freqMap') keys
      keysAdded = filter (\k -> not $ Map.member k freqMap) keys

  if freqMap == freqMap'
     then putStrLn "Success"
     else do
        putStrLn "Removed:"
        print keysRemoved
        putStrLn "Added:"
        print keysAdded
        putStrLn "Failure"

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
