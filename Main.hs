module Main where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as B
import           Data.Foldable
import           Synth
import           System.Environment
import           Text.Printf

saveFile :: FilePath -> [Synth.Sample] -> IO ()
saveFile path samples = B.writeFile path $ B.toLazyByteString $ fold $ map B.floatLE samples

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [[Char]] -> IO ()
parseArgs ["-o", path] = do
    saveFile (printf "%s" path) (render)
    putStrLn $ printf "Saved to %s" path

parseArgs _ = putStrLn "usage: hasksynth -o <file path>"
