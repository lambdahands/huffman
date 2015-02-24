module Huffman.Cruncher (crunchLine) where

import Huffman.Tree
import Data.Maybe
import Data.Binary.Put
import Data.Binary.Bits.Put
import qualified Data.ByteString.Lazy as BL

crunchLine :: IO ()
crunchLine = do
  string <- getLine
  let encoded = fromMaybe [] (encode string)
  let binary  = mapM_ (putBool . (== 1)) encoded
  BL.putStr (runPut $ runBitPut binary)
