module Misc.Binary where

import           Data.Word         (Word8)
import           Text.Parsec.Error (ParseError)

class FromBytes a where
  decode :: [Word8] -> Either ParseError a
