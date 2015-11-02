module Bmp.Parse where
import qualified Data.ByteString as BS (readFile)
import Bmp.Format


--pHeader =
info s = 1

fileInfo = (>>= return . info) BS.readFile
