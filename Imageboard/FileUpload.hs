{-# LANGUAGE OverloadedStrings #-}
module Imageboard.FileUpload (
    saveFile
) where
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Web.Scotty as S
import qualified Network.Wai.Parse as N (FileInfo(..))
import qualified Crypto.Hash as H
import Crypto.Hash.Algorithms (SHA512)
import Debug.Trace

import Imageboard.Types (File(..))

hashFile :: ByteString -> H.Digest SHA512
hashFile = H.hashlazy

saveFile :: S.File -> IO (Either Text File)
saveFile f = do
    let bytes = N.fileContent $ snd f
    let newFilename = show $ hashFile bytes
    B.writeFile newFilename bytes
    return $ Right $ File (T.pack newFilename) (fromIntegral $ B.length bytes) Nothing Nothing

    



