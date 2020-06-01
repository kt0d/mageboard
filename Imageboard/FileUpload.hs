{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}    
module Imageboard.FileUpload (
    saveFile,
    tryMkFile,
    FileData
) where
import Control.Monad.Except
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

type FileData = N.FileInfo ByteString

uploadDir :: FilePath
uploadDir = "static/media/"

hashFile :: ByteString -> H.Digest SHA512
hashFile = H.hashlazy

-- https://www.garykessler.net/library/file_sigs.html
recognizeFormat :: ByteString -> Either Text Text
recognizeFormat b
    | is "\xFF\xD8"                         = Right "jpg"
    | is "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A" = Right "png"
    | is "GIF87a" || is "GIF89a"            = Right "gif"
    | is "%PDF"                             = Right "pdf"
    | is "\x1A\x45\xDF\xA3"                 = Right "webm"
    | isAfter 4 "ftypMSNV"                  = Right "mp4"
    | is "ID3"                              = Right "mp3"
    | is "OggS\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00" = Right "ogg"
    | otherwise = Left "Unsupported file format"
    where 
        isAfter n = flip B.isPrefixOf (B.drop n b)
        is = isAfter 0

tryMkFile :: FileData -> Either Text (File, FilePath)
tryMkFile f = do
    let bytes = N.fileContent f
    let len = B.length bytes
    if len > 16777216 then throwError "File is too big"
    else do
        let mime = N.fileContentType f
        format <- liftEither $ recognizeFormat bytes
        let baseName = (show $ hashFile bytes)
        return $ (File (T.pack baseName) format (fromIntegral $ B.length bytes) Nothing Nothing,
                baseName ++ "." ++ T.unpack format)

setDimensions :: File -> FilePath -> IO File
setDimensions = undefined

makeThumbnail :: File -> FilePath -> IO ()
makeThumbnail = undefined

saveFile :: File -> FileData -> FilePath -> ExceptT Text IO ()
saveFile f fdata path = do
    liftIO $ B.writeFile (uploadDir ++ path) $ N.fileContent fdata    
