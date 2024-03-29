{-# LANGUAGE OverloadedStrings #-}
module Imageboard.FileUpload (
    saveFile,
    tryMkFile,
    FileData
) where
import Control.Monad.IO.Class
import Control.Monad.Except
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Network.Wai.Parse as N (FileInfo(..))
import qualified Crypto.Hash as H
import Crypto.Hash.Algorithms (SHA512)
import qualified System.Process as P
import System.Exit (ExitCode(..))
import Imageboard.Types (File(..), FileType(..), Dimensions(..))
import qualified Imageboard.Config as Config

-- | Type representing file as received by HTTP server.
type FileData = N.FileInfo B.ByteString

hashFile :: ByteString -> H.Digest SHA512
hashFile = H.hashlazy

toExtension :: IsString a => FileType -> a
toExtension JPG = ".jpg"
toExtension PNG = ".png"
toExtension GIF = ".gif"
toExtension WEBM = ".webm"
toExtension MP4 = ".mp4"
toExtension MP3 = ".mp3"
toExtension OGG = ".ogg"
toExtension PDF = ".pdf"
toExtension EPUB = ".epub"
toExtension SWF = ".swf"

-- <https://www.garykessler.net/library/file_sigs.html>
recognizeFormat :: ByteString -> Either Text FileType
recognizeFormat b
    | is "\xFF\xD8"                         = Right JPG
    | is "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A" = Right PNG
    | is "GIF87a" || is "GIF89a"            = Right GIF
    | is "\x1A\x45\xDF\xA3"                 = Right WEBM
    | isAfter 4 "ftypMSNV"                  = Right MP4
    | isAfter 4 "ftypisom"                  = Right MP4
    | is "ID3"                              = Right MP3
    | is "OggS\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00" = Right OGG
    | is "%PDF"                             = Right PDF
    | is "\x50\x4B\x03\x04" && isAfter 50 "epub+zip" = Right EPUB
    | is "FWS" || is "ZWS" || is "CWS"      = Right SWF
    | otherwise = Left "Unsupported file format"
    where 
        isAfter n = flip B.isPrefixOf (B.drop n b)
        is = isAfter 0

-- | Validate file send by the user.
-- This function recognizes format of sent file and its size
-- and fails if they do not match predefined requirements.
-- Otherwise, it returns new file and filename.
tryMkFile :: FileData -> Either Text (File, FilePath)
tryMkFile f = do
    let bytes = N.fileContent f
    let len = B.length bytes
    if len > 16777216 then throwError "File is too big"
    else do
        format <- liftEither $ recognizeFormat bytes
        let baseName = (show $ hashFile bytes) ++ toExtension format
        return $ (File (T.pack baseName) format (fromIntegral $ B.length bytes) Nothing
            , baseName)

-- | gm identify -format '%w %h' path[0]
getImgDimensions :: MonadIO m => FilePath -> ExceptT Text m Dimensions
getImgDimensions path = do
    (exit, out, _) <- liftIO $ P.readProcessWithExitCode "gm" 
            ["identify", "-format", "%w %h", path++"[0]"] []
    case exit of
        ExitSuccess -> do
            let [w,h] = map read $ words out
            return $ Dim w h
        ExitFailure code ->            
            throwError $ "'gm identify' failed with code: " `T.append` (T.pack $ show code)

-- | gm convert -strip -filter Box -thumbnail 200x200 path[0] toPath
mkImgThumbnail :: MonadIO m => FilePath -> FilePath -> ExceptT Text m ()
mkImgThumbnail path toPath = do
    (exit, _, _) <- liftIO $ P.readProcessWithExitCode "gm" 
            ["convert", "-strip", "-filter", "Box", "-thumbnail", "200x200"
            , path ++ "[0]", toPath] []
    case exit of
        ExitSuccess -> return ()
        ExitFailure code ->            
            throwError $ "'gm convert' failed with code: " `T.append` (T.pack $ show code)

-- | ffprobe -v -8 -show_entries stream=width,height -of 'csv=p=0:s=\ ' -select_streams v:0 path
getVidDimensions :: MonadIO m => FilePath -> ExceptT Text m Dimensions
getVidDimensions path = do
    (exit, out, _) <- liftIO $ P.readProcessWithExitCode "ffprobe"
            ["-v", "-8", "-show_entries", "stream=width,height",
             "-of", "csv=p=0:s=\\ ",   "-select_streams", "v:0", path] []
    case exit of
        ExitSuccess -> do
            let [w,h] = map read $ words out
            return $ Dim w h
        ExitFailure code ->            
            throwError $ "'ffprobe' failed with code: " `T.append` (T.pack $ show code)         

-- | ffmpeg -v -8 -i path -f mjpeg -vframes 1 -vf scale=w=200:h=200:force_original_aspect_radio=decrese -y toPath.jpg
mkVidThumbnail :: MonadIO m => FilePath -> FilePath -> ExceptT Text m ()
mkVidThumbnail path toPath = do
    (exit, _, _) <- liftIO $ P.readProcessWithExitCode "ffmpeg" 
            ["-v", "-8", "-i", path, "-f", "mjpeg", "-vframes", "1", 
            "-vf", "scale=w=200:h=200:force_original_aspect_ratio=decrease", "-y", toPath] []
    case exit of
        ExitSuccess -> return ()
        ExitFailure code ->            
            throwError $ "'ffmpeg' failed with code: " `T.append` (T.pack $ show code)

processFile :: MonadIO m => File -> FilePath -> ExceptT Text m File
processFile f = case ext f of
    JPG  -> doImg
    PNG  -> doImg
    GIF  -> doImg
    WEBM -> doVid 
    MP4  -> doVid
    PDF  -> doPdf
    _  -> zz
    where 
        zz = const $ pure f
        doFile dimAction mkThumbAction extension path = do
            dims <- dimAction (Config.uploadDir ++ path)
            _ <- mkThumbAction (Config.uploadDir ++ path) (Config.thumbnailDir ++ path ++ extension)
            return $ f { dim = Just dims}
        doPdf = doFile getImgDimensions mkImgThumbnail ".jpg"
        doImg = doFile getImgDimensions mkImgThumbnail ""
        doVid = doFile getVidDimensions mkVidThumbnail ".jpg"

-- | Try to save file, recognize its size and dimensions and generate thumbnail for it.
saveFile :: MonadIO m => File -> FileData -> FilePath -> ExceptT Text m File
saveFile f fdata path = do
    liftIO $ B.writeFile (Config.uploadDir ++ path) $ N.fileContent fdata
    processFile f path