{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface, CApiFFI, BangPatterns #-}
{-# OPTIONS_GHC -optc "-DPCRE2_CODE_UNIT_WIDTH=16" #-}

-- see https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/regexredux-ghc-3.html
-- and PCRE2 manpages

module Imageboard.Markup.PCRE2 (
    RegexReplace(..),
    gsub
) where
import Control.Monad
import Foreign (Word32, Word16, (.|.), 
    Ptr, nullPtr, poke, peek, with,
    alloca, allocaArray, mallocArray, free)
import Foreign.C (CInt(..), CSize(..))
import Data.Text (Text)
import Data.Text.IO as T
import qualified Data.Text.Foreign as TF
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace

data Code -- PCRE2_CODE
type CText = Ptr Word16 -- PCRE2_SPTR aka const PCRE2_UCHAR*

data RegexReplace = REReplace {pattern, replacement :: Text}

regexOptions :: Word32
regexOptions = c_PCRE2_UTF .|. c_PCRE2_MULTILINE

foreign import capi "pcre2.h value PCRE2_ERROR_NOMEMORY"
  c_PCRE2_ERROR_NOMEMORY :: CInt

foreign import capi "pcre2.h value PCRE2_ERROR_BADDATA"
  c_PCRE2_ERROR_BADDATA :: CInt

foreign import capi "pcre2.h value PCRE2_UTF"
  c_PCRE2_UTF :: Word32

foreign import capi "pcre2.h value PCRE2_MULTILINE"
  c_PCRE2_MULTILINE :: Word32

foreign import capi "pcre2.h value PCRE2_SUBSTITUTE_GLOBAL"
  c_PCRE2_SUBSTITUTE_GLOBAL :: Word32

foreign import capi "pcre2.h value PCRE2_SUBSTITUTE_OVERFLOW_LENGTH"
  c_PCRE2_SUBSTITUTE_OVERFLOW_LENGTH :: Word32

foreign import ccall "pcre2.h pcre2_compile_16"
  c_pcre2_compile :: CText -> CSize -> Word32 -> Ptr CInt -> Ptr CSize
    -> Ptr () -> IO (Ptr Code)

foreign import ccall "pcre2.h pcre2_get_error_message_16"
      c_pcre2_get_error_message :: CInt -> Ptr Word16 -> CSize -> IO CInt

foreign import ccall "pcre2.h pcre2_substitute_16"
    c_pcre2_substitute :: Ptr Code -> CText -> CSize -> CInt
        -> Word32 -> Ptr () -> Ptr ()
        -> CText -> CSize -> Ptr Word16 -> Ptr CSize -> IO CInt

foreign import ccall "pcre2.h pcre2_code_free_16"
  c_pcre2_code_free :: Ptr Code -> IO ()

compilePattern :: Text -> IO (Ptr Code)
compilePattern t = useAsPtr t $ \ptr len -> do
    alloca $ \errnum -> alloca $ \erroffset -> do
        code <- c_pcre2_compile 
            ptr len
            regexOptions
            errnum erroffset 
            nullPtr
        -- num <- peek errnum
        -- when (True)  (getErrorText num >>= T.putStrLn)
        return code

getErrorText :: CInt -> IO Text
getErrorText err = let bufflen = 2*256 in
    allocaArray bufflen $ \output' -> do
        outlen <- c_pcre2_get_error_message err output' $ fromIntegral bufflen
        if outlen == c_PCRE2_ERROR_BADDATA 
          then return "No such error"
          else TF.fromPtr output' (fromIntegral outlen - 1) -- TRAILING ZERO


substituteInternal :: (CText, CSize) -> RegexReplace -> IO (CText, CSize)
substituteInternal (subject', subjectLen) (REReplace p r) = 
    useAsPtr r $ \replac' replacLen -> 
    with 0 $ \outLen' -> do
        regex <- compilePattern p 
        ret <- c_pcre2_substitute regex 
            subject' subjectLen 0 
            (c_PCRE2_SUBSTITUTE_GLOBAL .|. c_PCRE2_SUBSTITUTE_OVERFLOW_LENGTH) 
            nullPtr nullPtr 
            replac' replacLen
            nullPtr outLen'
        --when (True)  (getErrorText ret >>= T.putStrLn)
        outLen <- (subtract 1) <$> peek outLen' -- TRAILING ZERO
        poke outLen' outLen
        output' <- mallocArray $ fromIntegral outLen
        subs <- c_pcre2_substitute regex 
            subject' subjectLen 0 
            c_PCRE2_SUBSTITUTE_GLOBAL 
            nullPtr nullPtr 
            replac' replacLen
            output' outLen'
        --when (True) (getErrorText subs >>= T.putStrLn)
        c_pcre2_code_free regex
        free subject'
        return $ (output', outLen)

substitute :: [RegexReplace] -> Text -> IO Text
substitute res subject = do
    (subject', subjectLen) <- unsafeToPtr subject
    (output', outLen) <- foldM substituteInternal 
        (subject', fromIntegral subjectLen) 
        res 
    unsafeToText output' (fromIntegral outLen)

useAsPtr :: Text -> (CText -> CSize -> IO a) -> IO a
useAsPtr t f = TF.useAsPtr t $ \tPtr tLen -> f tPtr (fromIntegral tLen)

unsafeToPtr :: Text -> IO (Ptr Word16, Int)
unsafeToPtr t = do
    let len = TF.lengthWord16 t
    t' <- mallocArray len
    TF.unsafeCopyToPtr t t'
    return (t', len)

unsafeToText :: Ptr Word16 -> Int -> IO Text
unsafeToText t' len = do
    t <- TF.fromPtr t' (fromIntegral len)
    free t'
    return t

{-# NOINLINE gsub #-}
gsub :: [RegexReplace] -> Text -> Text
gsub ps = unsafePerformIO . substitute ps