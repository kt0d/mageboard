{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface, CApiFFI, BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -optc "-DPCRE2_CODE_UNIT_WIDTH=16" #-}

-- see <https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/regexredux-ghc-3.html>
-- and PCRE2 manpages, also available at <https://www.pcre.org/current/doc/html/pcre2api.html>
{-|
PCRE2 bindings for Data.Text.
|-}
module Regex.PCRE2 (
    RegexReplace(..),
    gsub,
    gsubWith,
    gsubWithM,
    getAllMatches
) where
import Control.Monad
import Foreign (Word32, Word16, (.|.), 
    Ptr, nullPtr, poke, peek, with,
    alloca, allocaArray, mallocArray, peekArray, free)
import Foreign.C (CInt(..), CSize(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as T
import qualified Data.Text.Foreign as TF
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace
import qualified Data.Array as A
import Data.Functor.Identity

data Code -- PCRE2_CODE
data MatchData -- PCRE2_MATCH_DATA
type CText = Ptr Word16 -- PCRE2_SPTR aka const PCRE2_UCHAR*

data RegexReplace = REReplace {pattern, replacement :: Text}

regexOptions :: Word32
regexOptions = c_PCRE2_UTF .|. c_PCRE2_MULTILINE

substituteOptions :: Word32
substituteOptions = c_PCRE2_NO_UTF_CHECK .|. c_PCRE2_SUBSTITUTE_GLOBAL

foreign import capi "pcre2.h value PCRE2_ERROR_NOMEMORY"
  c_PCRE2_ERROR_NOMEMORY :: CInt

foreign import capi "pcre2.h value PCRE2_ERROR_BADDATA"
  c_PCRE2_ERROR_BADDATA :: CInt

foreign import capi "pcre2.h value PCRE2_UTF"
  c_PCRE2_UTF :: Word32

foreign import capi "pcre2.h value PCRE2_NO_UTF_CHECK"
  c_PCRE2_NO_UTF_CHECK :: Word32

foreign import capi "pcre2.h value PCRE2_MULTILINE"
  c_PCRE2_MULTILINE :: Word32

foreign import capi "pcre2.h value PCRE2_SUBSTITUTE_GLOBAL"
  c_PCRE2_SUBSTITUTE_GLOBAL :: Word32

foreign import capi "pcre2.h value PCRE2_SUBSTITUTE_OVERFLOW_LENGTH"
  c_PCRE2_SUBSTITUTE_OVERFLOW_LENGTH :: Word32

foreign import capi "pcre2.h value PCRE2_ERROR_NOMATCH"
  c_PCRE2_ERROR_NOMATCH :: CInt

foreign import ccall "pcre2.h pcre2_compile_16"
  c_pcre2_compile :: CText -> CSize -> Word32 -> Ptr CInt -> Ptr CSize
    -> Ptr () -> IO (Ptr Code)

foreign import ccall "pcre2.h pcre2_get_error_message_16"
      c_pcre2_get_error_message :: CInt -> Ptr Word16 -> CSize -> IO CInt

foreign import ccall "pcre2.h pcre2_substitute_16"
    c_pcre2_substitute :: Ptr Code -> CText -> CSize -> CSize
        -> Word32 -> Ptr () -> Ptr ()
        -> CText -> CSize -> Ptr Word16 -> Ptr CSize -> IO CInt

foreign import ccall "pcre2.h pcre2_match_16"
    c_pcre2_match :: Ptr Code -> CText -> CSize -> CSize
        -> Word32 -> Ptr MatchData -> Ptr () -> IO CInt

foreign import ccall "pcre2.h pcre2_code_free_16"
  c_pcre2_code_free :: Ptr Code -> IO ()

foreign import ccall "pcre2.h pcre2_match_data_create_from_pattern_16"
    c_pcre2_match_data_create_from_pattern :: Ptr Code -> Ptr () -> IO (Ptr MatchData)

foreign import ccall "pcre2.h pcre2_get_ovector_count_16"
    c_pcre2_get_ovector_count :: Ptr MatchData -> IO Word32

foreign import ccall "pcre2.h pcre2_get_ovector_pointer_16"
    c_pcre2_get_ovector_pointer :: Ptr MatchData -> IO (Ptr CSize)

foreign import ccall "pcre2.h pcre2_match_data_free_16"
  c_pcre2_match_data_free :: Ptr MatchData -> IO ()

compilePattern :: Text -> IO (Ptr Code)
compilePattern t = useAsPtr t $ \ptr len -> do
    alloca $ \errnum -> alloca $ \erroffset -> do
        code <- c_pcre2_compile 
            ptr len
            regexOptions
            errnum erroffset 
            nullPtr
        when (code == nullPtr) $ do
          errmsg <- T.unpack <$> (getErrorText =<< peek errnum)
          offset <- peek erroffset
          errorWithoutStackTrace $ "PCRE2.compilePattern error \
              \in pattern: " ++ T.unpack t ++ " at offset: " ++ show offset ++ ": " ++ errmsg 
        return code

getErrorText :: CInt -> IO Text
getErrorText err = let bufflen = 256 in
    allocaArray bufflen $ \output' -> do
        outlen <- c_pcre2_get_error_message err output' $ fromIntegral bufflen
        if outlen == c_PCRE2_ERROR_BADDATA 
          then return "No such error"
          else TF.fromPtr output' (fromIntegral outlen)


substituteInternal :: (CText, CSize) -> RegexReplace -> IO (CText, CSize)
substituteInternal (subject', subjectLen) (REReplace p r) = 
    useAsPtr r $ \replac' replacLen -> 
    with 0 $ \outLen' -> do
        regex <- compilePattern p 
        _ <- c_pcre2_substitute regex 
            subject' subjectLen 0 
            (substituteOptions .|. c_PCRE2_SUBSTITUTE_OVERFLOW_LENGTH) 
            nullPtr nullPtr 
            replac' replacLen
            nullPtr outLen'
        outLen <- subtract 1 <$> peek outLen' -- TRAILING ZERO
        poke outLen' outLen
        output' <- mallocArray $ fromIntegral outLen
        _ <- c_pcre2_substitute regex 
            subject' subjectLen 0 
            substituteOptions 
            nullPtr nullPtr 
            replac' replacLen
            output' outLen'
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

-- | Match patterns and do substitutions.
-- This function will try to compile given patterns and apply
-- given substitutions in order they appear in input list.
-- It may throw runtime error if submitted match pattern is incorrect.
{-# NOINLINE gsub #-}
gsub :: [RegexReplace] -> Text -> Text
gsub ps = unsafePerformIO . substitute ps

ovectorToArray :: Text -> Ptr MatchData -> IO ((CSize,CSize), A.Array Int Text)
ovectorToArray subject matchData = do
    ovCount <- c_pcre2_get_ovector_count matchData
    ovPtr <- c_pcre2_get_ovector_pointer matchData
    ovector <- toPairs <$> peekArray (2 * fromIntegral ovCount) ovPtr
    let ovArr = A.listArray (0,fromIntegral ovCount - 1) $ map (flip sliceWord16 subject) ovector
    return (head ovector, ovArr)
    where
        toPairs (x0:x1:xs) = (x0,x1) : toPairs xs
        toPairs [] = []
        toPairs _ = errorWithoutStackTrace "odd number of elements in list"

sliceWord16 :: Integral a => (a,a) -> Text -> Text
sliceWord16 (a,b) = TF.takeWord16 (fromIntegral (b-a)) . TF.dropWord16 (fromIntegral a)

replaceWith :: Monad m => Text -> Text -> ((Int -> Text) -> m Text) -> IO (m Text)
replaceWith s p fun = do
    (s', sLen) <- unsafeToPtr s
    regex <- compilePattern p
    matchData <- c_pcre2_match_data_create_from_pattern regex nullPtr
    let loop (sofar::m Text,startOffset::CSize) = do
            ret <- c_pcre2_match regex
                s' (fromIntegral sLen)
                startOffset 0
                matchData nullPtr
            if ret == c_PCRE2_ERROR_NOMATCH
            then let newSofar = do
                        t <- sofar
                        return $ t <> TF.dropWord16 (fromIntegral startOffset) s
                 in return (newSofar,0)
            else do
                ((matchOffset,afterMatch),ovArr) <- ovectorToArray s matchData
                let newSofar = do
                        t <- sofar
                        replac <- fun (ovArr A.!)
                        return $ t <> sliceWord16 (startOffset,matchOffset) s <> replac
                loop (newSofar,afterMatch)
    (output,_::CSize) <- loop (return T.empty,0)
    free s'
    c_pcre2_code_free regex
    c_pcre2_match_data_free matchData
    return output

-- | Match pattern and substitute with output of given function.
-- This function will compile given pattern and substitute matched parts
-- of input text with output of substituting function. Substituting function
-- can access captured groups by their number.
-- It may throw runtime error if match pattern is incorrect or if capture group
-- with given number does not exist.
{-# NOINLINE gsubWith #-}
gsubWith :: Text -- ^ Input text
    -> Text -- ^ Pattern to match
    -> ((Int -> Text) -> Text) -- Substituting function
    -> Text -- ^ Output text
gsubWith s p f = runIdentity $ unsafePerformIO $ replaceWith s p (return . f)

-- | Monadic version of 'gsubWith'.
{-# NOINLINE gsubWithM #-}
gsubWithM :: Monad m => Text -> Text -> ((Int -> Text) -> m Text) -> m Text
gsubWithM s p f = unsafePerformIO $ replaceWith s p f

collectMaches :: Monoid a => Text -> Text -> ((Int -> Text) -> a)  -> IO a
collectMaches s p fun = do
    (s', sLen) <- unsafeToPtr s
    regex <- compilePattern p
    matchData <- c_pcre2_match_data_create_from_pattern regex nullPtr
    let loop (sofar::a,startOffset::CSize) = do
            ret <- c_pcre2_match regex
                s' (fromIntegral sLen)
                startOffset 0
                matchData nullPtr
            if ret == c_PCRE2_ERROR_NOMATCH
            then return (sofar,0)
            else do
                ((_,afterMatch),ovArr) <- ovectorToArray s matchData
                let newSofar = sofar <> fun (ovArr A.!)
                loop (newSofar,afterMatch)
    (output,_::CSize) <- loop (mempty,0)
    free s'
    c_pcre2_code_free regex
    c_pcre2_match_data_free matchData
    return output

{-# NOINLINE getAllMatches #-}
getAllMatches :: Text -> Text -> ((Int -> Text) -> a) -> [a]
getAllMatches s p fun = unsafePerformIO $ collectMaches s p (pure . fun)