-- Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module Text.JSON.YAJL
	(
	-- * Parser
	  Parser
	, ParseStatus (..)
	, newParserIO
	, newParserST
	
	-- ** Parser callbacks
	, Callback
	, setCallback
	, clearCallback
	
	-- *** Containers
	, parsedBeginArray
	, parsedEndArray
	, parsedBeginObject
	, parsedEndObject
	
	-- *** Basic values
	, parsedNull
	, parsedBoolean
	
	-- *** Numeric callbacks
	, parsedInteger
	, parsedDouble
	, parsedNumber
	
	-- *** Text callbacks
	, parsedAttributeText
	, parsedAttributeBytes
	, parsedAttributeBuffer
	
	, parsedText
	, parsedBytes
	, parsedBuffer
	
	-- ** Parser input
	, parseText
	, parseLazyText
	, parseBytes
	, parseLazyBytes
	, parseBuffer
	, parseComplete
	, getBytesConsumed
	
	-- * Generator
	, Generator
	, GeneratorConfig (..)
	, GeneratorError (..)
	, newGenerator
	, getBuffer
	, clearBuffer
	
	-- ** Generator events
	, generateNull
	, generateBoolean
	, generateIntegral
	, generateDouble
	, generateNumber
	, generateText
	, generateBeginArray
	, generateEndArray
	, generateBeginObject
	, generateEndObject
	) where
import qualified Control.Exception as E
import qualified Control.Monad.ST as ST
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Typeable (Typeable)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Foreign.Concurrent as FC

-- import unqualified for C2Hs
import Foreign hiding (free)
import Foreign.C

#include <yajl/yajl_parse.h>
#include <yajl/yajl_gen.h>
#include <string.h>

{# pointer yajl_handle as ParserHandle newtype #}

data Parser m = Parser
	{ parserHandle :: ForeignPtr ParserHandle
	, parserCallbacks :: ForeignPtr ()
	, parserErrorRef :: IORef (Maybe E.SomeException)
	, parserToIO :: forall a. m a -> IO a
	, parserFromIO :: forall a. IO a -> m a
	}

data ParseStatus
	= ParseFinished
	| ParseContinue
	-- ^ More input is required before parsing can complete.
	
	| ParseCancelled
	-- ^ A callback returned 'False'.
	
	| ParseError T.Text
	-- ^ An error occured while parsing. The included message contains
	-- details about the error.
	
	deriving (Show, Eq)

newParserIO :: IO (Parser IO)
newParserIO = E.block $ do
	ref <- newIORef Nothing
	cCallbacks <- mallocForeignPtrBytes {# sizeof yajl_callbacks #}
	ParserHandle handlePtr <- withForeignPtr cCallbacks $ \raw -> do
		{# call memset #} raw 0 {# sizeof yajl_callbacks #}
		FC.addForeignPtrFinalizer cCallbacks $ freeParserCallbacks raw
		
		-- TODO: set checkUTF8 flag
		
		{# call yajl_alloc #} raw nullPtr nullPtr nullPtr
	parserFP <- newForeignPtr cParserFree handlePtr
	return $ Parser parserFP cCallbacks ref id id

newParserST :: ST.ST s (Parser (ST.ST s))
newParserST = ST.unsafeIOToST $ do
	p <- newParserIO
	return $ p
		{ parserToIO = ST.unsafeSTToIO
		, parserFromIO = ST.unsafeIOToST
		}

freeParserCallbacks :: Ptr () -> IO ()
freeParserCallbacks raw = do
	{# get yajl_callbacks->yajl_null #} raw >>= freeHaskellFunPtr
	{# get yajl_callbacks->yajl_boolean #} raw >>= freeHaskellFunPtr
	{# get yajl_callbacks->yajl_number #} raw >>= freeHaskellFunPtr
	{# get yajl_callbacks->yajl_string #} raw >>= freeHaskellFunPtr
	{# get yajl_callbacks->yajl_start_map #} raw >>= freeHaskellFunPtr
	{# get yajl_callbacks->yajl_map_key #} raw >>= freeHaskellFunPtr
	{# get yajl_callbacks->yajl_end_map #} raw >>= freeHaskellFunPtr
	{# get yajl_callbacks->yajl_start_array #} raw >>= freeHaskellFunPtr
	{# get yajl_callbacks->yajl_end_array #} raw >>= freeHaskellFunPtr

foreign import ccall "yajl/yajl_parse.h &yajl_free"
	cParserFree :: FunPtr (Ptr ParserHandle -> IO ())

-- | A callback should return 'True' to continue parsing, or 'False'
-- to cancel.
--
data Callback m a = Callback (Parser m -> a -> IO ()) (Parser m -> IO ())

setCallback :: Parser m -> Callback m a -> a -> m ()
setCallback p (Callback set _) io = parserFromIO p $ set p io

clearCallback :: Parser m -> Callback m a -> m ()
clearCallback p (Callback _ clear) = parserFromIO p $ clear p

-- Callback wrappers
type Callback0 = Ptr () -> IO CInt
type CallbackBool = Ptr () -> CInt -> IO CInt
type CallbackLong = Ptr () -> CLong -> IO CInt
type CallbackDouble = Ptr () -> CDouble -> IO CInt
type CallbackBuf = Ptr () -> Ptr CChar -> CUInt -> IO CInt
type CallbackUBuf = Ptr () -> Ptr CUChar -> CUInt -> IO CInt

catchRef :: Parser m -> m Bool -> IO CInt
catchRef p io = do
	continue <- E.catch (E.unblock (parserToIO p io)) $ \e -> do
		writeIORef (parserErrorRef p) $ Just e
		return False
	return $ cFromBool continue

wrapCallback0 :: Parser m -> m Bool -> IO (FunPtr Callback0)
wrapCallback0 p io = allocCallback0 $ \_ -> catchRef p io

wrapCallbackBool :: Parser m -> (Bool -> m Bool) -> IO (FunPtr CallbackBool)
wrapCallbackBool p io = allocCallbackBool $ \_ -> catchRef p . io . cToBool

wrapCallbackLong :: Parser m -> (Integer -> m Bool) -> IO (FunPtr CallbackLong)
wrapCallbackLong p io = allocCallbackLong $ \_ -> catchRef p . io . toInteger

wrapCallbackDouble :: Parser m -> (Double -> m Bool) -> IO (FunPtr CallbackDouble)
wrapCallbackDouble p io = allocCallbackDouble $ \_ -> catchRef p . io . realToFrac

wrapCallbackText :: Parser m -> (T.Text -> m Bool) -> IO (FunPtr CallbackUBuf)
wrapCallbackText p cb = wrapCallbackBytes p (cb . TE.decodeUtf8)

wrapCallbackBytes' :: Parser m -> (B.ByteString -> m Bool) -> IO (FunPtr CallbackBuf)
wrapCallbackBytes' p io =
	allocCallbackBuf $ \_ cstr len ->
	catchRef p $ parserFromIO p $ do
		bytes <- B.packCStringLen (castPtr cstr, fromIntegral len)
		parserToIO p $ io bytes

wrapCallbackBytes :: Parser m -> (B.ByteString -> m Bool) -> IO (FunPtr CallbackUBuf)
wrapCallbackBytes p io =
	allocCallbackUBuf $ \_ cstr len ->
	catchRef p $ parserFromIO p $ do
		bytes <- B.packCStringLen (castPtr cstr, fromIntegral len)
		parserToIO p $ io bytes

wrapCallbackBuffer :: Parser m -> ((Ptr Word8, Integer) -> m Bool) -> IO (FunPtr CallbackUBuf)
wrapCallbackBuffer p io =
	allocCallbackUBuf $ \_ cstr len ->
	catchRef p $
	io (castPtr cstr, toInteger len)

foreign import ccall "wrapper"
	allocCallback0 :: Callback0 -> IO (FunPtr Callback0)

foreign import ccall "wrapper"
	allocCallbackBool :: CallbackBool -> IO (FunPtr CallbackBool)

foreign import ccall "wrapper"
	allocCallbackLong :: CallbackLong -> IO (FunPtr CallbackLong)

foreign import ccall "wrapper"
	allocCallbackDouble :: CallbackDouble -> IO (FunPtr CallbackDouble)

foreign import ccall "wrapper"
	allocCallbackBuf :: CallbackBuf -> IO (FunPtr CallbackBuf)

foreign import ccall "wrapper"
	allocCallbackUBuf :: CallbackUBuf -> IO (FunPtr CallbackUBuf)

callback :: (Parser m -> a -> IO (FunPtr b))
         -> (Ptr () -> IO (FunPtr b))
         -> (Ptr () -> FunPtr b -> IO ())
         -> Callback m a
callback wrap getPtr setPtr = Callback set clear where
	set parser io = withForeignPtr (parserCallbacks parser) $ \p -> do
		free p
		wrap parser io >>= setPtr p
	clear parser = withForeignPtr (parserCallbacks parser) $ \p -> do
		free p
		setPtr p nullFunPtr
	free p = do
		cb <- getPtr p
		if cb == nullFunPtr
			then return ()
			else freeHaskellFunPtr cb

parsedBeginArray :: Callback m (m Bool)
parsedBeginArray = callback wrapCallback0
	{# get yajl_callbacks->yajl_start_array #}
	{# set yajl_callbacks->yajl_start_array #}

parsedEndArray :: Callback m (m Bool)
parsedEndArray = callback wrapCallback0
	{# get yajl_callbacks->yajl_end_array #}
	{# set yajl_callbacks->yajl_end_array #}

parsedBeginObject :: Callback m (m Bool)
parsedBeginObject = callback wrapCallback0
	{# get yajl_callbacks->yajl_start_map #}
	{# set yajl_callbacks->yajl_start_map #}

parsedEndObject :: Callback m (m Bool)
parsedEndObject = callback wrapCallback0
	{# get yajl_callbacks->yajl_end_map #}
	{# set yajl_callbacks->yajl_end_map #}

parsedNull :: Callback m (m Bool)
parsedNull = callback wrapCallback0
	{# get yajl_callbacks->yajl_null #}
	{# set yajl_callbacks->yajl_null #}

parsedBoolean :: Callback m (Bool -> m Bool)
parsedBoolean = callback wrapCallbackBool
	{# get yajl_callbacks->yajl_boolean #}
	{# set yajl_callbacks->yajl_boolean #}

parsedInteger :: Callback m (Integer -> m Bool)
parsedInteger = callback wrapCallbackLong
	{# get yajl_callbacks->yajl_integer #}
	{# set yajl_callbacks->yajl_integer #}

parsedDouble :: Callback m (Double -> m Bool)
parsedDouble = callback wrapCallbackDouble
	{# get yajl_callbacks->yajl_double #}
	{# set yajl_callbacks->yajl_double #}

-- | If 'parsedNumber' is set, it overrides 'parsedInteger' and 'parsedDouble'.
-- Registered functions for these callbacks will not receive any input until
-- 'parsedNumber' is unset.
--
-- If 'parsedNumber' is not set, but one of 'parsedInteger' or 'parsedDouble'
-- is set, then any values which cannot be represented by 'CLong' or 'CDouble'
-- will cause a parse error.
--
-- The 'B.ByteString' is in UTF-8.
parsedNumber :: Callback m (B.ByteString -> m Bool)
parsedNumber = callback wrapCallbackBytes'
	{# get yajl_callbacks->yajl_number #}
	{# set yajl_callbacks->yajl_number #}

-- | Only one of 'parsedAttributeText', 'parsedAttributeBytes', or
-- 'parsedAttributeBuffer' may be set. If another of these callbacks is set,
-- it will unset the others.
parsedAttributeText :: Callback m (T.Text -> m Bool)
parsedAttributeText = callback wrapCallbackText
	{# get yajl_callbacks->yajl_map_key #}
	{# set yajl_callbacks->yajl_map_key #}

-- | Only one of 'parsedAttributeText', 'parsedAttributeBytes', or
-- 'parsedAttributeBuffer' may be set. If another of these callbacks is set,
-- it will unset the others.
--
-- The 'B.ByteString' is in UTF-8.
parsedAttributeBytes :: Callback m (B.ByteString -> m Bool)
parsedAttributeBytes = callback wrapCallbackBytes
	{# get yajl_callbacks->yajl_map_key #}
	{# set yajl_callbacks->yajl_map_key #}

-- | Only one of 'parsedAttributeText', 'parsedAttributeBytes', or
-- 'parsedAttributeBuffer' may be set. If another of these callbacks is set,
-- it will unset the others.
--
-- The buffer is in UTF-8.
parsedAttributeBuffer :: Callback m ((Ptr Word8, Integer) -> m Bool)
parsedAttributeBuffer = callback wrapCallbackBuffer
	{# get yajl_callbacks->yajl_map_key #}
	{# set yajl_callbacks->yajl_map_key #}

-- | Only one of 'parsedText', 'parsedBytes', or 'parsedBuffer' may be set.
-- If another of these callbacks is set, it will unset the others.
parsedText :: Callback m (T.Text -> m Bool)
parsedText = callback wrapCallbackText
	{# get yajl_callbacks->yajl_string #}
	{# set yajl_callbacks->yajl_string #}

-- | Only one of 'parsedText', 'parsedBytes', or 'parsedBuffer' may be set.
-- If another of these callbacks is set, it will unset the others.
--
-- The 'B.ByteString' is in UTF-8.
parsedBytes :: Callback m (B.ByteString -> m Bool)
parsedBytes = callback wrapCallbackBytes
	{# get yajl_callbacks->yajl_string #}
	{# set yajl_callbacks->yajl_string #}

-- | Only one of 'parsedText', 'parsedBytes', or 'parsedBuffer' may be set.
-- If another of these callbacks is set, it will unset the others.
--
-- The buffer is in UTF-8.
parsedBuffer :: Callback m ((Ptr Word8, Integer) -> m Bool)
parsedBuffer = callback wrapCallbackBuffer
	{# get yajl_callbacks->yajl_string #}
	{# set yajl_callbacks->yajl_string #}

withParser :: Parser m -> (ParserHandle -> IO a) -> m a
withParser p io = parserFromIO p $ withParserIO p io

withParserIO :: Parser m -> (ParserHandle -> IO a) -> IO a
withParserIO p io = withForeignPtr (parserHandle p) $ io . ParserHandle

-- | Get the number of bytes consumed from the last input chunk.
-- 
-- Note that if using 'parseText' or 'parseLazyText', this corresponds to
-- UTF-8 bytes, /not/ characters.
-- 
-- If the most recent call to 'parseBytes', 'parseText', etc, returned
-- 'ParseFinished', this will indicate whether there are any un-parsed
-- bytes past the end of input.
-- 
-- If the most recent parse returned 'ParseError', this will indicate where
-- the error occured.
-- 
getBytesConsumed :: Parser m -> m Integer
getBytesConsumed p = withParser p $ \h ->
	toInteger `fmap` {# call yajl_get_bytes_consumed #} h

parseImpl :: Parser m -> (ParserHandle -> IO CInt) -> m ParseStatus
parseImpl p io = parserFromIO p $ do
	writeIORef (parserErrorRef p) Nothing
	rc <- E.block $ withParserIO p io
	touchForeignPtr $ parserCallbacks p
	case rc of
		0 -> return ParseFinished
		1 -> do
			threw <- readIORef $ parserErrorRef p
			case threw of
				Nothing -> return ParseCancelled
				Just exc -> E.throwIO exc
		2 -> return ParseContinue
		3 -> ParseError `fmap` getParseError p
		_ -> return $ ParseError . T.pack $ "Unknown 'yajl_status': " ++ show rc

parseText :: Parser m -> T.Text -> m ParseStatus
parseText p = parseBytes p . TE.encodeUtf8

parseLazyText :: Parser m -> TL.Text -> m ParseStatus
parseLazyText p = parseText p . T.concat . TL.toChunks

-- | The input must be in UTF-8.
parseBytes :: Parser m -> B.ByteString -> m ParseStatus
parseBytes p bytes = parseImpl p $ \h ->
	BU.unsafeUseAsCStringLen bytes $ \(cstr, len) ->
	{# call yajl_parse #} h (castPtr cstr) (fromIntegral len)

-- | The input must be in UTF-8.
parseLazyBytes :: Parser m -> BL.ByteString -> m ParseStatus
parseLazyBytes p = parseBytes p . B.concat . BL.toChunks

-- | The input must be in UTF-8.
parseBuffer :: Parser m -> (Ptr Word8, Integer) -> m ParseStatus
parseBuffer p (ptr, len) = parseImpl p $ \h ->
	{# call yajl_parse #} h (castPtr ptr) (fromIntegral len)

-- | Clients should call this when no more input is available, to indicate
-- EOF.
parseComplete :: Parser m -> m ParseStatus
parseComplete p = parseImpl p {# call yajl_parse_complete #}

getParseError :: Parser m -> IO T.Text
getParseError p = withParserIO p $ \h -> E.bracket
	({# call yajl_get_error #} h 0 nullPtr 0)
	({# call yajl_free_error #} h)
	(\bytes -> T.pack `fmap` peekCString (castPtr bytes))

data Generator s = Generator
	{ genHandle :: ForeignPtr GenHandle
	}

data GeneratorConfig = GeneratorConfig
	{ generatorBeautify :: Bool
	-- ^ Whether to generate indented, whitespaced output.
	
	, generatorIndent :: T.Text
	-- ^ How much to indent beautified output by. This is only used
	-- if 'generatorBeautify' is 'True'.
	}

-- | If an error is encountered when generating data, a 'GeneratorError'
-- will be thrown.
-- 
-- With the exception of 'MaximumDepthExceeded', this is usually due to
-- incorrect use of the library.
-- 
data GeneratorError
	= InvalidAttributeName
	| MaximumDepthExceeded
	| GeneratorInErrorState
	| GenerationComplete
	| InvalidNumber
	| NoBuffer
	| UnknownError Integer
	deriving (Show, Eq, Typeable)

instance E.Exception GeneratorError

{# pointer yajl_gen as GenHandle newtype #}
{# pointer *yajl_gen_config as GenConfig newtype #}

-- | Create a new, empty generator with the given configuration.
-- 
newGenerator :: GeneratorConfig -> ST.ST s (Generator s)
newGenerator config = ST.unsafeIOToST $
	allocaBytes {# sizeof yajl_gen_config #} $ \cConfig -> do
		cIndent <- marshalText $ generatorIndent config
	
		{# set yajl_gen_config->beautify #} cConfig $ cFromBool $ generatorBeautify config
		withForeignPtr cIndent $ {# set yajl_gen_config->indentString #} cConfig
	
		GenHandle handlePtr <- cGenAlloc (GenConfig cConfig) nullPtr
		touchForeignPtr cIndent
		handleFP <- newForeignPtr cGenFree handlePtr
		return $ Generator handleFP

marshalText :: T.Text -> IO (ForeignPtr CChar)
marshalText text =
	BU.unsafeUseAsCStringLen (TE.encodeUtf8 text) $ \(temp, len) ->
	mallocForeignPtrBytes (len + 1) >>= \fp ->
	withForeignPtr fp $ \array -> do
		copyArray array temp len
		poke (advancePtr array len) 0
		return fp

{# fun yajl_gen_alloc as cGenAlloc
	{ id `GenConfig'
	, id `Ptr ()'
	} -> `GenHandle' id #}

foreign import ccall "yajl/yajl_gen.h &yajl_gen_free"
	cGenFree :: FunPtr (Ptr GenHandle -> IO ())

withGenerator :: Generator s -> (GenHandle -> IO a) -> ST.ST s a
withGenerator gen io = ST.unsafeIOToST $ withForeignPtr (genHandle gen) $ io . GenHandle

-- | Retrieve the @NUL@-terminated byte buffer.
-- 
getBuffer :: Generator s -> ST.ST s B.ByteString
getBuffer gen =
	withGenerator gen $ \h ->
	alloca $ \bufPtr ->
	alloca $ \lenPtr -> do
	{# call yajl_gen_get_buf #} h bufPtr lenPtr
	buf <- peek bufPtr
	len <- peek lenPtr
	-- TODO: check that len is < (maxBound :: Int)
	B.packCStringLen (castPtr buf, fromIntegral len)

-- | Clear the generator's output buffer. This does not change the state
-- of the generator.
-- 
clearBuffer :: Generator s -> ST.ST s ()
clearBuffer g = withGenerator g {# call yajl_gen_clear #}

generateNull :: Generator s -> ST.ST s ()
generateNull g = generate' g {# call yajl_gen_null #}

generateBoolean :: Generator s -> Bool -> ST.ST s ()
generateBoolean g x = generate' g $ \h ->
	{# call yajl_gen_bool #} h (cFromBool x)

generateIntegral :: Integral a => Generator s -> a -> ST.ST s ()
generateIntegral g = generateNumber g . showBytes . toInteger

generateDouble :: Generator s -> Double -> ST.ST s ()
generateDouble g = generateNumber g . showBytes

generateNumber :: Generator s -> B.ByteString -> ST.ST s ()
generateNumber g bytes = generate' g $ \h ->
	BU.unsafeUseAsCStringLen bytes $ \(cstr, len) ->
	{# call yajl_gen_number #} h (castPtr cstr) (fromIntegral len)

generateText :: Generator s -> T.Text -> ST.ST s ()
generateText g text = generate' g $ \h ->
	withUtf8 text $ \(utf8, len) ->
	{# call yajl_gen_string #} h utf8 len

generateBeginArray :: Generator s -> ST.ST s ()
generateBeginArray g = generate' g {# call yajl_gen_array_open #}

generateEndArray :: Generator s -> ST.ST s ()
generateEndArray g = generate' g {# call yajl_gen_array_close #}

generateBeginObject :: Generator s -> ST.ST s ()
generateBeginObject g = generate' g {# call yajl_gen_map_open #}

generateEndObject :: Generator s -> ST.ST s ()
generateEndObject g = generate' g {# call yajl_gen_map_close #}

generate' :: Generator s -> (GenHandle -> IO CInt) -> ST.ST s ()
generate' g io = withGenerator g io >>= \rc -> case rc of
	0 -> return ()
	1 -> throwST InvalidAttributeName
	2 -> throwST MaximumDepthExceeded
	3 -> throwST GeneratorInErrorState
	4 -> throwST GenerationComplete
	5 -> throwST InvalidNumber
	6 -> throwST NoBuffer
	_ -> throwST $ UnknownError $ toInteger rc

cFromBool :: Integral a => Bool -> a
cFromBool True = 1
cFromBool False = 0

cToBool :: CInt -> Bool
cToBool 1 = True
cToBool 0 = False
cToBool x = error $ "cToBool " ++ show x

withUtf8 :: T.Text -> ((Ptr CUChar, CUInt) -> IO a) -> IO a
withUtf8 text io =
	let bytes = TE.encodeUtf8 text in
	BU.unsafeUseAsCStringLen bytes $ \(cstr, len) ->
	io (castPtr cstr, fromIntegral len)

showBytes :: Show a => a -> B.ByteString
showBytes = BC.pack . show

throwST :: E.Exception e => e -> ST.ST s a
throwST = ST.unsafeIOToST . E.throwIO
