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
module Text.JSON.YAJL
	(
	-- * Parser
	  Parser
	, ParserCallbacks (..)
	, ParseStatus (..)
	, newParser
	, parseUTF8
	, parseText
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable (Typeable)
import qualified Data.STRef as ST
import qualified Foreign.Concurrent as FC

-- import unqualified for C2Hs
import Foreign
import Foreign.C

#include <yajl/yajl_parse.h>
#include <yajl/yajl_gen.h>

data Parser s = Parser
	{ parserHandle :: ForeignPtr ParserHandle
	, parserCallbacks :: ForeignPtr ()
	, parserErrorRef :: ST.STRef s (Maybe E.SomeException)
	}

-- | Each callback should return 'True' to continue parsing, or 'False'
-- to cancel.
--
data ParserCallbacks s = ParserCallbacks
	{ parsedNull :: ST.ST s Bool
	, parsedBoolean :: Bool -> ST.ST s Bool
	, parsedNumber :: B.ByteString -> ST.ST s Bool
	, parsedText :: T.Text -> ST.ST s Bool
	, parsedBeginArray :: ST.ST s Bool
	, parsedEndArray :: ST.ST s Bool
	, parsedBeginObject :: ST.ST s Bool
	, parsedAttributeName :: T.Text -> ST.ST s Bool
	, parsedEndObject :: ST.ST s Bool
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

{# pointer yajl_handle as ParserHandle newtype #}

newParser :: ParserCallbacks s -> ST.ST s (Parser s)
newParser callbacks = do
	ref <- ST.newSTRef Nothing
	ST.unsafeIOToST $ do
		cCallbacks <- mallocForeignPtrBytes {# sizeof yajl_callbacks #}
		withForeignPtr cCallbacks $ \raw -> do
			wrapCallback0 ref (parsedNull callbacks)
				>>= {# set yajl_callbacks->yajl_null #} raw
			wrapCallbackBool ref (parsedBoolean callbacks)
				>>= {# set yajl_callbacks->yajl_boolean #} raw
			wrapCallbackNum ref (parsedNumber callbacks)
				>>= {# set yajl_callbacks->yajl_number #} raw
			wrapCallbackText ref (parsedText callbacks)
				>>= {# set yajl_callbacks->yajl_string #} raw
			wrapCallback0 ref (parsedBeginObject callbacks)
				>>= {# set yajl_callbacks->yajl_start_map #} raw
			wrapCallbackText ref (parsedAttributeName callbacks)
				>>= {# set yajl_callbacks->yajl_map_key #} raw
			wrapCallback0 ref (parsedEndObject callbacks)
				>>= {# set yajl_callbacks->yajl_end_map #} raw
			wrapCallback0 ref (parsedBeginArray callbacks)
				>>= {# set yajl_callbacks->yajl_start_array #} raw
			wrapCallback0 ref (parsedEndArray callbacks)
				>>= {# set yajl_callbacks->yajl_end_array #} raw
			
			-- Unused
			{# set yajl_callbacks->yajl_integer #} raw nullFunPtr
			{# set yajl_callbacks->yajl_double #} raw nullFunPtr
			
			FC.addForeignPtrFinalizer cCallbacks $ freeParserCallbacks raw
		
		ParserHandle handlePtr <- withForeignPtr cCallbacks $ \ptr ->
			{# call yajl_alloc #} ptr nullPtr nullPtr nullPtr
		parserFP <- newForeignPtr cParserFree handlePtr
		return $ Parser parserFP cCallbacks ref

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

-- Callback wrappers
type Callback0 = Ptr () -> IO CInt
type CallbackBool = Ptr () -> CInt -> IO CInt
type CallbackNum = Ptr () -> Ptr CChar -> CUInt -> IO CInt
type CallbackText = Ptr () -> Ptr CUChar -> CUInt -> IO CInt

catchRef :: ST.STRef s (Maybe E.SomeException) -> ST.ST s Bool -> IO CInt
catchRef ref st = do
	continue <- E.catch (E.unblock (ST.unsafeSTToIO st)) $ \e -> do
		ST.unsafeSTToIO $ ST.writeSTRef ref $ Just e
		return False
	return $ cFromBool continue

wrapCallback0 :: ST.STRef s (Maybe E.SomeException) -> ST.ST s Bool -> IO (FunPtr Callback0)
wrapCallback0 ref st = allocCallback0 $ \_ -> catchRef ref st

wrapCallbackBool :: ST.STRef s (Maybe E.SomeException) -> (Bool -> ST.ST s Bool) -> IO (FunPtr CallbackBool)
wrapCallbackBool ref st = allocCallbackBool $ \_ -> catchRef ref . st . cToBool

wrapCallbackNum :: ST.STRef s (Maybe E.SomeException) -> (B.ByteString -> ST.ST s Bool) -> IO (FunPtr CallbackNum)
wrapCallbackNum ref st = allocCallbackNum $ \_ cstr len -> catchRef ref $ do
	bytes <- ST.unsafeIOToST $ B.packCStringLen (cstr, fromIntegral len)
	st bytes

wrapCallbackText :: ST.STRef s (Maybe E.SomeException) -> (T.Text -> ST.ST s Bool) -> IO (FunPtr CallbackText)
wrapCallbackText ref st = allocCallbackText $ \_ cstr len -> catchRef ref $ do
	bytes <- ST.unsafeIOToST $ BU.unsafePackCStringLen (castPtr cstr, fromIntegral len)
	st (TE.decodeUtf8 bytes)

foreign import ccall "wrapper"
	allocCallback0 :: Callback0 -> IO (FunPtr Callback0)

foreign import ccall "wrapper"
	allocCallbackBool :: CallbackBool -> IO (FunPtr CallbackBool)

foreign import ccall "wrapper"
	allocCallbackNum :: CallbackNum -> IO (FunPtr CallbackNum)

foreign import ccall "wrapper"
	allocCallbackText :: CallbackText -> IO (FunPtr CallbackText)

withParser :: Parser s -> (ParserHandle -> IO a) -> ST.ST s a
withParser p io = ST.unsafeIOToST $ withForeignPtr (parserHandle p) $ io . ParserHandle

parseUTF8 :: Parser s -> B.ByteString -> ST.ST s ParseStatus
parseUTF8 p bytes = parse' p $ \h ->
	BU.unsafeUseAsCStringLen bytes $ \(cstr, len) ->
	{# call yajl_parse #} h (castPtr cstr) (fromIntegral len)

parseText :: Parser s -> T.Text -> ST.ST s ParseStatus
parseText p text = parse' p $ \h ->
	withUtf8 text $ \(utf8, len) ->
	{# call yajl_parse #} h utf8 len

-- | Indicate that no more input is available, and parse any remaining
-- buffered input.
-- 
parseComplete :: Parser s -> ST.ST s ParseStatus
parseComplete p = parse' p {# call yajl_parse_complete #}

parse' :: Parser s -> (ParserHandle -> IO CInt) -> ST.ST s ParseStatus
parse' p io = do
	ST.writeSTRef (parserErrorRef p) Nothing
	rc <- blockST $ withParser p io
	case rc of
		0 -> return ParseFinished
		1 -> do
			threw <- ST.readSTRef $ parserErrorRef p
			case threw of
				Nothing -> return ParseCancelled
				Just exc -> throwST exc
		2 -> return ParseContinue
		3 -> ParseError `fmap` getParseError p
		_ -> return $ ParseError . T.pack $ "Unknown 'yajl_status': " ++ show rc

-- | Get the number of bytes consumed from the last input chunk.
-- 
-- Note that if using 'parseText', this corresponds to UTF-8 bytes,
-- /not/ characters.
-- 
-- If the most recent call to 'parseUTF8' or 'parseText' returned
-- 'ParseFinished', this will indicate whether there are any un-parsed
-- bytes past the end of input.
-- 
-- If the most recent parse returned 'ParseError', this will indicate where
-- the error occured.
-- 
getBytesConsumed :: Parser s -> ST.ST s Integer
getBytesConsumed p = withParser p $ \h ->
	toInteger `fmap` {# call yajl_get_bytes_consumed #} h

getParseError :: Parser s -> ST.ST s T.Text
getParseError p = withParser p $ \h -> E.bracket
	({# call yajl_get_error #} h 0 nullPtr 0)
	({# call yajl_free_error #} h)
	(\bytes -> T.pack `fmap` peekCString (castPtr bytes))

data Generator s = Generator
	{ genHandle :: ForeignPtr GenHandle
	, genIndent :: ForeignPtr CChar
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
		handleFP <- newForeignPtr cGenFree handlePtr
		return $ Generator handleFP cIndent

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

blockST :: ST.ST s a -> ST.ST s a
blockST = ST.unsafeIOToST . E.block . ST.unsafeSTToIO
