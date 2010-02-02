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
	, newParser
	, parseUTF8
	, parseText
	, parseComplete
	, getBytesConsumed
	
	-- * Generator
	, Generator
	, GeneratorConfig (..)
	, newGenerator
	, getBuffer
	, clearBuffer
	
	-- ** Generator events
	, generateNull
	, generateBoolean
	, generateNumber
	, generateText
	, generateBeginArray
	, generateEndArray
	, generateBeginObject
	, generateEndObject
	) where
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable (Typeable)

-- import unqualified for C2Hs
import Foreign
import Foreign.C

#include <yajl/yajl_parse.h>
#include <yajl/yajl_gen.h>

data Parser = Parser
	{ parserHandle :: ForeignPtr ParserHandle
	, parserCallbacks :: ForeignPtr ()
	}

-- | Each callback should return 'True' to continue parsing, or 'False'
-- to cancel.
--
data ParserCallbacks = ParserCallbacks
	{ parsedNull :: IO Bool
	, parsedBoolean :: Bool -> IO Bool
	, parsedInteger :: Integer -> IO Bool
	-- TODO: fractional numbers?
	, parsedText :: T.Text -> IO Bool
	, parsedBeginArray :: IO Bool
	, parsedEndArray :: IO Bool
	, parsedBeginObject :: IO Bool
	, parsedAttributeName :: T.Text -> IO Bool
	, parsedEndObject :: IO Bool
	}

{# pointer yajl_handle as ParserHandle newtype #}

newParser :: ParserCallbacks -> IO Parser
newParser callbacks = do
	cCallbacks <- mallocForeignPtrBytes {# sizeof yajl_callbacks #}
	withForeignPtr cCallbacks $ \raw -> do
		-- TODO
		{# set yajl_callbacks->yajl_null #} raw nullFunPtr
		{# set yajl_callbacks->yajl_boolean #} raw nullFunPtr
		{# set yajl_callbacks->yajl_integer #} raw nullFunPtr
		{# set yajl_callbacks->yajl_double #} raw nullFunPtr
		{# set yajl_callbacks->yajl_number #} raw nullFunPtr
		{# set yajl_callbacks->yajl_string #} raw nullFunPtr
		{# set yajl_callbacks->yajl_start_map #} raw nullFunPtr
		{# set yajl_callbacks->yajl_map_key #} raw nullFunPtr
		{# set yajl_callbacks->yajl_end_map #} raw nullFunPtr
		{# set yajl_callbacks->yajl_start_array #} raw nullFunPtr
		{# set yajl_callbacks->yajl_end_array #} raw nullFunPtr
	
	-- TODO
	
	ParserHandle handlePtr <- withForeignPtr cCallbacks $ \ptr ->
		{# call yajl_alloc #} ptr nullPtr nullPtr nullPtr
	parserFP <- newForeignPtr cParserFree handlePtr
	return $ Parser parserFP cCallbacks

foreign import ccall "yajl/yajl_parse.h &yajl_free"
	cParserFree :: FunPtr (Ptr ParserHandle -> IO ())

withParser :: Parser -> (ParserHandle -> IO a) -> IO a
withParser p io = withForeignPtr (parserHandle p) $ io . ParserHandle

parseUTF8 :: Parser -> B.ByteString -> IO ParseStatus
parseUTF8 p bytes =
	withParser p $ \handle ->
	B.useAsCStringLen bytes $ \(cstr, len) ->
	{# call yajl_parse #} handle (castPtr cstr) (fromIntegral len)
	>>= checkParseStatus

{# fun yajl_parse as parseText
	{ withParser* `Parser'
	, withUtf8* `T.Text'&
	} -> `ParseStatus' checkParseStatus* #}

{# fun yajl_parse_complete as parseComplete
	{ withParser* `Parser'
	} -> `ParseStatus' checkParseStatus* #}

{# fun yajl_get_bytes_consumed as getBytesConsumed
	{ withParser* `Parser'
	} -> `Integer' toInteger #}

{# enum yajl_status as RawParseStatus {underscoreToCase} #}

data ParseStatus
	= ParseContinue
	| ParseFinished
	| ParseCancelled
	deriving (Show, Eq)

checkParseStatus :: CInt -> IO ParseStatus
checkParseStatus int = case toEnum $ fromIntegral int of
	YajlStatusOk -> return ParseFinished
	YajlStatusClientCanceled -> return ParseCancelled
	YajlStatusInsufficientData -> return ParseContinue
	YajlStatusError -> E.throwIO $ E.ErrorCall "TODO"

data Generator = Generator
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

data GeneratorError
	= InvalidAttributeName
	| MaximumDepthExceeded
	| GeneratorInErrorState
	| GenerationComplete
	| InvalidNumber
	| NoBuffer
	deriving (Show, Eq, Typeable)

instance E.Exception GeneratorError

{# pointer yajl_gen as GenHandle newtype #}
{# pointer *yajl_gen_config as GenConfig newtype #}

-- | Create a new, empty generator with the given configuration.
-- 
newGenerator :: GeneratorConfig -> IO Generator
newGenerator config = allocaBytes {# sizeof yajl_gen_config #} $ \cConfig -> do
	cIndent <- marshalText $ generatorIndent config
	
	{# set yajl_gen_config->beautify #} cConfig 0
	withForeignPtr cIndent $ {# set yajl_gen_config->indentString #} cConfig
	
	GenHandle handlePtr <- cGenAlloc (GenConfig cConfig) nullPtr
	handleFP <- newForeignPtr cGenFree handlePtr
	return $ Generator handleFP cIndent

marshalText :: T.Text -> IO (ForeignPtr CChar)
marshalText text =
	B.useAsCStringLen (TE.encodeUtf8 text) $ \(temp, len) ->
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

withGenerator :: Generator -> (GenHandle -> IO a) -> IO a
withGenerator gen io = withForeignPtr (genHandle gen) $ io . GenHandle

-- | Retrieve the @NUL@-terminated generator buffer.
-- 
getBuffer :: Generator -> IO B.ByteString
getBuffer gen =
	withGenerator gen $ \handle ->
	alloca $ \bufPtr ->
	alloca $ \lenPtr -> do
	{# call yajl_gen_get_buf #} handle bufPtr lenPtr
	buf <- peek bufPtr
	len <- peek lenPtr
	-- TODO: check that len is < (maxBound :: Int)
	B.packCStringLen (castPtr buf, fromIntegral len)

-- | Clear the generator's output buffer. This does not change the state
-- of the generator.
-- 
{# fun yajl_gen_clear as clearBuffer
	{ withGenerator* `Generator'
	} -> `()' id #}

{# fun yajl_gen_null as generateNull
	{ withGenerator* `Generator'
	} -> `()' checkGenStatus* #}

{# fun yajl_gen_bool as generateBoolean
	{ withGenerator* `Generator'
	, `Bool'
	} -> `()' checkGenStatus* #}

generateNumber :: Num a => Generator -> a -> IO ()
generateNumber gen num =
	withGenerator gen $ \handle ->
	withCStringLen (show num) $ \(cstr, len) ->
	{# call yajl_gen_number #} handle (castPtr cstr) (fromIntegral len)
	>>= checkGenStatus

{# fun yajl_gen_string as generateText
	{ withGenerator* `Generator'
	, withUtf8* `T.Text'&
	} -> `()' checkGenStatus* #}

{# fun yajl_gen_array_open as generateBeginArray
	{ withGenerator* `Generator'
	} -> `()' checkGenStatus* #}

{# fun yajl_gen_array_close as generateEndArray
	{ withGenerator* `Generator'
	} -> `()' checkGenStatus* #}

{# fun yajl_gen_map_open as generateBeginObject
	{ withGenerator* `Generator'
	} -> `()' checkGenStatus* #}

{# fun yajl_gen_map_close as generateEndObject
	{ withGenerator* `Generator'
	} -> `()' checkGenStatus* #}

{# enum yajl_gen_status as GenStatus {underscoreToCase} #}

checkGenStatus :: CInt -> IO ()
checkGenStatus int = case toEnum $ fromIntegral int of
	YajlGenStatusOk -> return ()
	YajlGenKeysMustBeStrings -> E.throwIO InvalidAttributeName
	YajlMaxDepthExceeded -> E.throwIO MaximumDepthExceeded
	YajlGenInErrorState -> E.throwIO GeneratorInErrorState
	YajlGenGenerationComplete -> E.throwIO GenerationComplete
	YajlGenInvalidNumber -> E.throwIO InvalidNumber
	YajlGenNoBuf -> E.throwIO NoBuffer

cFromBool :: Bool -> CInt
cFromBool True = 1
cFromBool False = 0

withUtf8 :: T.Text -> ((Ptr CUChar, CUInt) -> IO a) -> IO a
withUtf8 text io =
	B.useAsCStringLen (TE.encodeUtf8 text) $ \(cstr, len) ->
	io (castPtr cstr, fromIntegral len)
