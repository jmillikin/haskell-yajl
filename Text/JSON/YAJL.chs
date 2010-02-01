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
	, parse
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

#include <yajl/yajl_gen.h>

data Parser = Parser

data ParserCallbacks = ParserCallbacks
	{ parsedNull :: IO ()
	, parsedBoolean :: Bool -> IO ()
	, parsedInteger :: Integer -> IO ()
	-- TODO: fractional numbers?
	, parsedText :: T.Text -> IO ()
	, parsedBeginArray :: IO ()
	, parsedEndArray :: IO ()
	, parsedBeginObject :: IO ()
	, parsedAttributeName :: T.Text -> IO ()
	, parsedEndObject :: IO ()
	}

newParser :: ParserCallbacks -> IO Parser
newParser = undefined

parse :: Parser -> B.ByteString -> IO ()
parse = undefined

parseText :: Parser -> T.Text -> IO ()
parseText = undefined

parseComplete :: Parser -> IO ()
parseComplete = undefined

getBytesConsumed :: Parser -> IO Integer
getBytesConsumed = undefined

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
	} -> `()' checkStatus* #}

{# fun yajl_gen_bool as generateBoolean
	{ withGenerator* `Generator'
	, `Bool'
	} -> `()' checkStatus* #}

generateNumber :: Num a => Generator -> a -> IO ()
generateNumber gen num =
	withGenerator gen $ \handle ->
	withCStringLen (show num) $ \(cstr, len) ->
	{# call yajl_gen_number #} handle (castPtr cstr) (fromIntegral len)
	>>= checkStatus

generateText :: Generator -> T.Text -> IO ()
generateText gen text =
	withGenerator gen $ \handle ->
	B.useAsCStringLen (TE.encodeUtf8 text) $ \(cstr, len) ->
	{# call yajl_gen_string #} handle (castPtr cstr) (fromIntegral len)
	>>= checkStatus

{# fun yajl_gen_array_open as generateBeginArray
	{ withGenerator* `Generator'
	} -> `()' checkStatus* #}

{# fun yajl_gen_array_close as generateEndArray
	{ withGenerator* `Generator'
	} -> `()' checkStatus* #}

{# fun yajl_gen_map_open as generateBeginObject
	{ withGenerator* `Generator'
	} -> `()' checkStatus* #}

{# fun yajl_gen_map_close as generateEndObject
	{ withGenerator* `Generator'
	} -> `()' checkStatus* #}

{# enum yajl_gen_status as GenStatus {underscoreToCase} #}

checkStatus :: CInt -> IO ()
checkStatus int = case toEnum $ fromIntegral int of
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
