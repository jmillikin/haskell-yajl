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
	, clear
	
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
import Data.ByteString (ByteString)
import Data.Text (Text)

data Parser = Parser

data ParserCallbacks = ParserCallbacks
	{ parsedNull :: IO ()
	, parsedBoolean :: Bool -> IO ()
	, parsedInteger :: Integer -> IO ()
	-- TODO: fractional numbers?
	, parsedText :: Text -> IO ()
	, parsedBeginArray :: IO ()
	, parsedEndArray :: IO ()
	, parsedBeginObject :: IO ()
	, parsedAttributeName :: Text -> IO ()
	, parsedEndObject :: IO ()
	}

newParser :: ParserCallbacks -> IO Parser
newParser = undefined

parse :: Parser -> ByteString -> IO ()
parse = undefined

parseText :: Parser -> Text -> IO ()
parseText = undefined

parseComplete :: Parser -> IO ()
parseComplete = undefined

getBytesConsumed :: Parser -> IO Integer
getBytesConsumed = undefined

data Generator = Generator

data GeneratorConfig = GeneratorConfig
	{ generatorBeautify :: Bool
	, generatorIndent :: Maybe Text
	}

newGenerator :: GeneratorConfig -> IO Generator
newGenerator = undefined

getBuffer :: Generator -> IO ByteString
getBuffer = undefined

clear :: Generator -> IO ()
clear = undefined

generateNull :: Generator -> IO ()
generateNull = undefined

generateBoolean :: Generator -> Bool -> IO ()
generateBoolean = undefined

generateNumber :: Num a => Generator -> a -> IO ()
generateNumber = undefined

generateText :: Generator -> Text -> IO ()
generateText = undefined

generateBeginArray :: Generator -> IO ()
generateBeginArray = undefined

generateEndArray :: Generator -> IO ()
generateEndArray = undefined

generateBeginObject :: Generator -> IO ()
generateBeginObject = undefined

generateEndObject :: Generator -> IO ()
generateEndObject = undefined
