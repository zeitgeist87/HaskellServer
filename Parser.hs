-- HaskellServer - Minimalistic webserver written in Haskell
-- Copyright (C) 2014  Andreas Rohner
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

--------------------------------------------------------------------------------
-- | 
-- Module      : Parser
-- Note        : 
-- 
-- Functions to parse HTTP Requests
-- 
--------------------------------------------------------------------------------
module Parser (Header (..), 
    Method (..), 
    Url (..), 
    Encoding (..), 
    parseHeaders,
    parseParams,
    urlDecode
    ) where

import Data.List
import Data.Char
import Data.Time
import Data.Time.Clock.POSIX
import Data.Maybe
import System.Locale


data Method = GET | HEAD | POST | PUT | DELETE | TRACE
                deriving (Eq, Ord, Read, Show)

data Url = Url {
    urlMethod :: Method,
    urlPath :: String,
    urlVersion :: Float,
    urlParams :: [(String, String)]
    }
                deriving (Show)

instance Read Url where
    readsPrec _ = parseUrl

data Encoding = Chunked | Compress | Deflate | Gzip | Trailers
                    deriving (Eq, Read, Show)

type Timestamp = Integer

data Header =  HdrCacheControl String
    | HdrConnection Bool
    | HdrContentLength Integer
    | HdrContentType String
    | HdrDate Timestamp
    | HdrAccept [String]
    | HdrAcceptCharset [String]
    | HdrAcceptEncoding [Encoding]
    | HdrAcceptLanguage [String]
    | HdrCookie String
    | HdrHost String
    | HdrIfModifiedSince Timestamp
    | HdrIfUnmodifiedSince Timestamp
    | HdrRange Integer
    | HdrReferer String
    | HdrUserAgent String
        deriving(Eq, Show)



headerMap = 
    [ ("connection",  parseConnection),
    ("content-length", parseInt HdrContentLength),
    ("content-type", parseString HdrContentType),
    ("date", parseDate HdrDate),
    ("accept", parseStrings HdrAccept),
    ("accept-charset", parseStrings HdrAcceptCharset),
    ("accept-encoding", parseEncodings HdrAcceptEncoding),
    ("accept-language", parseStrings HdrAcceptLanguage),
    ("cookie", parseString HdrCookie),
    ("host", parseString HdrHost),
    ("if-modified-since", parseDate HdrIfModifiedSince),
    ("if-unmodified-since", parseDate HdrIfUnmodifiedSince),
    ("range", parseInt HdrRange),
    ("referer", parseString HdrReferer),
    ("user-agent", parseString HdrUserAgent)
    ]
  where
    parseConnection (f:_) = Just (HdrConnection (toLower f == 'k'))
    convDate = fromInteger . round . utcTimeToPOSIXSeconds
    parseDate t date = maybe Nothing (\d -> Just (t (convDate d))) (parseWebDate date)
    parseWebDate date = parseTime defaultTimeLocale rfc822DateFormat
                        (ltrim date) :: Maybe UTCTime
    parseInt t s = case reads (dropWhile (not . isDigit) s) :: [(Integer, String)] of
        [(v, _)] -> Just (t v)
        _        -> Nothing
    parseString t s = Just (t (ltrim s))
    parseStrings t s = case filter (/=[]) (wordsWhen sepList s) of
        [] -> Nothing
        list -> Just (t (filter (/="") $ map (takeWhile (/=';')) list))
    parseEncoding (x:xs) = case reads ((toUpper x):xs) :: [(Encoding,String)] of
        [(e, _)] -> Just e
        _        -> Nothing
    parseEncodings t "*" = Just (t [Chunked, Compress, Deflate, Gzip, Trailers])
    parseEncodings t s = case filter (/=[]) (wordsWhen sepList s) of
        [] -> Nothing
        list -> case catMaybes $ map parseEncoding list of
            [] -> Nothing
            l  -> Just (t l)


ltrim = dropWhile isSpace
sepList c = c == ',' || isSpace c
--from http://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


parseUrl :: ReadS Url
parseUrl line = case words line of
    [method, '/' : url, 'H':'T':'T':'P':'/':ver] -> 
        case reads method :: [(Method, String)] of
            [(m,"")] -> 
                case reads ver :: [(Float, String)] of
                    [(v,rs)] -> [(Url m (path url) v (params url), rs)]
                    _       -> []
            _        -> []
    _ -> []
  where
    path = relpath . urlDecode . takeWhile (/='?')
    params url = case dropWhile (/='?') url of
        ""      -> []
        '?':us  -> parseParams us
    relpath [] = []
    relpath ('.':'.':xs) = relpath ('.':xs)
    relpath (x:xs) = x : (relpath xs)

urlDecode ('%':x:y:xs)
    | isHexDigit x && isHexDigit y = (chr $ (digitToInt x) * 16 + (digitToInt y)):
                                        (urlDecode xs)
    | otherwise = '%': (urlDecode (x:y:xs))
urlDecode ('+':xs) = ' ' : urlDecode xs
urlDecode (x:xs) = x : urlDecode xs
urlDecode [] = []

parseParams = map (fixtupl . break (=='=')) . wordsWhen (=='&')
  where
    fixtupl (n, "") = (n, "")
    fixtupl (n, v) = (n, (urlDecode . tail) v)




parseHeaders :: [String] -> [Header]
parseHeaders hs = catMaybes $ map parseHeader (concatHLines hs)
  where
    concatHLines [] = []
    concatHLines [x] = [x]
    concatHLines (x:(c:cs):xs)
        | c == ' ' || c == '\t' = concatHLines $ (x ++ ' ':(ltrim cs)) : xs
        | otherwise = x : (c:cs) : (concatHLines xs)
    parseHeader :: String -> Maybe Header
    parseHeader line = case break (== ':') (ltrim line) of
        ([], _)       -> Nothing
        (_, [])       -> Nothing
        (name, ':':value) -> maybe Nothing (\p -> p (ltrim value) ) 
                            (lookup (map toLower name) headerMap)

