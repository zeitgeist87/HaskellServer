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
-- Module      : Request
-- Note        : 
-- 
-- Request data structure and handling
-- 
--------------------------------------------------------------------------------
module Request ( Request (..),
        reqConnection,
        reqContentType,
        reqContentLength,
        handleRequest
        ) where

import System.IO
import System.Directory
import System.FilePath
import Control.Exception
import Data.Time
import System.Locale
import Data.Time.Clock.POSIX
import Parser
import Settings

data Request = Request {
    reqUrl :: Url,
    reqHeaders :: [Header]
    }
    deriving (Show)

reqConnection Request {reqHeaders = headers } = lookupHeader headers
  where
    lookupHeader ((HdrConnection b):_) = b
    lookupHeader (x:xs) = lookupHeader xs
    lookupHeader [] = False

reqContentType Request {reqHeaders = headers } = lookupHeader headers
  where
    lookupHeader ((HdrContentType s):_) = s
    lookupHeader (x:xs) = lookupHeader xs
    lookupHeader [] = ""

reqContentLength Request {reqHeaders = headers } = lookupHeader headers
  where
    lookupHeader ((HdrContentLength i):_) = i
    lookupHeader (x:xs) = lookupHeader xs
    lookupHeader [] = 0

reqIfModifiedSince Request {reqHeaders = headers } = lookupHeader headers
  where
    lookupHeader ((HdrIfModifiedSince i):_) = i
    lookupHeader (x:xs) = lookupHeader xs
    lookupHeader [] = 0

reqRange Request {reqHeaders = headers } = lookupHeader headers
  where
    lookupHeader ((HdrRange i):_) = i
    lookupHeader (x:xs) = lookupHeader xs
    lookupHeader [] = 0

handleNotFound :: Handle -> Request -> IO ()
handleNotFound hdl req = 
    hPutStr hdl $ "HTTP/" ++ (show $ urlVersion (reqUrl req)) ++ " 404 Not Found\r\n\
        \Content-Length: 24\r\n\
        \Content-Type: text/plain;charset=UTF-8\r\n\r\n\
        \Sorry! File not Found..."

sendHttp hdl req msg = do
    let size = length msg
    hPutStr hdl $ "HTTP/" ++ (show $ urlVersion (reqUrl req)) ++ " 200 OK\r\n\
        \Content-Type: text/plain;charset=UTF-8\r\n\
        \Content-Length: "
    hPutStr hdl (show size)
    hPutStr hdl "\r\n\r\n"
    hPutStr hdl msg



echoRequest :: Handle -> Request -> IO ()
echoRequest hdl req = do
    let msg = show req
    sendHttp hdl req msg
    
welcome :: Handle -> Request -> IO () 
welcome hdl req = do
    let params = urlParams $ reqUrl req
    let fname = maybe "<empty>" (\n -> n) $ lookup "firstname" params
    let lname = maybe "<empty>" (\n -> n) $ lookup "lastname" params
    let msg = "Welcome " ++ fname ++ " " ++ lname
    sendHttp hdl req msg

handleDyn :: Settings -> Handle -> Request -> String -> IO ()
handleDyn s hdl req name
    | name == "echo" = echoRequest hdl req
    | name == "welcome" = welcome hdl req
    | otherwise = return ()


handleRequest :: Settings -> Handle -> Request -> IO ()
handleRequest s hdl req = do
    path <- getPath
    let ext = takeExtensions path
    if ext == ".hs" then
        handleDyn s hdl req  $ dropExtensions $ takeFileName path
    else
        catch (uploadFile path) handleEx 
  where
    convDate = fromInteger . round . utcTimeToPOSIXSeconds
    handleEx :: IOException -> IO ()
    handleEx _ =  handleNotFound hdl req
    httpVersion req = urlVersion (reqUrl req)
    uploadFile path = do
        t <- getModificationTime path
        let modtime = convDate t
        let modsince = reqIfModifiedSince req
        if modtime <= modsince then
            hPutStr hdl $ "HTTP/" ++ (show $ httpVersion req) ++ " 304 Not Modified\r\n\r\n"
        else
            withBinaryFile path ReadMode (sendFile t path)
    sendFile t path inhdl = do
        size <- hFileSize inhdl
        let mime = getMime $ takeExtensions path
        let date = formatTime defaultTimeLocale
                        rfc822DateFormat t
        let range = reqRange req
        if range > 0 && range < size then do
            hSeek inhdl AbsoluteSeek range
            content <- hGetContents inhdl
            hPutStr hdl $ "HTTP/" ++ (show $ httpVersion req) ++ " 206 Partial Content\r\n"
            hPutStr hdl $ "Content-Length: " ++ (show $ size-range)
            hPutStr hdl "\r\n"
            hPutStr hdl $ "Content-Type: " ++ mime
            hPutStr hdl "\r\n"
            hPutStr hdl $ "Last-Modified: " ++ date
            hPutStr hdl "\r\n"
            hPutStr hdl $ "Accept-Ranges: bytes\r\n"
            hPutStr hdl $ "Content-Range: bytes " ++ (show range) ++ "-" ++ (show $ size - 1) ++
                                                "/" ++ (show size)
            hPutStr hdl "\r\n\r\n"
            hPutStr hdl content
        else do
            content <- hGetContents inhdl
            hPutStr hdl $ "HTTP/" ++ (show $ httpVersion req) ++ " 200 OK\r\n"
            hPutStr hdl $ "Content-Length: " ++ (show size)
            hPutStr hdl "\r\n"
            hPutStr hdl $ "Content-Type: " ++ mime
            hPutStr hdl "\r\n"
            hPutStr hdl $ "Accept-Ranges: bytes\r\n"
            hPutStr hdl $ "Last-Modified: " ++ date
            hPutStr hdl "\r\n\r\n"
            hPutStr hdl content
    getPath = do
        let p = normalise $ (settPubDir s) </> (urlPath $ reqUrl req)
        pexist <- doesDirectoryExist p
        if pexist then do
            let np = (dropTrailingPathSeparator p) </> "index.html"
            return np
        else
            return p
    getMime ext
        | ext == ".html" || ext == ".htm" = "text/html;charset=UTF-8"
        | ext == ".css" = "text/css"
        | ext == ".js" = "text/javascript;charset=UTF-8"
        | ext == ".jpg" || ext == ".jpeg" = "image/jpeg"
        | ext == ".txt" = "text/plain;charset=UTF-8"
        | ext == ".ico" = "image/vnd.microsoft.icon"
        | ext == ".png" = "image/png"
        | otherwise = "application/octet"
