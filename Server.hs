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
-- Module      : Server
-- Note        : Main module
-- 
-- 
-- 
--------------------------------------------------------------------------------
module Main (main) where

import System.IO
import Data.Maybe
import Control.Concurrent
import Control.Exception
import Network.Socket
import System.Environment
import Settings
import Parser
import Request


main = withSocketsDo $ do
    args <- getArgs
    prog <- getProgName
    let usage = getUsage prog
    let s = parseSettings args 
    evalSettings s usage

    addrs <- getAddrInfo Nothing (Just (settHost s)) (Just (show $ settPort s))
    let addr = head addrs
    sock <- socket (addrFamily addr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr) 
    listen sock 20
    mainLoop s sock

mainLoop :: Settings -> Socket -> IO ()
mainLoop s sock = do
    conn <- accept sock
    forkIO (runConn s conn)
    mainLoop s sock

runConn  :: Settings -> (Socket, SockAddr) -> IO ()
runConn settings (sock, _) = do
    bracket (socketToHandle sock ReadWriteMode) (hClose) (\hdl -> do
            hSetBuffering hdl (BlockBuffering (Just $ 8*1024))
            catch (lineLoop hdl) handleIOEx
        )
  where
    --ignore IOException
    --IOExceptions occur when the client
    --disconnects
    handleIOEx :: IOException -> IO ()
    handleIOEx _ = return ()
    lineLoop hdl = do
        line <- hGetLine hdl
        case reads line :: [(Url,String)] of
            [(url, _)] -> do
                let lines = []
                lines <- readLines hdl lines
                let req = Request { reqUrl = url, reqHeaders = 
                                    (parseHeaders $ reverse lines) }

                if reqContentLength req > 1024*1024 then
                    --body is to big, abort
                    return ()
                else do
                    req <- downBody hdl req
                    handleRequest settings hdl req

                    hFlush hdl

                    --keep connection alive
                    --for next request
                    if reqConnection req then
                        lineLoop hdl
                    else
                        return ()
            _ -> return ()
    readLines :: Handle -> [String] -> IO [String]
    readLines hdl lines = do
        line <- hGetLine hdl
        if line == "\r" then
            return lines
        else do
            res <- (readLines hdl $ (init line):lines)
            return res
    readLen :: Handle -> Integer -> IO String
    readLen hdl len = do
        if len > 0 then do
            c <- hGetChar hdl
            rest <- readLen hdl (len-1)
            let ret = c:rest
            return ret
        else
            return []
    downBody hdl req = do
        let len = reqContentLength req

        if len > 0 then do
            body <- readLen hdl len

            if urlMethod (reqUrl req) == POST && 
                takeWhile (/=';') (reqContentType req) == "application/x-www-form-urlencoded" then do
                let params = (urlParams $ reqUrl req) ++ (parseParams body)
                let url = (reqUrl req) { urlParams = params }
                return req { reqUrl = url }
            else
                return req
        else
            return req
