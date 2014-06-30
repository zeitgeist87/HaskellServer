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
-- Module      : Settings
-- Note        : Contains code to parse command line arguments
-- 
-- Contains helper functions to parse command line
-- 
--------------------------------------------------------------------------------
module Settings (Settings (..), parseSettings, evalSettings, getUsage) where

import System.Console.GetOpt
import Data.Maybe
import System.Exit
import System.FilePath

data Flag = Version | Host String | Port String | PubDir String | Help

data Settings = Settings {
    settHost :: String,
    settPort :: Int,
    settPubDir  :: String,
    settVersion :: Bool,
    settMsg :: Maybe String
    }
        deriving (Show)

defaultSettings = Settings {
    settHost = "localhost",
    settPort = 8080,
    settPubDir = "public",
    settVersion = False,
    settMsg = Nothing
  }

options :: [OptDescr Flag]
options = [
      Option ['v'] ["version"] (NoArg Version)        "show version number",
      Option ['n'] ["host"]    (ReqArg Host "NAME")   "hostname to bind to",
      Option ['p'] ["port"]    (ReqArg Port "INT")    "port number",
      Option ['d'] ["dir"]    (ReqArg PubDir "DIR")    "path to public directory",
      Option ['h'] ["help"]    (NoArg Help)           "display this help"
    ]


getUsage :: String -> String
getUsage prog = usageInfo (usageHeader prog) options
  where
    usageHeader prog = "Usage: " ++ prog ++ " [OPTION...]"


processFlag :: Flag -> Settings -> Settings
processFlag Version s = s { settVersion = True }
processFlag (Host h)  s = s { settHost = h }
processFlag (Port p)  s = case reads p :: [(Int,String)] of
        (x, _):_  -> validatePort p x s 
        _         -> s { settMsg = Just ("Unable to parse port: "
                            ++ p ++ "\n") }
  where
    validatePort p x s 
        | x > 0 && x < 65536 = s { settPort = x }
        | otherwise          = s { settMsg = Just ("Invalid port number: "
                                            ++ p ++ "\n") }
processFlag Help s = s { settMsg = Just "" }
processFlag (PubDir d) s = s { settPubDir = (validateDir d) }
  where
    validateDir = makeValid . normalise . dropTrailingPathSeparator
    

parseSettings :: [String] -> Settings
parseSettings args = case getOpt Permute options args of
    (flags, [] , []) -> foldr ($) defaultSettings (map processFlag flags)
    (_, nonOpts, []) -> defaultSettings { settMsg = Just ("Unrecognized arguments: " 
                                        ++ unwords nonOpts ++ "\n") }
    (_, _, msgs)     -> defaultSettings { settMsg = Just (concat msgs) }



evalSettings :: Settings -> String -> IO ()
evalSettings Settings { settMsg = Just "" } u = do
    putStrLn u
    exitWith ExitSuccess
evalSettings Settings { settMsg = Just m } u = do
    putStrLn $ m ++ u
    exitWith $ ExitFailure 0
evalSettings Settings { settVersion = True } _ = do
    putStrLn "Simple HTTP Server 0.1"
    exitWith ExitSuccess
evalSettings _ _ = return ()

