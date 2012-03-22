module Conf
( Conf (..)
, Options (..)
, buildConf
, checkConfItem
, getConfItems
, parseArgv
) where

import qualified Data.ConfigFile as CF
import Data.Either.Utils (forceEither)
import Network.Browser
import System.Console.GetOpt

data Conf = Conf
	{ opts		:: Options
	, dataUrl	:: String
	, dataPath	:: FilePath
	, proxy		:: Proxy
	}

data Options = Options
	{ optVerbose	:: Bool
	, optConfigPath	:: FilePath
	}

instance Show Options where
	show o =
	  "-v " ++ (show $ optVerbose o) ++ "\n" ++
	  "-C " ++ (optConfigPath o)

defaultOptions :: Options
defaultOptions = Options
	{ optVerbose	= False
	, optConfigPath = "./.keplermon.conf"
	}

options :: [OptDescr (Options -> Options)]
options =
	[ Option ['v'] ["verbose"]
	  (NoArg (\optns -> optns { optVerbose = True }))
	  "verbose output"
	, Option ['C'] ["config"]
	  (ReqArg (\p optns -> optns {optConfigPath = p }) "PATH")
	  "filepath to config"
	]

parseArgv :: [String] -> IO (Options, [String]) 
parseArgv argv =
	let opt = getOpt RequireOrder options argv
	in case opt of
	  (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
	  (_,_,errs) -> ioError
	    (userError (concat errs ++ usageInfo header options))
      	where header = "Usage: keplermon [-v] [-C configpath]"

buildConf :: Options -> IO Conf
buildConf o = do
	items    <- getConfItems $ optConfigPath o
	let prx  = getProxyConf items
	let url  = lookup "url" items
	let url' = checkConfItem url "url"
	let dat  = lookup "datapath" items
	let dat' = checkConfItem dat "datapath"
	return $ Conf o url' dat' prx

getConfItems :: FilePath -> IO [(CF.OptionSpec, String)]
getConfItems path = do
	val    <- CF.readfile CF.emptyCP path
	let cp = forceEither val
	return $ forceEither $ CF.items cp "DEFAULT"

getProxyConf :: [(CF.OptionSpec, String)] -> Proxy
getProxyConf items = do
	maybeToProxy $ lookup "proxy" items
	where
	  maybeToProxy :: Maybe String -> Proxy
	  maybeToProxy Nothing  = NoProxy
	  maybeToProxy (Just s) = Proxy s Nothing

checkConfItem :: Maybe String -> String -> String
checkConfItem (Just s) _ = s
checkConfItem Nothing itemName = error $ "failed to parse " ++ itemName

