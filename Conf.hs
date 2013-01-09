module Conf (
  Conf (..)
, Options (..)
, buildConf
, lookupConfItem
, getConfItems
, parseArgv
) where

import qualified Data.ConfigFile as CF
import Data.Either.Utils (forceEither)
import Network.Browser
import System.Console.GetOpt
import System.Directory (getAppUserDataDirectory)
import System.FilePath (pathSeparator)

data Conf = Conf {
            opts        :: Options
          , dataUrl     :: String
          , proxy       :: Proxy
          }

data Options = Options {
               optVerbose       :: Bool
             , optConfigPath    :: FilePath
             , optDataPath      :: FilePath
             }

instance Show Options where
    show o =
      "-v " ++ show (optVerbose o) ++ "\n" ++
      "-C " ++ optConfigPath o ++ "\n" ++
      "-d " ++ optDataPath o

defaultOptions :: FilePath -> Options
defaultOptions p = Options {
      optVerbose    = False
    , optConfigPath = p ++ [pathSeparator] ++ "keplermon.conf"
    , optDataPath   = p ++ [pathSeparator] ++ "keplermon.data"
    }

options :: [OptDescr (Options -> Options)]
options = [
      Option "v" ["verbose"]
      (NoArg (\optns -> optns { optVerbose = True }))
      "verbose output"
    , Option "C" ["config"]
      (ReqArg (\p optns -> optns {optConfigPath = p }) "PATH")
      "filepath to config"
    , Option "d" ["data"]
      (ReqArg (\p optns -> optns {optDataPath = p }) "PATH")
      "filepath to data"
    ]

parseArgv :: [String] -> IO (Options, [String]) 
parseArgv argv = do
    dDir       <- getAppUserDataDirectory "keplermon"
    let opt    = getOpt RequireOrder options argv
    let header = "Usage: keplermon [-v] [-C configpath] [-d datapath]"
    case opt of
          (o,n,[]  ) -> return (foldl (flip id) (defaultOptions dDir) o, n)
          (_,_,errs) -> ioError
            (userError (concat errs ++ usageInfo header options))

buildConf :: Options -> IO Conf
buildConf o = do
    items    <- getConfItems $ optConfigPath o
    let prx  = getProxyConf items
    let url  = lookupConfItem "url" items
    return $ Conf o url prx

getConfItems :: FilePath -> IO [(CF.OptionSpec, String)]
getConfItems path = do
    val    <- CF.readfile CF.emptyCP path
    let cp = forceEither val
    return $ forceEither $ CF.items cp "DEFAULT"

getProxyConf :: [(CF.OptionSpec, String)] -> Proxy
getProxyConf items = maybeToProxy $ lookup "proxy" items
    where
      maybeToProxy :: Maybe String -> Proxy
      maybeToProxy Nothing  = NoProxy
      maybeToProxy (Just s) = Proxy s Nothing

lookupConfItem :: String -> [(CF.OptionSpec, String)] -> String
lookupConfItem itemName items = 
    let value = lookup itemName items
    in  checkConfItem value itemName

checkConfItem :: Maybe String -> String -> String
checkConfItem (Just s) _       = s
checkConfItem Nothing itemName = error $ "failed to parse " ++ itemName

