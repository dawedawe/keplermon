module Conf (
  Conf (..)
, Options (..)
, buildConf
, createDotDir
, lookupConfItem
, getConfItems
, parseArgv
) where

import Control.Monad (unless)
import qualified Data.ConfigFile as CF
import Data.Either.Utils (forceEither)
import Network.Browser
import System.Console.GetOpt
import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
    getAppUserDataDirectory)
import System.FilePath (pathSeparator)

data Conf = Conf {
            opts        :: Options
          , dataUrl     :: String
          , dataPath    :: FilePath
          , proxy       :: Proxy
          }

data Options = Options {
               optVerbose       :: Bool
             , optConfigPath    :: FilePath
             }

instance Show Options where
    show o =
      "-v " ++ show (optVerbose o) ++ "\n" ++
      "-c " ++ optConfigPath o

-- |Default cli options, used if none given.
defaultOptions :: FilePath -> Options
defaultOptions p = Options {
      optVerbose    = False
    , optConfigPath = p ++ [pathSeparator] ++ "keplermon.conf"
    }

options :: [OptDescr (Options -> Options)]
options = [
      Option "v" ["verbose"]
      (NoArg (\optns -> optns { optVerbose = True }))
      "verbose output"
    , Option "c" ["config"]
      (ReqArg (\p optns -> optns {optConfigPath = p }) "PATH")
      "filepath to config"
    ]

-- |Parse the cli argument vector.
parseArgv :: [String] -> IO (Options, [String]) 
parseArgv argv = do
    dDir       <- dotDirPath
    let opt    = getOpt RequireOrder options argv
    let header = "Usage: keplermon [-v] [-c configpath]"
    case opt of
          (o,n,[]  ) -> return (foldl (flip id) (defaultOptions dDir) o, n)
          (_,_,errs) -> ioError
            (userError (concat errs ++ usageInfo header options))

-- |Build Conf out of cli options and config file definitions.
buildConf :: Options -> IO Conf
buildConf o = do
    items     <- getConfItems $ optConfigPath o
    let prx   = getProxyConf items
    let url   = lookupConfItem "url" items
    let dPath = lookupConfItem "datapath" items
    return $ Conf o url dPath prx

-- |Read config items out of config file.
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

-- |Lookup item in tuple list and return it's value.
lookupConfItem :: String -> [(CF.OptionSpec, String)] -> String
lookupConfItem itemName items = 
    let value = lookup itemName items
    in  checkConfItem value itemName

-- |Fail with error if ConfItem can't be parsed.
checkConfItem :: Maybe String -> String -> String
checkConfItem (Just s) _       = s
checkConfItem Nothing itemName = error $ "failed to parse " ++ itemName

-- |Create dotdir with a default config file.
createDotDir :: IO ()
createDotDir = do
    dDir   <- dotDirPath
    exists <- doesDirectoryExist dDir
    createDirectoryIfMissing (not exists) dDir
    let cPath = dDir ++ [pathSeparator] ++ "keplermon.conf"
    unless exists $ writeFile cPath (defaultConf dDir)

-- |Default path to application data directory aka the dotdir.
dotDirPath :: IO String
dotDirPath = getAppUserDataDirectory "keplermon"

defaultConf :: FilePath -> String
defaultConf dDir =
    "url = http://www.nasa.gov/mission_pages/kepler/main/index.html\n" ++
    "datapath = " ++ dDir ++ [pathSeparator]  ++ "keplermon.data\n" ++
    "# proxy = 127.0.0.1:8118"
