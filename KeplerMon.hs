module KeplerMon (
  AstroCounts (..)
, getAndPrintCounts
, getCurrentCounts
) where

import Control.Monad (unless)
import Data.Time
import System.Directory (doesFileExist)
import Network.HTTP
import Network.Browser
import Text.HTML.TagSoup

import Conf

data AstroCounts = AstroCounts {
                   timeStamp        :: UTCTime
                 , confirmedPlanets :: Int
                 , planetCandidates :: Int
                 , eclipsingBiStars :: Int
                 }

-- |Scrape the web page and print the stats to stdout.
getAndPrintCounts :: Conf -> IO ()
getAndPrintCounts conf = do
    let dPath = dataPath conf
    curCounts <- getCurrentCounts (proxy conf) (dataUrl conf)
    initDataFileIfNeeded dPath curCounts
    oldCounts <- readCounts dPath
    let diffs = diffCounts oldCounts curCounts
    let info  = addDiffsToCounts curCounts diffs
    putStrLn $ buildDisplayString (timeStamp oldCounts) info
    writeCounts dPath curCounts

-- |Build a string for displaying purposes of the counts and the diffs.
buildDisplayString :: UTCTime -> [String] -> String
buildDisplayString oldTime [countdiff0, countdiff1, countdiff2] =
    "Confirmed Planets:      " ++ countdiff0 ++ "\n" ++
    "Planet Candidates:      " ++ countdiff1 ++ "\n" ++
    "Eclipsing Binary Stars: " ++ countdiff2 ++ "\n" ++
    "compared to old data from " ++ show oldTime
buildDisplayString _ _ = error "undefined arguments for buildDisplayString"

-- |Add diff strings to the stats of an AstroCounts.
addDiffsToCounts :: AstroCounts -> [String] -> [String]
addDiffsToCounts (AstroCounts _ cp pc ebs) =
    zipWith helper [show cp, show pc, show ebs]
    where
      helper :: String -> String -> String
      helper x y = x ++ "\t" ++ y

-- |Diff the stats of two AstroCounts.
diffCounts :: AstroCounts -> AstroCounts -> [String]
diffCounts old new =
    map enrichDiff [cpdiff, pcdiff, ebsdiff]
    where
      cpdiff  = confirmedPlanets new - confirmedPlanets old
      pcdiff  = planetCandidates new - planetCandidates old
      ebsdiff = eclipsingBiStars new - eclipsingBiStars old

enrichDiff :: Int -> String
enrichDiff n
    | n > 0     = "(+" ++ show n ++ ")"
    | otherwise = "(" ++ show n ++ ")"

-- |Use proxy or not to scrape the counts from the page at the given url.
getCurrentCounts :: Proxy -> String -> IO AstroCounts
getCurrentCounts prox url = do
    tags   <- fmap parseTags $ getPage prox url
    tstamp <- getCurrentTime
    let counts       = partitions (~== TagText "Confirmed Planets: ") tags
    let confPlanets  = read $ filter (/= ',') (fromTagText (head counts !! 2))
    let planCandits  = read $ filter (/= ',') (fromTagText (head counts !! 8))
    let eclipBiStars = read $ filter (/= ',') (fromTagText (head counts !! 14))
    return (AstroCounts tstamp confPlanets planCandits eclipBiStars)

getPage :: Proxy -> String -> IO String
getPage prox url = do
    (_,rsp) <- Network.Browser.browse $ do
      setProxy prox
      setOutHandler $ const (return ()) -- no debug output
      request (getRequest url)
    return (rspBody rsp)

-- |Write counts to path if it doesn't exists already.
initDataFileIfNeeded :: FilePath -> AstroCounts -> IO ()
initDataFileIfNeeded path counts = do
    e <- doesFileExist path
    unless e (writeCounts path counts)

-- |Write a name = value file with the given counts.
writeCounts :: FilePath -> AstroCounts -> IO ()
writeCounts path (AstroCounts ts cp pc ebs) =
    let countString = "timestamp = " ++ show ts ++ "\n" ++
                      "confirmed_planets = " ++ show  cp ++ "\n" ++
                      "planet_candidates = " ++ show  pc ++ "\n" ++
                      "eclipsing_binary_stars = " ++ show ebs ++ "\n"
    in writeFile path countString

-- |Parse a name = value file to an AstroCounts
readCounts :: FilePath -> IO AstroCounts
readCounts path = do
    countItems <- getConfItems path
    let t = read $ lookupConfItem "timestamp" countItems
    let c = read $ lookupConfItem "confirmed_planets" countItems
    let p = read $ lookupConfItem "planet_candidates" countItems
    let e = read $ lookupConfItem "eclipsing_binary_stars" countItems
    return (AstroCounts t c p e)

