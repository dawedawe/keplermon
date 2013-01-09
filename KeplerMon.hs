module KeplerMon (
  getAndPrintCounts
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

getAndPrintCounts :: Conf -> IO ()
getAndPrintCounts conf = do
    curCounts <- getCurrentCounts (proxy conf) (dataUrl conf)
    initDataFileIfNeeded (dataPath conf) curCounts
    oldCounts <- readOldCounts $ dataPath conf
    let diffs = diffOldNewCounts oldCounts curCounts
    let info  = appDiffsToCounts curCounts diffs
    putStrLn $ buildDisplayString (timeStamp oldCounts) info
    writeCurrentCounts (dataPath conf) curCounts

buildDisplayString :: UTCTime -> [String] -> String
buildDisplayString oldTime [countdiff0, countdiff1, countdiff2] =
    dispString
    where
      dispString = "Confirmed Planets:      " ++ countdiff0 ++ "\n" ++
                   "Planet Candidates:      " ++ countdiff1 ++ "\n" ++
                   "Eclipsing Binary Stars: " ++ countdiff2 ++ "\n" ++
                   "compared to old data from " ++ show oldTime
buildDisplayString _ _ = error "undefined arguments for buildDisplayString"

appDiffsToCounts :: AstroCounts -> [String] -> [String]
appDiffsToCounts (AstroCounts _ cp pc ebs) =
    zipWith helper [show cp, show pc, show ebs]
    where
      helper :: String -> String -> String
      helper x y = x ++ "\t" ++ y

diffOldNewCounts :: AstroCounts -> AstroCounts -> [String]
diffOldNewCounts old new =
    map enrichDiff [cpdiff, pcdiff, ebsdiff]
    where
      cpdiff  = confirmedPlanets new - confirmedPlanets old
      pcdiff  = planetCandidates new - planetCandidates old
      ebsdiff = eclipsingBiStars new - eclipsingBiStars old

enrichDiff :: Int -> String
enrichDiff n
    | n > 0     = "(+" ++ show n ++ ")"
    | otherwise = "(" ++ show n ++ ")"

getCurrentCounts :: Proxy -> String -> IO AstroCounts
getCurrentCounts prox url = do
    tags   <- fmap parseTags $ getPage prox url
    tstamp <- getCurrentTime
    let counts = partitions (~== "<div id=\"ullitags\"") tags
    let confPlanets  = read $ filterComma (fromTagText (head counts !! 3))
    let planCandits  = read $ filterComma (fromTagText (head counts !! 9))
    let eclipBiStars = read $ filterComma (fromTagText (head counts !! 15))
    return (AstroCounts tstamp confPlanets planCandits eclipBiStars)

getPage :: Proxy -> String -> IO String
getPage prox url = do
    (_,rsp) <- Network.Browser.browse $ do
      setProxy prox
      setOutHandler $ const (return ()) -- no debug output
      request (getRequest url)
    return (rspBody rsp)

initDataFileIfNeeded :: FilePath -> AstroCounts -> IO ()
initDataFileIfNeeded path counts = do
    e <- doesFileExist path
    unless e (writeCurrentCounts path counts)

writeCurrentCounts :: FilePath -> AstroCounts -> IO ()
writeCurrentCounts path (AstroCounts ts cp pc ebs) = do
    let countString =
         "timestamp = " ++ show ts ++ "\n" ++
         "confirmed_planets = " ++ show  cp ++ "\n" ++
         "planet_candidates = " ++ show  pc ++ "\n" ++
         "eclipsing_binary_stars = " ++ show ebs ++ "\n"
    writeFile path countString

readOldCounts :: FilePath -> IO AstroCounts
readOldCounts path = do
    countItems <- getConfItems path
    let t = read $ lookupConfItem "timestamp" countItems
    let c = read $ lookupConfItem "confirmed_planets" countItems
    let p = read $ lookupConfItem "planet_candidates" countItems
    let e = read $ lookupConfItem "eclipsing_binary_stars" countItems
    return (AstroCounts t c p e)

filterComma :: String -> String
filterComma = filter (/= ',')

