module KeplerMon
( getAndPrintCounts
) where

import System.Directory (doesFileExist)
import Network.HTTP
import Network.Browser
import Text.HTML.TagSoup

import Conf

data AstroCounts = AstroCounts
	{ confirmedPlanets :: Int
	, planetCandidates :: Int
	, eclipsingBiStars :: Int
	}

getAndPrintCounts :: Conf -> IO ()
getAndPrintCounts conf = do
	oldCounts <- readOldCounts $ dataPath conf
	curCounts <- getCurrentCounts (proxy conf) (dataUrl conf)
	initDataFileIfNeeded (dataPath conf) curCounts
	let diffs = diffOldNewCounts oldCounts curCounts
	let info = appDiffsToCounts curCounts diffs
	let disp = buildDisplayStrings info
	mapM_ putStrLn disp 
	writeCurrentCounts (dataPath conf) curCounts

buildDisplayStrings :: [String] -> [String]
buildDisplayStrings [countdiff0, countdiff1, countdiff2] =
	[cp, pc, ebs]
	where
	cp  = "Confirmed Planets:      " ++ countdiff0
	pc  = "Planet Candidates:      " ++ countdiff1
	ebs = "Eclipsing Binary Stars: " ++ countdiff2
buildDisplayStrings _ = error "undefined arguments for buildDisplayStrings"

appDiffsToCounts :: AstroCounts -> [String] -> [String]
appDiffsToCounts ac [d0, d1, d2] =
	[cpinfo, pcinfo, ebsinfo]
	where
	cpinfo  = show (confirmedPlanets ac) ++ "\t" ++ d0
	pcinfo  = show (planetCandidates ac) ++ "\t" ++ d1
	ebsinfo = show (eclipsingBiStars ac) ++ "\t" ++ d2
appDiffsToCounts _ _ = error "undefined arguments for appDiffsToCounts"

diffOldNewCounts :: AstroCounts -> AstroCounts -> [String]
diffOldNewCounts old new =
	map enrichDiff [cpdiff, pcdiff, ebsdiff]
	where
	cpdiff  = confirmedPlanets new - confirmedPlanets old
	pcdiff  = planetCandidates new - planetCandidates old
	ebsdiff = eclipsingBiStars new - eclipsingBiStars old

enrichDiff :: Int -> String
enrichDiff n | (n < 0)   = "(" ++ show n ++ ")"
	     | otherwise = "(+" ++ (show n) ++ ")"

getCurrentCounts :: Proxy -> String -> IO AstroCounts
getCurrentCounts prox url = do
	tags <- fmap parseTags $ getPage prox url
	let counts = partitions (~== "<div id=\"ullitags\"") tags
	let confPlanets = read $ filterComma $ fromTagText (counts !! 0 !! 3)
	let planCandits = read $ filterComma $ fromTagText (counts !! 0 !! 9)
	let eclipBiStars = read $ filterComma $ fromTagText (counts !! 0 !! 15)
	return (AstroCounts confPlanets planCandits eclipBiStars)

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
	if e == False
	  then writeCurrentCounts path counts
	  else return ()

writeCurrentCounts :: FilePath -> AstroCounts -> IO ()
writeCurrentCounts path (AstroCounts cp pc ebs) =
	writeFile path countString
	where
	countString = 
	  "confirmed_planets = " ++ (show  cp) ++ "\n" ++
	  "planet_candidates = " ++ (show  pc) ++ "\n" ++
	  "eclipsing_binary_stars = " ++ (show ebs) ++ "\n"

readOldCounts :: FilePath -> IO AstroCounts
readOldCounts path = do
	countItems <- getConfItems $ path
	let c = read $ lookupConfItem "confirmed_planets" countItems
	let p = read $ lookupConfItem "planet_candidates" countItems
	let e = read $ lookupConfItem "eclipsing_binary_stars" countItems
	return (AstroCounts c p e)

filterComma :: String -> String
filterComma = filter (\c -> c /= ',')

