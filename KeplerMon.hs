module KeplerMon
( getAndPrintCounts
, AstroCounts
) where

import System.Directory (doesFileExist)
import Network.HTTP
import Network.Browser
import Text.HTML.TagSoup

import Conf

type ConfirmedPlanets = Int
type PlanetCandidates = Int
type EclipsingBiStars = Int
type AstroCounts = (ConfirmedPlanets, PlanetCandidates, EclipsingBiStars)

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
buildDisplayStrings s = [cp, pc, ebs]
	where
	  cp  = "Confirmed Planets:      " ++ s !! 0
	  pc  = "Planet Candidates:      " ++ s !! 1
	  ebs = "Eclipsing Binary Stars: " ++ s !! 2

appDiffsToCounts :: AstroCounts -> [String] -> [String]
appDiffsToCounts (cp, pc, ebs) diffs =
	[cpinfo, pcinfo, ebsinfo]
	where
	  cpinfo  = show cp ++ "\t" ++ (diffs !! 0)
	  pcinfo  = show pc ++ "\t" ++ (diffs !! 1)
	  ebsinfo = show ebs ++ "\t" ++ (diffs !! 2)

diffOldNewCounts :: AstroCounts -> AstroCounts -> [String]
diffOldNewCounts (o_cp, o_pc, o_ebs) (n_cp, n_pc, n_ebs) =
	map enrichDiff [cpdiff, pcdiff, ebsdiff]
	where
	  cpdiff  = n_cp - o_cp
	  pcdiff  = n_pc - o_pc
	  ebsdiff = n_ebs - o_ebs

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
	return (confPlanets, planCandits, eclipBiStars)

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
writeCurrentCounts path (cp, pc, ebs) = writeFile path countString 
	where countString = 
		"confirmed_planets = " ++ (show  cp) ++ "\n" ++
		"planet_candidates = " ++ (show  pc) ++ "\n" ++
		"eclipsing_binary_stars = " ++ (show ebs) ++ "\n"

readOldCounts :: FilePath -> IO AstroCounts
readOldCounts path = do
	countItems <- getConfItems $ path
	let c  = lookup "confirmed_planets" countItems
	let c' = read $ checkConfItem c "confirmed_planets"
	let p  = lookup "planet_candidates" countItems
	let p' = read $ checkConfItem p "planet_candidates"
	let e  = lookup "eclipsing_binary_stars" countItems
	let e' = read $ checkConfItem e "eclipsing_binary_stars"
	return (c', p', e')

filterComma :: String -> String
filterComma = filter (\c -> c /= ',')

