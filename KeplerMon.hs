module KeplerMon
( getAndPrintCounts
) where

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
	curCounts <- getCurrentCounts (proxy conf) (dataUrl conf)
	printAstroCounts curCounts

getCurrentCounts :: Proxy -> String -> IO AstroCounts
getCurrentCounts prox url = do
	tags <- fmap parseTags $ getPage prox url
	let counts = partitions (~== "<div id=\"ullitags\"") tags
	let confPlanets = read $ filterComma $ fromTagText (counts !! 0 !! 3)
	let planCandits = read $ filterComma $ fromTagText (counts !! 0 !! 9)
	let eclipBiStars = read $ filterComma $ fromTagText (counts !! 0 !! 15)
	return (confPlanets, planCandits, eclipBiStars)

printAstroCounts :: AstroCounts -> IO ()
printAstroCounts (cp, pc, ebs) = do
	putStrLn $ "Confirmed Planets:      " ++ (show cp)
	putStrLn $ "Planet Candidates:      " ++ (show pc)
	putStrLn $ "Eclipsing Binary Stars: " ++ (show ebs)

getPage :: Proxy -> String -> IO String
getPage prox url = do
	(_,rsp) <- Network.Browser.browse $ do
	  setProxy prox
	  setOutHandler $ const (return ()) -- no debug output
	  request (getRequest url)
	return (rspBody rsp)

filterComma :: String -> String
filterComma = filter (\c -> c /= ',')

