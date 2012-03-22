import System.Environment
import System.Console.GetOpt()

import KeplerMon
import Conf

main :: IO ()
main = do
	parsedArgv <- (getArgs >>= parseArgv)
	let parsedOptions = fst parsedArgv

	if (optVerbose parsedOptions)
	  then putStrLn $ show parsedOptions
	  else return ()

	conf <- buildConf parsedOptions
	getAndPrintCounts conf

