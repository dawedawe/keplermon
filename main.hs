import Control.Monad (when)
import System.Environment
import System.Console.GetOpt()

import KeplerMon
import Conf

main :: IO ()
main = do
	parsedArgv <- getArgs >>= parseArgv
	let parsedOptions = fst parsedArgv

	when (optVerbose parsedOptions) (print parsedOptions)
	conf <- buildConf parsedOptions
	getAndPrintCounts conf

