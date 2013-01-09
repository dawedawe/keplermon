module Main where

import Control.Monad (when)
import System.Environment (getArgs)

import KeplerMon
import Conf

main :: IO ()
main = do
    parsedArgv <- getArgs >>= parseArgv
    let parsedOptions = fst parsedArgv
    createDotDir
    when (optVerbose parsedOptions) (print parsedOptions)
    conf <- buildConf parsedOptions
    getAndPrintCounts conf

