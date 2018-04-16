module Main where

import           System.MQ.Monad     (runMQMonad)
import           System.MQ.Scheduler (getLogicConfig, getNetConfig,
                                      schedulerLogic)

main :: IO ()
main = do
    netConfig   <- getNetConfig
    logicConfig <- getLogicConfig
    runMQMonad $ schedulerLogic netConfig logicConfig
