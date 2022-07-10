-- |This module includes the main function of the program which is an IO () based function.

module Main where

import Customer
import Control.Concurrent
import System.IO

-- |This is the main function of the program. It executes IO () functionality containing the main actions of the program, takes no arguments and returns nothing.
main :: IO () -- ^ returns IO action
main = do 
 hSetBuffering stdout NoBuffering
 m <- newEmptyMVar
 e <- newChan
 let names = ["Ant","James","Jess","Kat","Dave","Mike","Marie","Sara","Eve","Kate"]
 mapM_ (const $ forkIO $ createCustomers names [1..10] m e)[1]
 putMVar m () 
 mapM_ (const $ readChan e)[1]
 return ()
 