
module Main where

import CategorySelection
import Command
import Configuration
import Detail
import Domain
import ExitWithMsg
import Help
import Import
import ImportFileName
import Summary
import TransactionList
import VersionNumber 

import Control.Monad.Except
import System.Directory
import System.Environment

main :: IO ()
main = do
    run <- runExceptT budget
    either putStrLn return run

configFilePath :: FilePath
configFilePath = "/.budget_conf"

budget :: Domain ()
budget = do
    args <- liftIO getArgs
    home <- liftIO getHomeDirectory
    cfg <- fromFile (home ++ configFilePath)
    cmd <- command args
    doCommand cfg cmd

doCommand :: Configuration -> Command -> Domain ()
doCommand cfg (Detail mFilePath catSel mPeriod criteria) = do
    mainFilePath <- cfg `atKey` "TRANSACTIONS"   
    transactions <- transactionsFromFile (maybe mainFilePath id mFilePath)
    selector <- categorySelector catSel
    let report = detail mFilePath catSel mPeriod criteria <$> pure selector <*> pure transactions
    liftIO (either exitWithMsg (putStr . unlines) report)

doCommand cfg (Summary mFilePath catSel mPeriod criteria) = do 
    mainFilePath <- cfg `atKey` "TRANSACTIONS"
    transactions <- transactionsFromFile (maybe mainFilePath id mFilePath)
    selector <- categorySelector catSel
    let report = (summary mFilePath catSel mPeriod criteria) <$> pure selector <*> pure transactions
    liftIO (either exitWithMsg (putStr . unlines) report)

doCommand cfg (Import importFilePath (Just account)) = do
    mainFilePath <- cfg `atKey` "TRANSACTIONS"
    transactions <- transactionsFromFile mainFilePath
    importations <- transactionsFromFile importFilePath
    result <- domain (importTransactions account transactions importations)
    let result_length = length (fst result) - length transactions
    let result_dupes  = snd result
    let result_new_trans = fst result
    transactionsToFile mainFilePath result_new_trans
    liftIO (putStrLn (show result_length ++ " transactions imported"))
    liftIO (putStrLn (case result_dupes of
                        [] -> "no duplicates were found"
                        _ -> unlines ([ "transactions that were already in the main file, or that were duplicates in the import file, and were NOT imported:" ] 
                              ++ (showDuplicates result_dupes))))

doCommand cfg (Import importFilePath Nothing) = do
    isDirectory <- liftIO (doesDirectoryExist importFilePath)
    case isDirectory of
        False -> do
              let name = extractAccountNamePart importFilePath
              either (liftIO . exitWithMsg) (\n -> doCommand cfg (Import importFilePath (Just n))) name
        True -> do
            directory <- liftIO (importDirectory importFilePath)
            either (liftIO . exitWithMsg) (mapM_ (\filePath -> doCommand cfg (Import filePath Nothing))) directory

doCommand _ (Yearly _ _) = undefined

doCommand _ (Help arg) = liftIO (help arg)

doCommand _ Version = liftIO $ putStrLn $ unlines $ [ "budget " ++ show versionNumber
                                                    , "by Andrea Chiou & Christophe Thibaut"]

