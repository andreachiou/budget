module ExitWithMsg ( exitWithMsg )
    where
import System.Exit

exitWithMsg :: String -> IO ()
exitWithMsg msg = do
    putStrLn msg
    exitFailure

