module Database.Logger where

logError :: String -> IO ()
logError s = do
    putStrLn s