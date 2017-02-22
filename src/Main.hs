-- run not recent: run a command only if haven't done so recently

import Control.Monad
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory
import System.Environment
import System.FilePath
import qualified HSH

getConnection :: IO Connection
getConnection = do
    home <- getHomeDirectory
    let sqlFile = home </> ".config" </> "rnr"
    dbExisted <- doesFileExist sqlFile
    createDirectoryIfMising True (home </> ".config")
    conn <- handleSqlError $ connectSqlite3 sqlFile
    unless dbExisted . withTransaction conn $ \c -> run c (
        "CREATE TABLE run_log (" ++
        "command TEXT NOT NULL, " ++
        "args TEXT NOT NULL, " ++
        "did_time INTEGER, " ++
        "PRIMARY KEY (command, args))") [] >> return ()
    return conn

getTimeInt :: IO Int
getTimeInt = fmap floor getPOSIXTime

escArgs = unwords . map escArg

recordRun :: Connection -> String -> [String] -> IO ()
recordRun conn command args = do
  time <- getTimeInt
  let argsSql = toSql $ escArgs args
  withTransaction conn $ \c -> do
    numRowsChanged <- run c
      "UPDATE run_log SET did_time = ? WHERE command = ? AND args = ?"
      [toSql time, toSql command, argsSql]
    when (numRowsChanged == 0) $ run c
      "INSERT INTO run_log (command, args, did_time) VALUES (?, ?, ?)"
      [toSql command, argsSql, toSql time] >> return ()
  disconnect conn

lastRun :: Connection -> String -> [String] -> IO (Maybe Int)
lastRun conn command args = do
  ret <- withTransaction conn $ \c -> quickQuery c
    "SELECT did_time FROM run_log WHERE command = ? AND args = ? \
    \ORDER BY did_time DESC LIMIT 1"
    [toSql command, toSql $ escArgs args]
  return . listToMaybe $ map (fromSql . head) ret

escArg :: [Char] -> [Char]
escArg a = let
  escSQ '\'' = "'\\''"
  escSQ '\\' = "\\\\"
  escSQ x = [x]
  in "'" ++ concatMap escSQ a ++ "'"

parseTimeStr :: (Num t, Read t) => String -> t
parseTimeStr s = let
  [(num, unit)] = reads s
  unitSec = case unit of
    "s" -> 1
    "m" -> 60
    "h" -> 3600
    "d" -> 24 * 3600
    _ -> error "unknown time unit"
  in num * unitSec

main :: IO ()
main = do
  conn <- getConnection
  timeStr:command:args <- getArgs
  timeNow <- getTimeInt
  timeLastRunMb <- lastRun conn command args
  let
    run = do
      HSH.runIO (command, args)
      recordRun conn command args
  case timeLastRunMb of
    Nothing -> run
    Just timeLastRun ->
      when (timeLastRun + (parseTimeStr timeStr) <= timeNow) run
