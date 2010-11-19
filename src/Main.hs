-- run not recent: run a command only if haven't done so recently

import Control.Monad
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import Database.HDBC
import Database.HDBC.PostgreSQL
import System.Environment
import qualified HSH

cPS :: IO Connection
cPS = handleSqlError $ connectPostgreSQL "dbname=me_log"

getTimeInt :: IO Int
getTimeInt = fmap floor getPOSIXTime

escArgs = unwords . map escArg

recordRun :: String -> [String] -> IO ()
recordRun command args = do
  time <- getTimeInt
  conn <- cPS
  let argsSql = toSql $ escArgs args
  withTransaction conn $ \ c -> do
    numRowsChanged <- run c
      "UPDATE run_log SET did_time = ? WHERE command = ? AND args = ?"
      [toSql time, toSql command, argsSql]
    when (numRowsChanged == 0) $ run c
      "INSERT INTO run_log (command, args, did_time) VALUES (?, ?, ?)"
      [toSql command, argsSql, toSql time] >> return ()
  disconnect conn

lastRun :: String -> [String] -> IO (Maybe Int)
lastRun command args = do
  conn <- cPS
  ret <- withTransaction conn $ \ conn -> quickQuery conn
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
  timeStr:command:args <- getArgs
  timeNow <- getTimeInt
  timeLastRunMb <- lastRun command args
  let
    run = do
      HSH.runIO (command, args)
      recordRun command args
  case timeLastRunMb of
    Nothing -> run
    Just timeLastRun ->
      when (timeLastRun + (parseTimeStr timeStr) <= timeNow) run
