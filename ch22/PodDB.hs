module PodDB where

import Database.HDBC
import Database.HDBC.Sqlite3
import PodTypes
import Control.Monad(when)
import Data.List(sort)

connect :: FilePath -> IO Connection
connect fp =
  do dbh <- connectSqlite3 fp
     prepDB dbh
     return dbh

-- prepDB :: IConnection conn => conn -> IO ()
-- prepDB dbh =
--   do tables <- getTables dbh
--      when (not ("podcasts" `elem` tables)) $
--        do run
