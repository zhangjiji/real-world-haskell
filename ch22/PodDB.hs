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

prepDB :: IConnection conn => conn -> IO ()
prepDB dbh =
  do tables <- getTables dbh
     when (not ("podcasts" `elem` tables)) $
       do run dbh "CREATE TABLE podcasts (castid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, castURL TEXT NOT NULL UNIQUE)" []
          return ()
     when (not ("episodes" `elem` tables)) $
       do run dbh "CREATE TABLE episodes (epid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, epcastid INTEGER NOT NULL, epurl TEXT NOT NULL, epdone INTEGER NOT NULL, UNIQUE(epcastid, epurl), UNIQUE(epcastid, epid))" []
          return ()
     commit dbh

addPodcast :: IConnection conn => conn -> Podcast -> IO Podcast
addPodcast dbh podcast =
  handleSql errorHandler $
    do
      run dbh "INSERT INTO podcasts (castURL) VALUES (?)" [toSql (castURL podcast)]
      r <- quickQuery' dbh "SELECT castid FROM podcasts WHERE castURL = ?" [toSql (castURL podcast)]
--      commit dbh
      
      case r of
        [[x]] -> return $ podcast {castId = fromSql x}
        y -> fail $ "Select None"
  where errorHandler e =
          do fail $ "error"
