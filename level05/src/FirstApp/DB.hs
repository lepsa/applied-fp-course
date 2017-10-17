{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module FirstApp.DB
  ( Table (..)
  , FirstAppDB (FirstAppDB)
  , initDb
  , closeDb
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import Control.Exception (SomeException (SomeException), catch)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, Query (Query))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse (SQLOtherError))

import           FirstApp.Types                     (Comment, CommentText,
                                                     Error (DbError), Topic, mkTopic, fromDbComment)

-- ------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple ready for this section! |
-- ------------------------------------------------------------------------|

-- We need to have a way to pass around the name of the Table we're going to us
-- for the comments in this application. We _could_ pass around a `Text` value,
-- but we can be better than that.
newtype Table = Table Text
  deriving Show

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
data FirstAppDB = FirstAppDB
  { conn  :: Connection
  , table :: Table
  }

-- Quick helper to pull the connection and close it down.
closeDb
  :: FirstAppDB
  -> IO ()
closeDb = Sql.close . conn
-- Because our `Table` is a configurable value, this application has a SQL
-- injection vulnerability. That being said, in order to leverage this weakness,
-- your appconfig.json file must be compromised and your app restarted. If that
-- is capable of happening courtesy of a hostile actor, there are larger issues.

-- Complete the withTable function so that the placeholder '$$tablename$$' is
-- found and replaced in the provided Query.
-- | withTable
-- >>> withTable (Table "tbl_nm") "SELECT * FROM $$tablename$$"
-- "SELECT * FROM tbl_nm"
-- >>> withTable (Table "tbl_nm") "SELECT * FROM foo"
-- "SELECT * FROM foo"
-- >>> withTable (Table "tbl_nm") ""
-- ""
withTable
  :: Table
  -> Query
  -> Query
withTable (Table t) q = Query . Text.replace "$$tablename$$" t $ Sql.fromQuery q
 
-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDb
  :: FilePath
  -> Table
  -> IO ( Either SQLiteResponse FirstAppDB )
initDb fp tab =
  catch (Right <$> f) e
  where
    f :: IO FirstAppDB
    f = do 
      c <- Sql.open fp
      createTable c
      pure $ FirstAppDB c tab
    e :: SQLiteResponse -> IO (Either SQLiteResponse a)
    e = pure . Left
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTable :: Connection -> IO ()
    createTable = flip Sql.execute_ createTableQ
    createTableQ :: Query
    createTableQ = withTable tab
      "CREATE TABLE IF NOT EXISTS $$tablename$$ (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DbComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DbComment to a Comment, we need to use ``fromDbComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
getComments db t =
  -- There are several possible implementations of this function. Paritcularly
  -- there may be a trade-off between deciding to throw an Error if a DbComment
  -- cannot be converted to a Comment, or simply ignoring any DbComment that is
  -- not valid.
  traverse fromDbComment <$> Sql.query (conn db) q (Sql.Only t)
  where
    q :: Query
    q = withTable (table db) "select id, topic, comment, time from $$tablename$$ where topic = ?"

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
addCommentToTopic db t c = do
  utc <- getCurrentTime
  catch (Right <$> Sql.execute (conn db) q (t, c, utc)) err
  where
    err = (pure . Left . DbError)
    q :: Query
    q = withTable (table db) "insert into $$tablename$$ (topic, comment, time) values (?,?,?)"

getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics db =
  catch f err
  where
    f :: IO (Either Error [Topic])
    f = traverse (mkTopic . Sql.fromOnly) <$> Sql.query_ (conn db) q
    err = pure . Left . DbError
    q :: Query
    q = withTable (table db) "select topic from $$tablename$$"

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic db t =
  catch (Right <$> Sql.execute (conn db) q (Sql.Only t)) err
  where
    err = pure . Left . DbError
    q :: Query
    q = withTable (table db) "delete from $$tablename$$ where topic = ?"
