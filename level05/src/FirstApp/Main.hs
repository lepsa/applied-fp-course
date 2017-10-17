{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FirstApp.Main
  ( runApp
  , prepareAppReqs
  , app
  ) where

import           Control.Applicative                (liftA2)
import           Control.Monad                      (join)

import           Network.Wai                        (Application, Request,
                                                     Response, pathInfo,
                                                     requestMethod, responseLBS,
                                                     strictRequestBody)
import           Network.Wai.Handler.Warp           (run)

import           Network.HTTP.Types                 (Status, hContentType,
                                                     status200, status400,
                                                     status404, status500)

import qualified Data.ByteString.Lazy.Char8         as LBS

import           Data.Either                        (Either (Left, Right),
                                                     either)

import           Data.Semigroup                     ((<>))
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)

import           Data.Aeson                         (ToJSON)
import qualified Data.Aeson                         as A

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import qualified FirstApp.Conf                      as Conf
import qualified FirstApp.DB                        as DB
import           FirstApp.Types                     (ContentType (JSON, PlainText),
                                                     Error (EmptyCommentText, EmptyTopic, UnknownRoute, DbError),
                                                     RqType (AddRq, ListRq, ViewRq),
                                                     mkCommentText, mkTopic,
                                                     renderContentType)

-- Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = ConfErr Conf.ConfigError
  | DbInitErr SQLiteResponse
  deriving Show

runApp :: IO ()
runApp = do
  cfgE <- prepareAppReqs
  case cfgE of
    Left err        -> print err
    Right (cfg, db) -> run (Conf.confPortToWai cfg) $ app cfg db

prepareAppReqs
  :: IO (Either StartUpError (Conf.Conf,DB.FirstAppDB))
prepareAppReqs = do
  confE <- Conf.parseOptions path
  dbE   <- DB.initDb "db.sqlite" $ DB.Table "comments"
  let confE' = either (Left . ConfErr) Right confE
  let dbE' = either (Left . DbInitErr) Right dbE
  pure $ (,) <$> confE' <*> dbE'
  where
    path = "appconfig.json"

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse sts ct msg =
  responseLBS sts [(hContentType, renderContentType ct)] msg

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 =
  mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  mkResponse status400

-- Some new helpers for different statuses and content types
resp500
  :: ContentType
  -> LBS.ByteString
  -> Response
resp500 =
  mkResponse status500

resp200Json
  :: ToJSON a
  => a
  -> Response
resp200Json =
  mkResponse status200 JSON . A.encode

-- |
app
  :: Conf.Conf
  -> DB.FirstAppDB -- ^ Add the Database record to our app so we can use it
  -> Application
app cfg db rq cb = do
  rq' <- mkRequest rq
  resp <- handleRespErr <$> handleRErr rq'
  cb resp
  where
    -- Does this seem clunky to you?
    handleRespErr = either mkErrorResponse id
    -- Because it is clunky, and we have a better solution, later.
    handleRErr =
      -- We want to pass the Database through to the handleRequest so it's
      -- available to all of our handlers.
      either ( pure . Left ) ( handleRequest cfg db )

handleRequest
  :: Conf.Conf
  -> DB.FirstAppDB
  -> RqType
  -> IO (Either Error Response)
handleRequest _ db (AddRq t c) =
  fmap (const ( resp200 PlainText "Success" )) <$> DB.addCommentToTopic db t c
handleRequest _ db (ViewRq t)  =
  fmap (resp200 JSON . A.encode) <$> DB.getComments db t
handleRequest _ db ListRq      =
  fmap (resp200 JSON . A.encode) <$> DB.getTopics db

mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest rq =
  case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) ->
      mkAddRequest t <$> strictRequestBody rq
    -- View the comments on a given topic
    ( [t, "view"], "GET" ) ->
      pure ( mkViewRequest t )
    -- List the current topics
    ( ["list"], "GET" )    ->
      pure mkListRequest
    -- Finally we don't care about any other requests so throw your hands in the air
    _                      ->
      pure ( Left UnknownRoute )

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 . LBS.toStrict) c

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest =
  fmap ViewRq . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownRoute =
  resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText =
  resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic =
  resp400 PlainText "Empty Topic"
mkErrorResponse (DbError _) =
  resp500 PlainText "Internal DB Error"  
