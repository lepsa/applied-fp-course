{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module FirstApp.Conf
    ( Conf (..)
    , Port (getPort)
    , HelloMsg (getHelloMsg)
    , parseOptions
    , confPortToWai
    ) where

import           Control.Exception          (catch, SomeException)

import           GHC.Word                   (Word16)

import           Data.Monoid                (Last (Last, getLast),
                                             Monoid (mappend, mempty), (<>))
import           Data.String                (fromString)

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text                  (Text)

import           Data.Aeson                 (FromJSON)

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as Aeson

import           Options.Applicative        (Parser, ParserInfo, eitherReader,
                                             execParser, fullDesc, header, help,
                                             helper, info, long, metavar,
                                             option, optional, progDesc, short,
                                             strOption)

import           Text.Read                  (readEither, readMaybe)
import Data.Bifunctor (first)

-- Doctest setup section
-- $setup
-- >>> :set -XOverloadedStrings

newtype Port = Port
  { getPort :: Word16 }
  deriving (Eq, Show)

newtype HelloMsg = HelloMsg
  { getHelloMsg :: ByteString }
  deriving (Eq, Show)

-- The ``Conf`` type will need:
-- - A customisable port number: ``Port``
-- - A changeable message for our users: ``HelloMsg``
data Conf = Conf
  { port :: Port
  , msg  :: HelloMsg
  }
  deriving (Eq, Show)

-- We're storing our Port as a Word16 to be more precise and prevent invalid
-- values from being used in our application. However Wai is not so stringent.
-- To accommodate this and make our lives a bit easier, we will write this
-- helper function to take ``Conf`` value and convert it to an ``Int``.
confPortToWai
  :: Conf
  -> Int
confPortToWai = fromInteger . toInteger . getPort . port

-- Similar to when we were considering our application types, leave this empty
-- for now and add to it as you go.
data ConfigError = MissingPort
                 | MissingMsg
                 | ParseError String
                 | FileError String
  deriving Show

-- Our application will be able to load configuration from both a file and
-- command line input. We want to be able to use the command line to temporarily
-- override the configuration from our file. How do we combine the different
-- inputs to enable this property?

-- We want the command line configuration to take precedence over the File
-- configuration, so if we think about combining each of our ``Conf`` records,
-- we want to be able to write something like this:

-- ``defaults <> file <> commandLine``

-- We can use the ``Monoid`` typeclass to handle combining the ``Conf`` records
-- together, and the ``Last`` type to wrap up our values to handle the desired
-- precedence. The ``Last`` type is a wrapper for Maybe that when used with its
-- ``Monoid`` instance will always preference the last ``Just`` value that it
-- has:

-- Last (Just 3) <> Last (Just 1) = Last (Just 1)
-- Last Nothing  <> Last (Just 1) = Last (Just 1)
-- Last (Just 1) <> Last Nothing  = Last (Just 1)

-- To make this easier, we'll make a new type ``PartialConf`` that will have our
-- ``Last`` wrapped values. We can then define a ``Monoid`` instance for it and
-- have our ``Conf`` be a known good configuration.
data PartialConf = PartialConf
   { mPort :: Last Port
   , mMsg  :: Last HelloMsg
   }

-- We now define our ``Monoid`` instance for ``PartialConf``. Allowing us to
-- define our always empty configuration, which would always fail our
-- requirements. More interestingly, we define our ``mappend`` function to lean
-- on the ``Monoid`` instance for Last to always get the last value.
instance Monoid PartialConf where
  mempty      = PartialConf (Last Nothing) (Last Nothing)
  mappend a b = PartialConf (mPort a <> mPort b) (mMsg a <> mMsg b)

-- For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf
  :: PartialConf
defaultConf = PartialConf
  (Last . Just $ Port 8080)
  (Last . Just $ HelloMsg "Hello World")

-- We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig (PartialConf p m) =
  Conf <$> fromEither MissingPort (getLast p)
       <*> fromEither MissingMsg (getLast m)
  where
    fromEither :: e -> Maybe a -> Either e a
    fromEither e = maybe (Left e) Right

-- This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
parseOptions
  :: FilePath
  -> IO (Either ConfigError Conf)
parseOptions p = do
  fe <- parseJSONConfigFile p
  c  <- execParser commandLineParser
  let partialE = (\f -> defaultConf <> f <> c) <$> fe
  pure $ partialE >>= makeConfig
  
-- | File Parsing

-- We're trying to avoid complications when selecting a configuration file
-- package from Hackage. We'll use an encoding that you are probably familiar
-- with, for better or worse, and write a small parser to pull out the bits we
-- need. The package we're using is the ``aeson`` package to parse some JSON and
-- we'll pick the bits off the Object.

-- Complete the helper function that will be used to retrieve values off the
-- JSON object.

-- | fromJsonObjWithKey
-- >>> let (Just obj) = ( Aeson.decode "{\"foo\":\"Susan\"}" ) :: Maybe Aeson.Object
--
-- >>> fromJsonObjWithKey "foo" (id :: Text -> Text) obj
-- Last {getLast = Just "Susan"}
--
-- >>> fromJsonObjWithKey "foo" id obj
-- Last {getLast = Nothing}
--
fromJsonObjWithKey
  :: FromJSON a
  => Text
  -> (a -> b)
  -> Aeson.Object
  -> Last b
fromJsonObjWithKey k f o = maybe (Last Nothing) (Last . fmap f) m 
  where
    m :: FromJSON a => Maybe (Maybe a)
    m = Aeson.parseMaybe (Aeson..:? k) o

-- |----
-- | You will need to update these tests when you've completed the following functions!
-- | The 'undefined' in these tests needs to be replaced with their respective Error constructors.
-- |----

-- | decodeObj
-- >>> decodeObj ""
-- Left (ParseError "Error in $: not enough input")
--
-- >>> decodeObj "{\"bar\":33}"
-- Right (fromList [("bar",Number 33.0)])
--
decodeObj
  :: ByteString
  -> Either ConfigError Aeson.Object
decodeObj bs = 
  first ParseError $ Aeson.eitherDecode bs

-- | Update these tests when you've completed this function.
--
-- | readObject
-- >>> readObject "badFileName.no"
-- Left (FileError "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
--
-- >>> readObject "test.json"
-- Right "{\"foo\":33}\n"
--
readObject
  :: FilePath
  -> IO ( Either ConfigError ByteString )
readObject p = catch (Right <$> LBS.readFile p) err
  where
    err :: SomeException -> IO (Either ConfigError a)
    err = pure . Left . FileError . show

-- Construct the function that will take a ``FilePath``, read it in and attempt
-- to decode it as a valid JSON object, using the ``aeson`` package. Then pull
-- specific keys off this object and construct our ``PartialConf``. Using the
-- function we wrote above to assist in pulling items off the object.
parseJSONConfigFile
  :: FilePath
  -> IO (Either ConfigError PartialConf)
parseJSONConfigFile p =
  (fmap f . decodeObj =<<) <$> readObject p
  where
    f :: Aeson.Object -> PartialConf
    f o = 
      let port' = fromJsonObjWithKey "port" Port o
          msg'  = fromJsonObjWithKey "msg"  (HelloMsg . fromString) o
      in PartialConf port' msg'

--fromJsonObjWithKey
--  :: FromJSON a
--  => Text
--  -> (a -> b)
--  -> Aeson.Object
--  -> Last b

-- | Command Line Parsing

-- We will use the ``optparse-applicative`` package to build our command line
-- parser. As this particular problem is fraught with silly dangers and we
-- appreciate someone else having eaten this gremlin on our behalf.

-- You'll need to use the documentation for ``optparse-applicative`` to help you
-- write these functions as we're relying on their API to produce the types we
-- need. We've provided some of the less interesting boilerplate for you.
commandLineParser
  :: ParserInfo PartialConf
commandLineParser =
  let mods = fullDesc
        <> progDesc "Manage comments for something"
        <> header "Your first Haskell app!"
  in
    info (helper <*> partialConfParser) mods

-- Combine the smaller parsers into our larger ``PartialConf`` type.
partialConfParser
  :: Parser PartialConf
partialConfParser =
  PartialConf <$> portParser <*> helloMsgParser

-- Parse the Port value off the command line args and into a Last wrapper.
portParser
  :: Parser (Last Port)
portParser =
  let
    mods =  long "port"
         <> short 'p'
         <> metavar "PORT"
         <> help "TCP Port to accept requests on"
  in
    Last . fmap Port . (readMaybe =<<) <$> (optional $ strOption mods)

-- Parse the HelloMsg from the input string into our type and into a Last wrapper.
helloMsgParser
  :: Parser (Last HelloMsg)
helloMsgParser =
  let
    mods = long "hello-msg"
         <> short 'm'
         <> metavar "HELLOMSG"
         <> help "Message to respond to requests with."
  in
    (fmap (HelloMsg . fromString) . Last) <$> (optional $ strOption mods)
