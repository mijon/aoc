{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative hiding (some)
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char -- for combiantors
import Data.Text as T
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttp
  | SchemeHttps
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)


-- [//[user:password@]host[:port]]￼
data Authority = Authority
  { authUser :: Maybe (Text, Text) -- (user, password)
  , authHost :: Text
  , authPort :: Maybe Int
  } deriving (Eq, Show)

data Uri = Uri
  { uriScheme    :: Scheme
  , uriAuthority :: Maybe Authority
  } deriving (Eq, Show)


pScheme :: Parser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttps  <$ string "https"
  , SchemeHttp   <$ string "http"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]



pAuthority :: Parser Authority
pAuthority = do
    void (string "//")
    authUser <- optional . try $ do
        user <- T.pack <$> some alphaNumChar
        void (char ':')
        password <- T.pack <$> some alphaNumChar
        void (char '@')
        return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.')
    authPort <- optional (char ':' *> L.decimal)
    return Authority {..}

pUri :: Parser Uri
pUri = do
    uriScheme <- pScheme
    void (char ':')
    uriAuthority <- optional pAuthority
    return Uri {..}


