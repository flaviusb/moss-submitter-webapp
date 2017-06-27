{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Exception
import Control.Concurrent
import Control.Exception
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString as BS
import System.IO

len = BS.length

data FileData = FileData { contents :: Text, id :: Text, lang :: Text, path :: Text, size :: Text }

data Switch = Switch {
                language :: Text,
                matchThreshold :: Int,
                numberOfMatchesToShow :: Int,
                filesByDirectory :: Bool,
                comment :: Text,
                experimental :: Bool
            }

-- Define our data that will be used for creating the form.

data MossForm = MossForm
  {
    fileInfo :: FileInfo,
    switch   :: Switch
  }

data FileForm = FileForm
    { ffileInfo :: FileInfo,
      fcomment :: Text,
      flanguage :: Language,
      fexperimental :: Bool,
      fdirectory :: Bool,
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

data Language = C | CC | JAVA | ML | PASCAL | ADA | LISP | SCHEME | HASKELL | FORTRAN | ASCII | VHDL | PERL | MATLAB | PYTHON | MIPS | PROLOG | SPICE | VB | CSHARP | MODULA2 | A8086 | JAVASCRIPT | PLSQL deriving (Show, Eq, Enum, Bounded)

languages :: [(Language, Text)]
languages = [(C, "c"), (CC, "cc"), (JAVA, "java"), (ML, "ml"), (PASCAL, "pascal"), (ADA, "ada"), (LISP, "lisp"), (SCHEME, "scheme"), (HASKELL, "haskell"), (FORTRAN, "fortran"), (ASCII, "ascii"), (VHDL, "vhdl"), (PERL, "perl"), (MATLAB, "matlab"), (PYTHON, "python"), (MIPS, "mips"), (PROLOG, "prolog"), (SPICE, "spice"), (VB, "vb"), (CSHARP, "csharp"), (MODULA2, "modula2"), (A8086, "a8086"), (JAVASCRIPT, "javascript"), (PLSQL, "plsql")]

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Moss submission webapp"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        case submission of
          Just _ -> setTitle "Files submitted to Moss"
          _      -> setTitle "Moss submission webapp"
        $(widgetFile "homepage")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    <*> areq (selectField optionsEnum) "Language" (Just C)
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
