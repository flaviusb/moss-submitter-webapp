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
-- import Control.Exception
import Control.Concurrent
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString as BS
import System.IO
import Numeric (readDec)
import Text.Read (readMaybe)

len = BS.length

data FileData = FileData { contents :: Text, id :: Text, lang :: Text, path :: Text, size :: Text }

data Switch = Switch {
                language :: Text,
                matchThreshold :: Int,
                numberOfMatchesToShow :: Int,
                filesByDirectory :: Bool,
                comment :: Text,
                experimental :: Bool
            } deriving (Show, Eq)

defaultSwitches = Switch {
                    language="c",
                    matchThreshold=10,
                    numberOfMatchesToShow=250,
                    filesByDirectory=False,
                    comment="",
                    experimental=False
                }

-- Define our data that will be used for creating the form.

data MossForm = MossForm
  {
    fileInfo :: FileInfo,
    switch   :: Switch
  }


-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

data Language = C | CC | JAVA | ML | PASCAL | ADA | LISP | SCHEME | HASKELL | FORTRAN | ASCII | VHDL | PERL | MATLAB | PYTHON | MIPS | PROLOG | SPICE | VB | CSHARP | MODULA2 | A8086 | JAVASCRIPT | PLSQL deriving (Show, Eq, Enum, Bounded)

mosslanguages :: [(Language, Text)]
mosslanguages = [(C, "c"), (CC, "cc"), (JAVA, "java"), (ML, "ml"), (PASCAL, "pascal"), (ADA, "ada"), (LISP, "lisp"), (SCHEME, "scheme"), (HASKELL, "haskell"), (FORTRAN, "fortran"), (ASCII, "ascii"), (VHDL, "vhdl"), (PERL, "perl"), (MATLAB, "matlab"), (PYTHON, "python"), (MIPS, "mips"), (PROLOG, "prolog"), (SPICE, "spice"), (VB, "vb"), (CSHARP, "csharp"), (MODULA2, "modula2"), (A8086, "a8086"), (JAVASCRIPT, "javascript"), (PLSQL, "plsql")]

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe MossForm
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

sampleForm :: Form MossForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ MossForm
    <$> fileAFormReq "Choose a file"
    <*> areq switchesField switchSettings (Just defaultSwitches)
    -- <*> areq textField textSettings Nothing
    -- <*> areq (selectField optionsEnum) "Language" (Just C)
    -- Add attributes like the placeholder and CSS classes.
    where switchSettings = FieldSettings
            { fsLabel = "Moss settings"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "")
                ]
            }

switchthing :: Text -> Text -> Text -> Text -> Text -> Text -> Either (SomeMessage (HandlerSite Handler)) (Maybe Switch)
switchthing language tmatchThreshold tnumberOfMatchesToShow tfilesByDirectory comment texperimental
    | experimental                <- texperimental == "checked"
    , filesByDirectory            <- tfilesByDirectory == "checked"
    , Just numberOfMatchesToShow  <- readMaybe (T.unpack tnumberOfMatchesToShow) :: Maybe Int
    , Just matchThreshold         <- readMaybe (T.unpack tmatchThreshold) :: Maybe Int
    = Right $ Just $ Switch {language=language, matchThreshold=matchThreshold, numberOfMatchesToShow=numberOfMatchesToShow, filesByDirectory=filesByDirectory, comment=comment, experimental=experimental}

-- At some point do proper validation pass here
switchthing _ _ _ _ _ _ = Left "Missing switches"


switchesField :: Field Handler Switch
switchesField = Field {
                  fieldParse = \rawvals _filevals ->
                    case rawvals of
                      [comment, language, matchThreshold, numberOfMatchesToShow, "by-directory", "experimental"] ->
                        return $ switchthing language matchThreshold numberOfMatchesToShow "checked" comment "checked"
                      [comment, language, matchThreshold, numberOfMatchesToShow, "by-directory"] ->
                        return $ switchthing language matchThreshold numberOfMatchesToShow "checked" comment "unchecked"
                      [comment, language, matchThreshold, numberOfMatchesToShow, "experimental"] ->
                        return $ switchthing language matchThreshold numberOfMatchesToShow "unchecked" comment "checked"
                      [comment, language, matchThreshold, numberOfMatchesToShow] ->
                        return $ switchthing language matchThreshold numberOfMatchesToShow "unchecked" comment "unchecked"
                      [comment, language, matchThreshold, numberOfMatchesToShow, _, _] ->
                        return $ switchthing language matchThreshold numberOfMatchesToShow "unchecked" comment "unchecked"
                      _ -> return $ Left (fromString (show rawvals)),
                  fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
                                [whamlet|
                                  <br>
                                  <label for="comment">
                                    Comment
                                  <input type="text" id="comment" name=#{nameAttr}>
                                  <label for="languages">
                                    Language of source files
                                  <select id="language" name=#{nameAttr}>
                                    <option value="" disabled selected style="display: none">
                                      Select a language
                                    $forall opt <- mosslanguages
                                      <option value=#{snd opt}>
                                        #{snd opt}
                                  <br>
                                  <label for="match-threshold">
                                    Match Threshold
                                  <input type="number" id="match-threshold" name=#{nameAttr}>
                                  <label for="num-matches">
                                    Number of matches to show
                                  <input type="number" id="num-matches" name=#{nameAttr}>
                                  <label for="by-directory">
                                    Group files by directory
                                  <input type="checkbox" id="by-directory" value="by-directory" name=#{nameAttr}>
                                  <label for="experimental">
                                    Use experimental Moss server
                                  <input type="checkbox" id="experimental" value="experimental" name=#{nameAttr}>
                                |],
                  fieldEnctype = Multipart
              }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
