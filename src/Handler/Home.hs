{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Handler.Home where

import Import hiding (decodeUtf8, decodeUtf8', encodeUtf8, hGetContents, connect)
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
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import qualified Data.ByteString as BS
import System.IO
import Numeric (readDec)
import Text.Read (readMaybe)
import System.Environment (getEnv)
import System.IO.Temp (withSystemTempFile)
import qualified Data.Map as M
import Codec.Archive.Zip
import Path (parseAbsFile)
import Path.Internal
import Control.Monad.Trans.Class
import Conduit hiding (connect)

makea href = preEscapedToMarkup $ mconcat ["<a href=\"", href, "\">", href, "</a>"]

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
        let linkToResults :: Maybe PackResults = Nothing
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
          Just mossForm -> do
            -- First we create a temp file and dump the bytes we got there, as the zip library needs a real file backing it
            -- Then we get the list of descriptors, fmapping a descriptor -> FileData over that to get a FileData
            -- The descriptor -> FileData will have to pull out the bytes, get the length, get a 'canonical path', and generate a unique id.
            -- At this point we assume the same language for every file; we may revisit this.
            --  The next bit will have to be in some kind of async or something
            --  moss_response <- submitToMoss (switch mossForm) fileData
            --  then with the result we send an email
            fileData <- lift $ withSystemTempFile "moss.zip" $ \tmpFile handle -> do
              -- hSetBinaryMode handle True
              -- The very first thing we do is close the handle we are given. This is because we need to
              -- create and destroy handles to this file, and we can't just pass in the handle we already have.
              hClose handle
              liftIO $ do
                runConduitRes $ (fileSource $ fileInfo mossForm) .| (sinkFileBS tmpFile)
              all_descriptors <- withArchive (Path tmpFile) (M.keys <$> getEntries)
              let decorated_descriptors = zip (fmap (T.pack . show) (take (length all_descriptors) [1..])) all_descriptors
              maybe_files <- mapM (make_file tmpFile $ language $ switch mossForm) decorated_descriptors
              let files = catMaybes maybe_files
              return files
            linkToResultsTemp <- lift $ submitToMoss (switch mossForm) fileData
            let linkToResults :: Maybe PackResults = Just linkToResultsTemp
            setTitle "Files submitted to Moss"
            $(widgetFile "homepage") 
          _      -> do
            let linkToResults :: Maybe PackResults = Nothing
            setTitle "Moss submission webapp"
            $(widgetFile "homepage")

data PackResults = PackResults Text Text deriving (Typeable, Show)

make_file :: FilePath -> Text -> (Text, EntrySelector) -> IO (Maybe FileData)
make_file file lang (id, selector) = do
  let path = getEntryName selector
  bytes <- withArchive (Path file) (getEntry selector)
  case decodeUtf8' bytes of
    Left _ -> return Nothing
    Right contents -> do
        let size = T.pack ((show $ T.length contents) :: String)
        return $ Just $ FileData {contents=contents, size=size, path=path, lang=lang, id=id}

sampleForm :: Form MossForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ MossForm
    <$> fileAFormReq "Choose a file"
    <*> areq switchesField switchSettings (Just defaultSwitches)
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
                                    Any comment or label you want to include in the results.
                                  <br>
                                  <input type="text" id="comment" name=#{nameAttr}>
                                  <br>
                                  <label for="languages">
                                    Language of source files.
                                  <br>
                                  <select id="language" name=#{nameAttr}>
                                    <option value="" disabled selected style="display: none">
                                      Select a language
                                    $forall opt <- mosslanguages
                                      <option value=#{snd opt}>
                                        #{snd opt}
                                  <br>
                                  <label for="match-threshold">
                                    Match threshold. This sets the number of times a fingerprint can be found in different student's work before it is ignored. This is useful to set if you have a template file that most of your students will be using, but Moss cannot detect cheating rings larger than this number. It should be set to be larger than the largest size cheating ring you believe you will encounter.
                                  <br>
                                  <input type="number" id="match-threshold" name=#{nameAttr}>
                                  <br>
                                  <label for="num-matches">
                                    Maximum number of matches to show.
                                  <br>
                                  <input type="number" id="num-matches" name=#{nameAttr}>
                                  <br>
                                  <label for="by-directory">
                                    Treat each directory as a single student's work. If this option is not selected, Moss treats every file individually as a single student's work.
                                  <br>
                                  <input type="checkbox" id="by-directory" value="by-directory" name=#{nameAttr}>
                                  <br>
                                  <label for="experimental">
                                    Use experimental Moss server, rather than the usual stable Moss server.
                                  <br>
                                  <input type="checkbox" id="experimental" value="experimental" name=#{nameAttr}>
                                |],
                  fieldEnctype = Multipart
              }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

-- userID = "12345"

sendPrologue :: Switch -> Socket -> IO Text
sendPrologue Switch{..} sock = do
  userID <- getEnv "MOSSUserID"
  let prologue = (encodeUtf8 $ T.concat [
          "moss ", T.pack userID, "\n",
          "directory ", if filesByDirectory then "1" else "0", "\n",
          "X ", if experimental then "1" else "0", "\n",
          "maxmatches ", T.pack $ show matchThreshold, "\n",
          "show ", T.pack $ show numberOfMatchesToShow, "\n",
          "language ", language, "\n"
        ])
  sendAll sock prologue
  -- now check whether the options are all supported
  -- At this point, that theoretically means just the language
  supported <- recv sock 1024
  return $ decodeUtf8 supported

uploadFile :: Socket -> FileData -> IO ()
uploadFile sock FileData{..} = do
  let opening_stanza = T.concat ["file ", id, " ", lang, " ", size, " ", path, "\n" ]
  let total_message = T.concat [opening_stanza, contents]
  -- We can't actually do logging here until we thread the handler monad through instead of lifting it to IO back in the
  -- call to submitToMoss
  -- Not sure why this comment causes build failures on travis if left on it's own line: -- $(logInfo) total_message
  sendAll sock $ encodeUtf8 total_message

--  sendAll sock (encodeUtf8 opening_stanza)
--  sendAll sock (encodeUtf8 contents)
--  sendAll sock "done.\n"

submitToMoss :: Switch -> [FileData] -> IO PackResults
submitToMoss options files = withSocketsDo $ do
  let hints = defaultHints { addrFlags = [ AI_NUMERICSERV ] }
  addr:_ <- getAddrInfo (Just hints) (Just "moss.stanford.edu") (Just "7690")
  sock <- socket (addrFamily addr) Stream (addrProtocol addr)
  connect sock (addrAddress addr)
  -- Now we do blah blah blah
  supported <- sendPrologue options sock
  if (T.dropWhileEnd (=='\n') supported) == "no" then do
    sendAll sock (encodeUtf8 "end\n") -- Something in the prologue wasn't supported - bail out here
    return $ PackResults supported "Something in the prologue was not supported"
  else do
    -- We continue, and upload all the files
    mapM_ (uploadFile sock) files
    sendAll sock $ encodeUtf8 $ T.concat ["query 0 ", comment options, "\n"]
    response <- recv sock 4096
    sendAll sock $ encodeUtf8 "end\n"
    close sock
    return $ PackResults supported $ decodeUtf8 response
