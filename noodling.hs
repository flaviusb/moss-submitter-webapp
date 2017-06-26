{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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

userID = "12345"

sendPrologue :: Switch -> Socket -> IO Text
sendPrologue Switch{..} sock = do
  let prologue = (encodeUtf8 $ T.concat [
          "moss ", userID, "\n",
          "directory ", if filesByDirectory then "1" else "0", "\n",
          "X ", if experimental then "1" else "0", "\n",
          "maxmatches ", T.pack $ show matchThreshold, "\n",
          "show ", T.pack $ show numberOfMatchesToShow, "\n",
          "language ", language, "\n"
        ])
  sendAll sock prologue
  -- now check whether the options are all supported
  -- At this point, that theoretically means just the language
  supported <- recv sock 16
  return $ decodeUtf8 supported

popFileData :: Text -> IO FileData
popFileData path = do
  handle <- openFile (T.unpack path) ReadMode  
  contents <- hGetContents handle  
  hClose handle
  let output = T.pack contents
  return FileData { contents=output, id="", lang="", path=path, size=(T.pack (show $ len $ encodeUtf8 output)) }

uploadFile :: Socket -> FileData -> IO ()
uploadFile sock FileData{..} = do
  let opening_stanza = T.concat ["file ", id, " ", lang, " ", size, " ", path, "\n" ]
  sendAll sock (encodeUtf8 opening_stanza)

submitToMoss :: Switch -> [FileData] -> IO ()
submitToMoss options files = withSocketsDo $ do
  let hints = defaultHints { addrFlags = [ AI_ALL, AI_NUMERICSERV ] }
  addr:_ <- getAddrInfo (Just hints) (Just "moss.stanford.edu") (Just "6790")
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  bind sock (addrAddress addr)
  -- Now we do blah blah blah
  supported <- sendPrologue options sock
  if (T.dropWhileEnd (=='\n') supported) == "no"
    then sendAll sock (encodeUtf8 "end\n") -- Something in the prologue wasn't supported - bail out here
  else
    -- We continue, and upload all the files
    mapM_ (uploadFile sock) files
