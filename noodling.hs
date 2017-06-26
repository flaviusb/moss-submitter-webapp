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

data Switch = X


popFileData :: Text -> IO FileData
popFileData path = do
  handle <- openFile (T.unpack path) ReadMode  
  contents <- hGetContents handle  
  hClose handle
  let output = T.pack contents
  return FileData { contents=output, id="", lang="", path=path, size=(T.pack (show $ len $ encodeUtf8 output)) }

uploadFile :: FileData -> Socket -> IO ()
uploadFile FileData{..} sock = do
  let opening_stanza = T.concat ["file ", id, " ", lang, " ", size, " ", path, "\n" ]
  sendAll sock (encodeUtf8 opening_stanza)

submitToMoss :: [Switch] -> [FileData] -> IO ()
submitToMoss options files = withSocketsDo $ do
  let hints = defaultHints { addrFlags = [ AI_ALL, AI_NUMERICSERV ] }
  addr:_ <- getAddrInfo (Just hints) (Just "moss.stanford.edu") (Just "6790")
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  bind sock (addrAddress addr)
  -- Now we do blah blah blah
