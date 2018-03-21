{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Control.Concurrent.Async (race_)
import           Control.Exception        (throwIO)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as C
import           Data.Monoid              ((<>))
import           GHC.IO.Handle            (BufferMode (NoBuffering),
                                           hSetBuffering)
import           GHC.IO.Handle.FD         (stdout)

import           Pipes
import           Pipes.Network.TCP
import           Shadowsocks.Encrypt      (getEncDec)
import           Shadowsocks.Util

initRemote :: (ByteString -> IO ByteString)
           -> Consumer ByteString IO (Maybe (ByteString, Int))
initRemote decrypt = await >>= \encRequest -> do
  request <- liftIO $ decrypt encRequest
  case unpackRequest request of
    Right (_, destAddr, destPort, _) -> return $ Just (destAddr, destPort)
    Left addrType -> liftIO $ throwIO $ UnknownAddrType addrType

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  Config{..} <- parseConfigOptions
  C.putStrLn $ "starting server at " <> C.pack (show serverPort)
  serve "*" (show serverPort) $ \(client, _) -> do
    (encrypt, decrypt) <- getEncDec method password
    runEffect ((Nothing <$ fromSocket client 64) >-> initRemote decrypt) >>=
      \case Nothing -> error "connection closed unexpected"
            Just (dest, port) -> do
              C.putStrLn $ "connecting " <> dest <> ":" <> C.pack (show port)
              connect (C.unpack dest) (show port) $ \(server, _) -> do
                let forward = fromSocket client 64 >-> cryptPipe decrypt >-> toSocket server
                    back    = fromSocket server 64 >-> cryptPipe encrypt >-> toSocket client
                -- hangs?
                race_ (runEffect forward) (runEffect back)
