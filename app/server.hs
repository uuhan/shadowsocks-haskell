{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Conduit                  (Sink, await, liftIO, ($$), ($$+),
                                           ($$+-), (=$=))
import           Control.Concurrent.Async (race_)
import           Control.Exception        (throwIO)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as C
import           Data.Conduit             (catchC)
import           Data.Conduit.Network     (appSink, appSockAddr, appSource,
                                           clientSettings, runTCPClient,
                                           runTCPServer, serverSettings)
import           Data.Monoid              ((<>))
import           GHC.IO.Handle            (BufferMode (NoBuffering),
                                           hSetBuffering)
import           GHC.IO.Handle.FD         (stdout)

import           Shadowsocks.Encrypt      (getEncDec)
import           Shadowsocks.Util

initRemote :: (ByteString -> IO ByteString)
           -> Sink ByteString IO (ByteString, Int)
initRemote decrypt = await >>=
    maybe (liftIO $ throwIO NoRequestBody) (\encRequest -> do
        request <- liftIO $ decrypt encRequest
        case unpackRequest request of
            Right (_, destAddr, destPort, _) -> return (destAddr, destPort)
            Left addrType -> liftIO $ throwIO $ UnknownAddrType addrType
        )

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    Config{..} <- parseConfigOptions
    let localSettings = serverSettings serverPort "*"
    C.putStrLn $ "starting server at " <> C.pack (show serverPort)
    let
      tcpHandler = flip runTCPServer $ \client -> do
          (encrypt, decrypt) <- getEncDec method password
          (clientSource, (host, port)) <-
              appSource client $$+
                  initRemote decrypt `catchC` \e ->
                      error $ show (e :: SSException) <> " from "
                            <> showSockAddr (appSockAddr client)
          let remoteSettings = clientSettings port host
          C.putStrLn $ "connecting " <> host <> ":" <> C.pack (show port)
          runTCPClient remoteSettings $ \appServer -> race_
              (clientSource $$+- cryptConduit decrypt =$= appSink appServer)
              (appSource appServer $$ cryptConduit encrypt =$= appSink client)

    tcpHandler localSettings
