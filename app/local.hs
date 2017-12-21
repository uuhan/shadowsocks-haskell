{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Control.Concurrent.Async (race_)
import           Control.Monad            (void)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as S
import qualified Data.ByteString.Char8    as C
import           Data.Monoid              ((<>))
import           GHC.IO.Handle            (BufferMode (NoBuffering),
                                           hSetBuffering)
import           GHC.IO.Handle.FD         (stdout)

import           Shadowsocks.Encrypt      (getEncDec)
import           Shadowsocks.Util
import           Pipes
import           Pipes.Network.TCP

initLocal :: Pipe ByteString ByteString IO ByteString
initLocal = do
  void await
  yield "\x05\x00"
  request <- await
  let (addrType, destAddr, destPort, _) =
          either (error . show . UnknownAddrType) id (unpackRequest $ S.drop 3 request)
      packed = packRequest addrType destAddr destPort
  yield "\x05\x00\x00\x01\x00\x00\x00\x00\x10\x10"
  liftIO $ C.putStrLn $ "connecting " <> destAddr
                                      <> ":" <> C.pack (show destPort)
  return packed

initRemote :: (ByteString -> IO ByteString)
           -> Pipe ByteString ByteString IO ()
initRemote encrypt = do
  addr <- await
  enc <- liftIO $ encrypt addr
  yield enc

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    Config{..} <- parseConfigOptions
    C.putStrLn $ "starting local at " <> C.pack (show localPort)
    serve "*" (show localPort) $ \(client, _) -> do
      (encrypt, decrypt) <- getEncDec method password
      connect server (show serverPort) $ \(server, _) -> do
        runEffect $ ((("closed" <$ fromSocket client 4096) >-> initLocal >-> ("closed" <$ toSocket client)) >~ initRemote encrypt) >-> toSocket server

        let forward = fromSocket client 4096 >-> cryptPipe encrypt >-> toSocket server
            back    = fromSocket server 4096 >-> cryptPipe decrypt >-> toSocket client

        race_ (runEffect forward) (runEffect back)
