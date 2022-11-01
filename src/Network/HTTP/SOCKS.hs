-- | This library provides non-TLS HTTP connections over SOCKS proxy.
-- If you need TLS with or without SOCKS, use @network-http-tls@.

module Network.HTTP.SOCKS (module Network.HTTP.SOCKS) where

import           Control.Exception            (IOException, bracketOnError,
                                               catch)
import           Network.Connection           as Network.HTTP.SOCKS (ProxySettings (..),
                                                                     SockSettings,
                                                                     initConnectionContext)
import qualified Network.Connection           as NC
import           Network.HTTP.Client          (ManagerSettings (..),
                                               defaultManagerSettings)
import           Network.HTTP.Client.Internal (Connection, makeConnection)

-- | An alias for 'SockSettingsSimple'. This specifies your SOCKS server.
-- See 'ProxySettings' for other proxy configuration options.
mkSockSettings :: String -> Int -> SockSettings
mkSockSettings host port = SockSettingsSimple host (fromIntegral port)

-- | Create 'ManagerSettings' with SOCKS configured.
-- The field manipulated here is 'managerRawConnection'; if you override this
-- field, the SOCKS configuration will be lost.
socksManagerSettings :: SockSettings -> IO ManagerSettings
socksManagerSettings socks = do
  ctx <- initConnectionContext
  return $ defaultManagerSettings
    { managerRawConnection =
        getSocksConnection (Just ctx) (Just socks)
    }

-- | Used in 'socksManagerSettings' to provide the value for the
-- 'managerRawConnection' field. This is lifted from @network-http-tls@, but the
-- TLS configuration is ripped out.
getSocksConnection :: Integral a
  => Maybe NC.ConnectionContext
  -> Maybe SockSettings
  -> IO (p -> [Char] -> a -> IO Connection)
getSocksConnection mcontext socks = do
    context <- maybe NC.initConnectionContext return mcontext
    return $ \_ha host' port' -> bracketOnError
        (NC.connectTo context NC.ConnectionParams
            { NC.connectionHostname = strippedHostName host'
            , NC.connectionPort = fromIntegral port'
            , NC.connectionUseSecure = Nothing
            , NC.connectionUseSocks = socks
            })
        NC.connectionClose
        convertConnection

convertConnection :: NC.Connection -> IO Connection
convertConnection conn = makeConnection
    (NC.connectionGetChunk conn)
    (NC.connectionPut conn)
    -- Closing an SSL connection gracefully involves writing/reading
    -- on the socket.  But when this is called the socket might be
    -- already closed, and we get a @ResourceVanished@.
    (NC.connectionClose conn `Control.Exception.catch` squashIOExceptions)
  where squashIOExceptions :: IOException -> IO ()
        squashIOExceptions _ = return ()

strippedHostName :: [Char] -> [Char]
strippedHostName hostName =
    case hostName of
        '[':'v':_ -> hostName -- IPvFuture, no obvious way to deal with this
        '[':rest ->
            case break (== ']') rest of
                (ipv6, "]") -> ipv6
                _           -> hostName -- invalid host name
        _ -> hostName
