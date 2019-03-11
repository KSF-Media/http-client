{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Network.HTTP.Client.Handle where

import           Prelude

import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import qualified Network.HTTP.Client as Http
import           Network.HTTP.Client.Conduit (Request, Response, bodyReaderSource,
                                                       defaultManagerSettings, parseRequest,
                                                       responseStatus)
import           UnliftIO                             (MonadUnliftIO, withRunInIO, bracket)

data Handle = Handle
  { handleResponseOpen  :: Http.Manager -> Http.Request -> IO (Response Http.BodyReader)
  , handleResponseClose :: forall a. Response a -> IO ()
  }

-- | Using capabilities of a given 'Handle' opens an 'Http.Response' and
--   passes it to the given action, then closes it.
withHttpResponse
  :: (MonadUnliftIO m)
  => Handle        -- ^ http client handle
  -> Http.Manager  -- ^ http connection manager
  -> Http.Request  -- ^ http request
  -> (Http.Response Http.BodyReader -> m a) -- ^ action to perform with response
  -> m a
withHttpResponse h manager request f =
  withRunInIO
    $ \runInIO -> bracket open close
    $ \response ->
        runInIO $ f response
  where
    open = handleResponseOpen h manager request
    close = handleResponseClose h

-- | Adjust the 'Handle' by passing each 'Http.Request' through a given action
--   before sending it.
modifyRequest
  :: (Http.Request -> IO Request)
  -> Handle -> Handle
modifyRequest f h = h
  { handleResponseOpen = \manager request -> do
      handleResponseOpen h manager =<< f request
  }

-- | Adjust the 'Handle' by passing each 'Http.Response' through a given action
--   right after opening it.
modifyOpenedResponse
  :: (Http.Response Http.BodyReader -> IO (Http.Response Http.BodyReader))
  -> Handle -> Handle
modifyOpenedResponse f h = h
  { handleResponseOpen = \manager request -> do
      f =<< handleResponseOpen h manager request
  }

newNetworkHandle :: IO Handle
newNetworkHandle = do
  pure Handle
    { handleResponseOpen = \request manager ->
        Http.responseOpen manager request
    , handleResponseClose = \response ->
        Http.responseClose response
    }
