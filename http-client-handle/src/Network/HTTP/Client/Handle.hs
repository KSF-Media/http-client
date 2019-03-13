{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RankNTypes         #-}
-- |
--   - Handle is injected into the functions defined in this
--     module in unopinioted way — as a first argument, each of
--     them can be easily promoted into a field of a 'Handle'
--     with no breakage.
module Network.HTTP.Client.Handle
  ( Handle()
  , handleResponseOpen
  , handleResponseClose
  , handleWithHttpResponse
  , handleWithHttpResponseC
  , modifyRequest
  , modifyOpenedResponse
  , newNetworkHandle
  ) where

import           Conduit                     (ConduitM, MonadResource, bracketP)
import qualified Network.HTTP.Client         as Http
import           Network.HTTP.Client.Conduit (Request, Response)
import           Prelude
import           UnliftIO                    (MonadUnliftIO, bracket, liftIO)

data Handle = Handle
  -- ???: maybe it would be good to keep a manager scoped as one of the fields instead of
  --      taking it as argument everywhere where it's needed
  --      (that would bind earlier, but usage would be less cumbersome)
  { handleResponseOpen  :: Http.Manager -> Http.Request -> IO (Response Http.BodyReader)
  , handleResponseClose :: forall a. Response a -> IO ()
  }

-- | Using capabilities of a given 'Handle' opens an 'Http.Response',
--   passes it to given action, then closes it (relies on 'bracket').
--
--   A usual caveat of 'bracket' applies here: after the action is
--   executed the response is closed and body can't be read anymore.
handleWithHttpResponse
  :: (MonadUnliftIO m)
  => Handle        -- ^ http client handle
  -> Http.Manager  -- ^ http connection manager
  -> Http.Request  -- ^ http request
  -> (Http.Response Http.BodyReader -> m a) -- ^ action to perform with response
  -> m a
handleWithHttpResponse h manager request consumer =
    bracket open close consumer
  where
    open = liftIO $ handleResponseOpen h manager request
    close = liftIO . handleResponseClose h

-- | Using capabilities of a given 'Handle' opens an 'Http.Response',
--   passes it to the given conduit and registers a finalizer that would
--   close the response once the consumer is sinked (relies on 'bracketP'
--   and 'ResourceT').
--
--   This function is particularly handy as it constructs a 'ConduitM' computation
--   that would implicitly initiate, process, and close the request.
--   This allows for convinient coding, but complexes the flow and prone to
--   resource overliving bugs and leaks.
--
--   To keep it simple, you can just use 'handleWithHttpResponse' and perform
--   all the processing (with a 'runConduit' call) in an action that you give to it:
--
--   @
--   -- request a largish body that would be sent in chunks
--   handleWithHttpResponse handle manager "https://stackage.org/nightly/cabal.config"
--     -- get a response with a 'Http.BodyReader'
--     \response -> runConduit do
--        -- make a stream of body chunks
--        Http.Conduit.bodyReaderSource (Http.responseBody response)
--          -- and write each of them to stdout
--          .| Conduit.mapM_C (liftIO . ByteString.putStr)
--   @
--
--   The only case when you might legitametely need 'handleWithHttpResponseC' is when
--   resource allocation is dynamic. For example when crawling the web recursively, it is
--   needed to make a request, read the body and then make more and more requests to the urls
--   found in these bodies. In such case 'MonadResource' might come handy.
--
--   For more info on this topic read <https://www.fpcomplete.com/blog/2018/10/resourcet-necessary-evil ResourceT: A necessary evil>
handleWithHttpResponseC
  :: (MonadResource m, MonadUnliftIO m)
  => Handle        -- ^ http client handle
  -> Http.Manager  -- ^ http connection manager
  -> Http.Request  -- ^ http request
  -> (Http.Response Http.BodyReader -> ConduitM i o m a) -- ^ response consumer
  -> ConduitM i o m a
handleWithHttpResponseC h manager request consumer =
    bracketP open close consumer
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

-- | A simple implementation of a 'Handle', can either be used as-is or extended
--   to suit application needs.
newNetworkHandle :: IO Handle
newNetworkHandle = do
  pure Handle
    { handleResponseOpen = \request manager ->
        Http.responseOpen manager request
    , handleResponseClose = \response ->
        Http.responseClose response
    }
