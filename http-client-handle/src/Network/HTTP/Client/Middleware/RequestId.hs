{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Network.HTTP.Client.Middleware.RequestId where

import           Prelude

import           Data.ByteString            (ByteString)
import           Data.Data                  (Data)
import           Data.Maybe                 (isJust)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        as Http
import           Network.HTTP.Client.Handle (Handle (..), modifyRequest)

newtype RequestId = RequestId ByteString
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | Add @X-Request-ID@ header to all outgoing requests.
--   Requests that already have this header are left as-is.
addRequestId :: RequestId -> Handle -> Handle
addRequestId (RequestId requestId) =
  modifyRequest $ \request -> pure request
    { Http.requestHeaders =
        if isJust $ lookup "X-Request-ID" $ Http.requestHeaders request
        then Http.requestHeaders request
        else ("X-Request-ID", requestId) : Http.requestHeaders request
    }

-- | Set @X-Request-ID@ header on all outgoing requests.
--   Removes all existing @X-Request-ID@ headers.
setRequestId :: RequestId -> Handle -> Handle
setRequestId (RequestId requestId) =
  modifyRequest $ \request -> pure request
    { Http.requestHeaders =
        ("X-Request-ID", requestId)
          :
        (filter (("X-Request-ID" /=) . fst) $ Http.requestHeaders request)
    }

