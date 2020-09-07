{-# LANGUAGE RankNTypes #-}

module Utils.RequestLenses where

import Control.Lens (Lens', lens)
import Control.Lens.Setter (Setter')
import Data.Aeson (encode)
import Data.Aeson.Types (ToJSON)
import Data.ByteString (ByteString)
import qualified Network.HTTP.Client as H
  ( Request,
    RequestBody (RequestBodyLBS),
    method,
    requestBody,
    requestHeaders,
    secure,
    setQueryString,
    urlEncodedBody,
  )
import qualified Network.HTTP.Types as H (RequestHeaders)
import Utils.StringUtils

method :: Lens' H.Request String
method = lens getter setter
  where
    getter :: H.Request -> String
    getter req = unpack $ H.method req

    setter :: H.Request -> String -> H.Request
    setter req val = req {H.method = pack val}

requestHeaders :: Lens' H.Request H.RequestHeaders
requestHeaders = lens getter setter
  where
    getter :: H.Request -> H.RequestHeaders
    getter req = H.requestHeaders req

    setter :: H.Request -> H.RequestHeaders -> H.Request
    setter req val = req {H.requestHeaders = val}

secure :: Lens' H.Request Bool
secure = lens getter setter
  where
    getter :: H.Request -> Bool
    getter req = H.secure req

    setter :: H.Request -> Bool -> H.Request
    setter req val = req {H.secure = val}

queryString :: Setter' H.Request [(String, Maybe String)]
queryString = lens undefined setter
  where
    setter :: H.Request -> [(String, Maybe String)] -> H.Request
    setter req val = H.setQueryString (packQueryString val) req

urlEncodedBody :: Setter' H.Request [(String, String)]
urlEncodedBody = lens undefined setter
  where
    setter :: H.Request -> [(String, String)] -> H.Request
    setter req val = H.urlEncodedBody (packUrlEncodedBody val) req

jsonBody :: forall a. ToJSON a => Setter' H.Request a
jsonBody = lens undefined setter
  where
    setter :: forall a. ToJSON a => H.Request -> a -> H.Request
    setter req val = req {H.requestBody = H.RequestBodyLBS $ encode val}

-- helper functions
packQueryString :: [(String, Maybe String)] -> [(ByteString, Maybe ByteString)]
packQueryString l = packQueryParam <$> l
  where
    packQueryParam :: (String, Maybe String) -> (ByteString, Maybe ByteString)
    packQueryParam (key, val) = (pack key, pack <$> val)

packUrlEncodedBody :: [(String, String)] -> [(ByteString, ByteString)]
packUrlEncodedBody l = packParam <$> l
  where
    packParam :: (String, String) -> (ByteString, ByteString)
    packParam (key, val) = (pack key, pack val)
