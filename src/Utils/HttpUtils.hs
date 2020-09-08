module Utils.HttpUtils where

import Codec.Picture (PixelRGB8, convertRGB8, decodeImage)
import Codec.Picture.Types (Image)
import Control.Lens.Getter ((^.))
import Control.Lens.Operators ((&))
import Control.Lens.Setter ((.~))
import Control.Monad (join)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (find)
import Network.HTTP.Client
  ( Request,
    host,
    httpLbs,
    newManager,
    parseRequest_,
    path,
    port,
    queryString,
    secure,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status)
import Network.HTTP.Types.URI (Query, QueryItem)
import qualified Network.Wai as Wai (queryString)
import qualified Network.Wai.Internal as Wai (Request)
import Utils.MaybeUtils (rightToMaybe)
import Utils.RequestLenses (method)
import Utils.ResponseLenses (body, status)
import Utils.StringUtils (Unpackable (unpack))

-- | Create and url string from an Request type
toUrl :: Request -> String
toUrl r =
  (if secure r then "https://" else "http://")
    ++ (unpack $ host r)
    ++ ":"
    ++ (show $ port r)
    ++ (unpack $ path r)
    ++ (unpack $ queryString r)

-- | Check if a QueryItem key is equal to a String
isQueryKeyEquals :: String -> QueryItem -> Bool
isQueryKeyEquals name (key, _) = name == (unpack key)

-- | Find QueryItem value by key
getQueryParam :: String -> Query -> Maybe String
getQueryParam key query = do
  let item = find (isQueryKeyEquals key) query
  unpack <$> join (snd <$> item)

-- | Find QueryItem value by key
getRequestQueryParam :: String -> Wai.Request -> Maybe String
getRequestQueryParam param request = getQueryParam param (Wai.queryString request)

-- | Get image by url
getImage :: String -> IO (Status, Maybe (Image PixelRGB8))
getImage url = do
  let request =
        parseRequest_ url
          & method .~ "GET"
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  let rawBody = response ^. body
  let image = decode rawBody
  return
    ( response ^. status,
      image
    )
  where
    decode :: ByteString -> Maybe (Image PixelRGB8)
    decode bytes = convertRGB8 <$> (rightToMaybe $ decodeImage $ BS.pack $ LBS.unpack bytes)
