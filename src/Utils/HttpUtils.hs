module Utils.HttpUtils where

import           Codec.Picture           (PixelRGB8, convertRGB8, decodeImage)
import           Codec.Picture.Types     (Image)
import           Control.Lens.Getter     ((^.))
import           Control.Lens.Operators  ((&))
import           Control.Lens.Setter     ((.~))
import           Control.Monad           (join)
import qualified Data.ByteString         as BS
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as LBS
import           Data.Foldable           (find)
import           Network.HTTP.Client     (Request, host, httpLbs, newManager,
                                          parseRequest_, path, port,
                                          queryString, secure)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types      (Status)
import           Network.HTTP.Types.URI  (Query, QueryItem)
import qualified Network.Wai             as Wai (queryString)
import qualified Network.Wai.Internal    as Wai (Request)
import           Relude                  (rightToMaybe)
import           Utils.RequestLenses     (method)
import           Utils.ResponseLenses    (body, status)
import           Utils.StringUtils

toUrl :: Request -> String
-- |Create and url string from an Request type
toUrl r =
  (if secure r then "https://" else "http://")
    ++ (unpack $ host r)
    ++ ":"
    ++ (show $ port r)
    ++ (unpack $ path r)
    ++ (unpack $ queryString r)

isQueryKeyEquals :: String -> QueryItem -> Bool
-- |Check if a QueryItem key is equal to a String
isQueryKeyEquals name (key, _) = name == (unpack key)

getQueryParam :: String -> Query -> Maybe String
-- |Find QueryItem value by key
getQueryParam key query = do
  let item = find (isQueryKeyEquals key) query
  unpack <$> join (snd <$> item)

getRequestQueryParam :: String -> Wai.Request -> Maybe String
-- |Find QueryItem value by key
getRequestQueryParam param request = getQueryParam param (Wai.queryString request)

getImage :: String -> IO (Status, Maybe (Image PixelRGB8))
-- |Get image by url
getImage url = do
  let request = parseRequest_ url
        & method .~ "GET"
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  let rawBody = response ^. body
  let image = decode rawBody
  return (
    response ^. status,
    image)
  where
    decode :: ByteString -> Maybe (Image PixelRGB8)
    decode bytes = convertRGB8 <$> (rightToMaybe $ decodeImage $ BS.pack $ LBS.unpack bytes)
