module Utils.HttpUtils where

import           Control.Monad          (join)
import           Data.Foldable          (find)
import           Network.HTTP.Client    (Request, host, path, port, queryString,
                                         secure)
import           Network.HTTP.Types.URI (Query, QueryItem)
import qualified Network.Wai            as Wai (queryString)
import qualified Network.Wai.Internal   as Wai (Request)
import           Utils.StringUtils


toUrl :: Request -> String
toUrl r =
  (if secure r then "https://" else "http://")
    ++ (unpack $ host r)
    ++ ":"
    ++ (show $ port r)
    ++ (unpack $ path r)
    ++ (unpack $ queryString r)

isQueryKeyEquals :: String -> QueryItem -> Bool
isQueryKeyEquals name (key, _) = name == (unpack key)

getQueryParam :: String -> Query -> Maybe String
getQueryParam key query = do
  let item = find (isQueryKeyEquals key) query
  unpack <$> join (snd <$> item)

getRequestQueryParam :: String -> Wai.Request -> Maybe String
getRequestQueryParam param request = getQueryParam param (Wai.queryString request)
