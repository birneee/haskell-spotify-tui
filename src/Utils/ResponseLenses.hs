-- |
--  Author: Benedikt Spies
--
--  Lenses for Network.HTTP.Client Response data type
module Utils.ResponseLenses where

import Control.Lens.Getter (Getter, to)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (Response, responseBody, responseStatus)
import Network.HTTP.Types (Status)

status :: Getter (Response ByteString) Status
status = to responseStatus

body :: Getter (Response ByteString) ByteString
body = to responseBody
