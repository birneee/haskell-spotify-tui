module Utils.StatusLenses where

import Control.Lens (Getter, to)
import Network.HTTP.Types (statusCode, Status)

code :: Getter Status Int
code = to statusCode