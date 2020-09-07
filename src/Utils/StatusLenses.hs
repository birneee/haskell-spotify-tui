module Utils.StatusLenses where

import Control.Lens (Getter, to)
import Network.HTTP.Types (Status, statusCode)

code :: Getter Status Int
code = to statusCode