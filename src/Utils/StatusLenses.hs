-- |
--  Author: Benedikt Spies
--
--  Lenses for Network.HTTP Status data type
module Utils.StatusLenses where

import Control.Lens (Getter, to)
import Network.HTTP.Types (Status, statusCode)

code :: Getter Status Int
code = to statusCode