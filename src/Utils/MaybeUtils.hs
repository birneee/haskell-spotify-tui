module Utils.MaybeUtils where

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just