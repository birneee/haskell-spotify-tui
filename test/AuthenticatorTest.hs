module AuthenticatorTest where

import Authenticator (getAuthorizationCode, getRefreshToken, getAccessToken)

testAuthorization :: IO ()
testAuthorization = do
  ac <- getAuthorizationCode
  print $ ac
  rt <- getRefreshToken ac
  print $ rt
  at <- getAccessToken rt
  print at