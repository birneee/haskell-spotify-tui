{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_FFP_Project (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kai-chun/.cabal/bin"
libdir     = "/home/kai-chun/.cabal/lib/x86_64-linux-ghc-8.0.2/FFP-Project-0.1.0.0-3q65CeGrbhoK9m0bvbmme8"
dynlibdir  = "/home/kai-chun/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/kai-chun/.cabal/share/x86_64-linux-ghc-8.0.2/FFP-Project-0.1.0.0"
libexecdir = "/home/kai-chun/.cabal/libexec"
sysconfdir = "/home/kai-chun/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "FFP_Project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "FFP_Project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "FFP_Project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "FFP_Project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "FFP_Project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "FFP_Project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
