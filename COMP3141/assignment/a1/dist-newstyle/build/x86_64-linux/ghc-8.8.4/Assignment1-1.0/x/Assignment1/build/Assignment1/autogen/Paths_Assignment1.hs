{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Assignment1 (
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
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/import/reed/2/z5416824/.cabal/bin"
libdir     = "/import/reed/2/z5416824/.cabal/lib/x86_64-linux-ghc-8.8.4/Assignment1-1.0-inplace-Assignment1"
dynlibdir  = "/import/reed/2/z5416824/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/import/reed/2/z5416824/.cabal/share/x86_64-linux-ghc-8.8.4/Assignment1-1.0"
libexecdir = "/import/reed/2/z5416824/.cabal/libexec/x86_64-linux-ghc-8.8.4/Assignment1-1.0"
sysconfdir = "/import/reed/2/z5416824/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Assignment1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Assignment1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Assignment1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Assignment1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Assignment1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Assignment1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
