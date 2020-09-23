{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_addition (
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

bindir     = "/home/meowyam/haskell/haskellbook/testing/.stack-work/install/x86_64-linux-tinfo6/ba8c15192970e99a572aa870ed6b2c90a64baa3affd72e3cc5c5bded0a4e7efa/8.8.4/bin"
libdir     = "/home/meowyam/haskell/haskellbook/testing/.stack-work/install/x86_64-linux-tinfo6/ba8c15192970e99a572aa870ed6b2c90a64baa3affd72e3cc5c5bded0a4e7efa/8.8.4/lib/x86_64-linux-ghc-8.8.4/addition-0.1.0.0-42ImAsvR477EtaAHY6j0ki"
dynlibdir  = "/home/meowyam/haskell/haskellbook/testing/.stack-work/install/x86_64-linux-tinfo6/ba8c15192970e99a572aa870ed6b2c90a64baa3affd72e3cc5c5bded0a4e7efa/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/meowyam/haskell/haskellbook/testing/.stack-work/install/x86_64-linux-tinfo6/ba8c15192970e99a572aa870ed6b2c90a64baa3affd72e3cc5c5bded0a4e7efa/8.8.4/share/x86_64-linux-ghc-8.8.4/addition-0.1.0.0"
libexecdir = "/home/meowyam/haskell/haskellbook/testing/.stack-work/install/x86_64-linux-tinfo6/ba8c15192970e99a572aa870ed6b2c90a64baa3affd72e3cc5c5bded0a4e7efa/8.8.4/libexec/x86_64-linux-ghc-8.8.4/addition-0.1.0.0"
sysconfdir = "/home/meowyam/haskell/haskellbook/testing/.stack-work/install/x86_64-linux-tinfo6/ba8c15192970e99a572aa870ed6b2c90a64baa3affd72e3cc5c5bded0a4e7efa/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "addition_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "addition_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "addition_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "addition_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "addition_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "addition_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
