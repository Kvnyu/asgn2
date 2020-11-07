{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_rummy (
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

bindir     = "/home/kevin/Documents/UNI/2020/asgn2/.stack-work/install/x86_64-linux-tinfo6/2f5e14bbb7df826ac192cc04eaa35634a811355ba215f1cde444fd1403941ca3/8.8.2/bin"
libdir     = "/home/kevin/Documents/UNI/2020/asgn2/.stack-work/install/x86_64-linux-tinfo6/2f5e14bbb7df826ac192cc04eaa35634a811355ba215f1cde444fd1403941ca3/8.8.2/lib/x86_64-linux-ghc-8.8.2/rummy-0.1.0.0-1pFtqJ27uFSHcahoiT5L6v"
dynlibdir  = "/home/kevin/Documents/UNI/2020/asgn2/.stack-work/install/x86_64-linux-tinfo6/2f5e14bbb7df826ac192cc04eaa35634a811355ba215f1cde444fd1403941ca3/8.8.2/lib/x86_64-linux-ghc-8.8.2"
datadir    = "/home/kevin/Documents/UNI/2020/asgn2/.stack-work/install/x86_64-linux-tinfo6/2f5e14bbb7df826ac192cc04eaa35634a811355ba215f1cde444fd1403941ca3/8.8.2/share/x86_64-linux-ghc-8.8.2/rummy-0.1.0.0"
libexecdir = "/home/kevin/Documents/UNI/2020/asgn2/.stack-work/install/x86_64-linux-tinfo6/2f5e14bbb7df826ac192cc04eaa35634a811355ba215f1cde444fd1403941ca3/8.8.2/libexec/x86_64-linux-ghc-8.8.2/rummy-0.1.0.0"
sysconfdir = "/home/kevin/Documents/UNI/2020/asgn2/.stack-work/install/x86_64-linux-tinfo6/2f5e14bbb7df826ac192cc04eaa35634a811355ba215f1cde444fd1403941ca3/8.8.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "rummy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rummy_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "rummy_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "rummy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rummy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rummy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
