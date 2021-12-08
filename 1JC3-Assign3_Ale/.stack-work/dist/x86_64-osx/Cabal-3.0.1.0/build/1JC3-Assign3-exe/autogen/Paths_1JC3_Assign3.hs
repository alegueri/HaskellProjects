{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_1JC3_Assign3 (
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

bindir     = "/Users/alessandraguerinoni/Desktop/1JC3/1JC3-Assign3_Ale/.stack-work/install/x86_64-osx/b7b09ca93a7c08705b4354675bb3bca01023018ac35ff9b979617a57e5ed01b3/8.8.4/bin"
libdir     = "/Users/alessandraguerinoni/Desktop/1JC3/1JC3-Assign3_Ale/.stack-work/install/x86_64-osx/b7b09ca93a7c08705b4354675bb3bca01023018ac35ff9b979617a57e5ed01b3/8.8.4/lib/x86_64-osx-ghc-8.8.4/1JC3-Assign3-0.1.0.0-EfOqe5h3qBtLuIQw0cdtYH-1JC3-Assign3-exe"
dynlibdir  = "/Users/alessandraguerinoni/Desktop/1JC3/1JC3-Assign3_Ale/.stack-work/install/x86_64-osx/b7b09ca93a7c08705b4354675bb3bca01023018ac35ff9b979617a57e5ed01b3/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/alessandraguerinoni/Desktop/1JC3/1JC3-Assign3_Ale/.stack-work/install/x86_64-osx/b7b09ca93a7c08705b4354675bb3bca01023018ac35ff9b979617a57e5ed01b3/8.8.4/share/x86_64-osx-ghc-8.8.4/1JC3-Assign3-0.1.0.0"
libexecdir = "/Users/alessandraguerinoni/Desktop/1JC3/1JC3-Assign3_Ale/.stack-work/install/x86_64-osx/b7b09ca93a7c08705b4354675bb3bca01023018ac35ff9b979617a57e5ed01b3/8.8.4/libexec/x86_64-osx-ghc-8.8.4/1JC3-Assign3-0.1.0.0"
sysconfdir = "/Users/alessandraguerinoni/Desktop/1JC3/1JC3-Assign3_Ale/.stack-work/install/x86_64-osx/b7b09ca93a7c08705b4354675bb3bca01023018ac35ff9b979617a57e5ed01b3/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "1JC3_Assign3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "1JC3_Assign3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "1JC3_Assign3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "1JC3_Assign3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "1JC3_Assign3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "1JC3_Assign3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
