{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cs517prj (
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

bindir     = "/Volumes/GoogleDrive/My Drive/OSU/2020/Spring/CS517/prj/code/cs517prj/.stack-work/install/x86_64-osx/58382100d7c1211d43a4b439518c4117b8bc0a2131cc7803bca804bbde3f2cfb/8.6.5/bin"
libdir     = "/Volumes/GoogleDrive/My Drive/OSU/2020/Spring/CS517/prj/code/cs517prj/.stack-work/install/x86_64-osx/58382100d7c1211d43a4b439518c4117b8bc0a2131cc7803bca804bbde3f2cfb/8.6.5/lib/x86_64-osx-ghc-8.6.5/cs517prj-0.1.0.0-6xGDKcf09oSElImMEFje2o-cs517prj-exe"
dynlibdir  = "/Volumes/GoogleDrive/My Drive/OSU/2020/Spring/CS517/prj/code/cs517prj/.stack-work/install/x86_64-osx/58382100d7c1211d43a4b439518c4117b8bc0a2131cc7803bca804bbde3f2cfb/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Volumes/GoogleDrive/My Drive/OSU/2020/Spring/CS517/prj/code/cs517prj/.stack-work/install/x86_64-osx/58382100d7c1211d43a4b439518c4117b8bc0a2131cc7803bca804bbde3f2cfb/8.6.5/share/x86_64-osx-ghc-8.6.5/cs517prj-0.1.0.0"
libexecdir = "/Volumes/GoogleDrive/My Drive/OSU/2020/Spring/CS517/prj/code/cs517prj/.stack-work/install/x86_64-osx/58382100d7c1211d43a4b439518c4117b8bc0a2131cc7803bca804bbde3f2cfb/8.6.5/libexec/x86_64-osx-ghc-8.6.5/cs517prj-0.1.0.0"
sysconfdir = "/Volumes/GoogleDrive/My Drive/OSU/2020/Spring/CS517/prj/code/cs517prj/.stack-work/install/x86_64-osx/58382100d7c1211d43a4b439518c4117b8bc0a2131cc7803bca804bbde3f2cfb/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cs517prj_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cs517prj_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cs517prj_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cs517prj_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cs517prj_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cs517prj_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
