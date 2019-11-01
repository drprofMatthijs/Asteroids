{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_gloss_counter (
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

bindir     = "C:\\Users\\casbo\\Desktop\\Haskell\\Assignment_3\\gloss-counter\\.stack-work\\install\\f5b5193a\\bin"
libdir     = "C:\\Users\\casbo\\Desktop\\Haskell\\Assignment_3\\gloss-counter\\.stack-work\\install\\f5b5193a\\lib\\x86_64-windows-ghc-8.6.5\\gloss-counter-0.1.0.0-DINvQQGvGbLIsC8oIwUtj3-gloss-counter"
dynlibdir  = "C:\\Users\\casbo\\Desktop\\Haskell\\Assignment_3\\gloss-counter\\.stack-work\\install\\f5b5193a\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\casbo\\Desktop\\Haskell\\Assignment_3\\gloss-counter\\.stack-work\\install\\f5b5193a\\share\\x86_64-windows-ghc-8.6.5\\gloss-counter-0.1.0.0"
libexecdir = "C:\\Users\\casbo\\Desktop\\Haskell\\Assignment_3\\gloss-counter\\.stack-work\\install\\f5b5193a\\libexec\\x86_64-windows-ghc-8.6.5\\gloss-counter-0.1.0.0"
sysconfdir = "C:\\Users\\casbo\\Desktop\\Haskell\\Assignment_3\\gloss-counter\\.stack-work\\install\\f5b5193a\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gloss_counter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gloss_counter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "gloss_counter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "gloss_counter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gloss_counter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gloss_counter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
