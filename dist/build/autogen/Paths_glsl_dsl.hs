module Paths_glsl_dsl (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/fiendfan1/.cabal/bin"
libdir     = "/home/fiendfan1/.cabal/lib/x86_64-linux-ghc-7.8.3/glsl-dsl-0.1.0.0"
datadir    = "/home/fiendfan1/.cabal/share/x86_64-linux-ghc-7.8.3/glsl-dsl-0.1.0.0"
libexecdir = "/home/fiendfan1/.cabal/libexec"
sysconfdir = "/home/fiendfan1/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "glsl_dsl_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "glsl_dsl_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "glsl_dsl_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "glsl_dsl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "glsl_dsl_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
