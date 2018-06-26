module Paths_FunGEn (
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
version = Version [1,0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\ivans\\Desktop\\PLC Game\\fungen\\.stack-work\\install\\cf5a45a3\\bin"
libdir     = "C:\\Users\\ivans\\Desktop\\PLC Game\\fungen\\.stack-work\\install\\cf5a45a3\\lib\\x86_64-windows-ghc-7.10.3\\FunGEn-1.0.1-1T23dZdtGeR6t6Mwex8FFS"
datadir    = "C:\\Users\\ivans\\Desktop\\PLC Game\\fungen\\.stack-work\\install\\cf5a45a3\\share\\x86_64-windows-ghc-7.10.3\\FunGEn-1.0.1"
libexecdir = "C:\\Users\\ivans\\Desktop\\PLC Game\\fungen\\.stack-work\\install\\cf5a45a3\\libexec"
sysconfdir = "C:\\Users\\ivans\\Desktop\\PLC Game\\fungen\\.stack-work\\install\\cf5a45a3\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "FunGEn_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "FunGEn_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "FunGEn_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "FunGEn_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "FunGEn_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
