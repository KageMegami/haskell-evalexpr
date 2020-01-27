{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_funEvalExpr (
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

bindir     = "/home/sora/delivery/fun_evalexpr/bonus/.stack-work/install/x86_64-linux/4443128f15690dedce3becc48f78c515dfb7dce2859a61dba87df86c08b2b541/8.6.5/bin"
libdir     = "/home/sora/delivery/fun_evalexpr/bonus/.stack-work/install/x86_64-linux/4443128f15690dedce3becc48f78c515dfb7dce2859a61dba87df86c08b2b541/8.6.5/lib/x86_64-linux-ghc-8.6.5/funEvalExpr-0.1.0.0-H1elcHzCFtz2rmjCTCuNj6"
dynlibdir  = "/home/sora/delivery/fun_evalexpr/bonus/.stack-work/install/x86_64-linux/4443128f15690dedce3becc48f78c515dfb7dce2859a61dba87df86c08b2b541/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/sora/delivery/fun_evalexpr/bonus/.stack-work/install/x86_64-linux/4443128f15690dedce3becc48f78c515dfb7dce2859a61dba87df86c08b2b541/8.6.5/share/x86_64-linux-ghc-8.6.5/funEvalExpr-0.1.0.0"
libexecdir = "/home/sora/delivery/fun_evalexpr/bonus/.stack-work/install/x86_64-linux/4443128f15690dedce3becc48f78c515dfb7dce2859a61dba87df86c08b2b541/8.6.5/libexec/x86_64-linux-ghc-8.6.5/funEvalExpr-0.1.0.0"
sysconfdir = "/home/sora/delivery/fun_evalexpr/bonus/.stack-work/install/x86_64-linux/4443128f15690dedce3becc48f78c515dfb7dce2859a61dba87df86c08b2b541/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "funEvalExpr_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "funEvalExpr_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "funEvalExpr_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "funEvalExpr_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "funEvalExpr_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "funEvalExpr_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
