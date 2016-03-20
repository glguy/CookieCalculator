{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Distribution.PackageDescription ( PackageDescription() )
import Distribution.InstalledPackageInfo
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..),
                                simpleUserHooks )
import Distribution.Simple.Utils ( rewriteFile,
                                        createDirectoryIfMissingVerbose )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ( ConfigFlags(configVerbosity,configProfLib),
                                      fromFlag, fromFlagOrDefault)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Verbosity ( Verbosity )
import Control.Monad (filterM)
import System.FilePath
import System.Directory

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { postConf = \args flags pkg lbi -> do
     generateBuildModule (fromFlag (configVerbosity flags)) pkg lbi
     postConf simpleUserHooks args flags pkg lbi
  }

-- | Generate a part of a Makefile which contains all libraries and
-- include locations used by the Cabal library.
generateBuildModule ::
  Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkgDesc lbi = do
  let autodir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True autodir
  let installDirs = absoluteInstallDirs pkgDesc lbi NoCopyDest
  withLibLBI pkgDesc lbi $ \_ libLBI ->
    do let [LibraryName thisLib] = componentLibraries libLBI
           thislib = (libdir installDirs </> "lib"++thisLib <.> "a")
           deplibs = [ dir </> "lib"++lib' <.> "a"
                     | pkg <- allPackages (installedPkgs lbi)
                     , dir <- libraryDirs pkg
                     , lib <- hsLibraries pkg
                     , let lib' = threadedVersion lib
                     ]

       -- Some libraries list multiple libraryDirs, we need to figure out
       -- where the actually Haskell library is.
       deplibs' <- filterM doesFileExist deplibs
       let libs = thislib : deplibs'
       rewriteFile (autodir </> "LIBRARY_LIST") (unlines libs)

       let pkgs = allPackages (installedPkgs lbi)
           libdirs = libdir installDirs : concatMap libraryDirs pkgs
           libNames = thisLib : map threadedVersion (concatMap hsLibraries pkgs)
           mkLibName x
             | fromFlagOrDefault False
                (configProfLib (configFlags lbi)) = x ++ "_p"
             | otherwise = x

           flags = unwords
                 $ map ("-L"++) libdirs
                ++ [ "-l" ++ mkLibName x | x <- libNames ]
       rewriteFile (autodir </> "LD_FLAGS") flags

-- We needed the threaded run-time so that SIGINT can be handled
-- cleanly when C code has called into Haskell
threadedVersion :: String -> String
threadedVersion lib =
  case lib of
    "Cffi" -> "Cffi_thr"
    "HSrts" -> "HSrts_thr"
    _ -> lib
