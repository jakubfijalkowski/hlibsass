{-# LANGUAGE CPP #-}
import           Control.Monad                      (unless, when)
import           Data.Char                          (toLower)
import           Data.Maybe                         (fromJust, fromMaybe)
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo (InstallDirs (..),
                                                     LocalBuildInfo (..),
                                                     absoluteInstallDirs,
                                                     localPkgDescr)
import           Distribution.Simple.Program.Find   (findProgramOnSearchPath,
                                                     defaultProgramSearchPath)
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils          (cabalVersion,
                                                     installExecutableFile,
                                                     rawSystemExit,
                                                     rawSystemStdout)
import           Distribution.System
import qualified Distribution.Verbosity             as Verbosity
import           System.Directory                   (getCurrentDirectory)

#if MIN_VERSION_Cabal(2, 0, 0)
import           Distribution.Version               (mkVersion)
#else
mkVersion :: [Int] -> Version
mkVersion = flip Version []

mkFlagName :: String -> FlagName
mkFlagName = FlagName
#endif

main = defaultMainWithHooks hooksFix
    where
        hooks = simpleUserHooks {
            preConf = makeLibsass
          , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
          , postConf = disablePostConfHooks
          , preBuild = updateLibDirs
          , postCopy = copyLibsass
          , postClean = cleanLibsass
        }
        -- Fix for Cabal-1.18 - it does not `copy` on `install`, so we `copy` on
        -- `install` manually. ;)
        hooksFix = if cabalVersion < mkVersion [1, 20, 0]
                       then hooks { postInst = installLibsass }
                       else hooks
execMake :: Verbosity.Verbosity -> String -> String -> IO ()
execMake verbosity build_target target = do
    gmakePath <- findProgramOnSearchPath Verbosity.silent defaultProgramSearchPath "gmake"
    let makeExec = case gmakePath of 
#if MIN_VERSION_Cabal(1, 24, 0)
                     Just (p, _) -> p
#else
                     Just p -> p
#endif
                     Nothing -> "make"
        baseArgs = if null build_target
                      then [makeExec, "--directory=libsass"]
                      else ["BUILD=" ++ build_target, makeExec, "--directory=libsass"]
        makeArgs = if null target
                      then baseArgs
                      else baseArgs ++ [target]
    rawSystemExit verbosity "env" makeArgs

makeLibsass :: Args -> ConfigFlags -> IO HookedBuildInfo
makeLibsass _ f = do
    let verbosity = fromFlag $ configVerbosity f
        external = getCabalFlag "externalLibsass" f
        target = if getCabalFlag "sharedLibsass" f then "shared" else "static"
    unless external $ execMake verbosity target ""
    return emptyHookedBuildInfo

disablePostConfHooks :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
disablePostConfHooks args flags pd lbi
  | getCabalFlag "externalLibsass" flags = postConf simpleUserHooks args flags pd lbi
  | otherwise = return ()

updateLibDirs :: Args -> BuildFlags -> IO HookedBuildInfo
updateLibDirs _ _ = do
    dir <- getCurrentDirectory
    let libsassDir = dir ++ "/libsass/lib"
        bi = emptyBuildInfo { extraLibDirs = [ libsassDir ] }
    return (Just bi, [])

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs lbi
    | getCabalFlag "externalLibsass" $ configFlags lbi = return lbi
    | otherwise = do
        let pkg_descr = localPkgDescr lbi
            lib = fromJust $ library pkg_descr
            libBuild = libBuildInfo lib
            libPref = libdir $ absoluteInstallDirs pkg_descr lbi NoCopyDest
        return lbi {
            localPkgDescr = pkg_descr {
                library = Just $ lib {
                    libBuildInfo = libBuild {
                        extraLibDirs = libPref : extraLibDirs libBuild
                    }
                }
            }
        }

copyLib :: ConfigFlags -> LocalBuildInfo -> FilePath -> IO ()
copyLib fl lbi libPref =
    let verb = fromFlag $ configVerbosity fl
        external = getCabalFlag "externalLibsass" fl
        Platform _ os = hostPlatform lbi
        shared = getCabalFlag "sharedLibsass" fl
        ext = if shared then "so" else "a"
    in unless external $
        if os == Windows
            then do
                installExecutableFile verb
                    "libsass/lib/libsass.a"
                    (libPref ++ "/libsass.a")
                when shared $ installExecutableFile verb
                    "libsass/lib/libsass.dll"
                    (libPref ++ "/libsass.dll")
           else
                installExecutableFile verb
                    ("libsass/lib/libsass." ++ ext)
                    (libPref ++ "/libsass." ++ ext)

copyLibsass :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyLibsass _ flags pkg_descr lbi =
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
        config = configFlags lbi
     in copyLib config lbi libPref

installLibsass :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
installLibsass _ flags pkg_descr lbi =
    let libPref = libdir $ absoluteInstallDirs pkg_descr lbi NoCopyDest
        config = configFlags lbi
     in copyLib config lbi libPref

cleanLibsass :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanLibsass _ flags _ _ =
    execMake (fromFlag $ cleanVerbosity flags) "" "clean"

#if MIN_VERSION_Cabal(2, 2, 0)
getCabalFlag :: String -> ConfigFlags -> Bool
getCabalFlag name flags = fromMaybe False (lookupFlagAssignment (mkFlagName name') allFlags)
    where allFlags = configConfigurationsFlags flags
          name' = map toLower name
#else
getCabalFlag :: String -> ConfigFlags -> Bool
getCabalFlag name flags = fromMaybe False (lookup (mkFlagName name') allFlags)
    where allFlags = configConfigurationsFlags flags
          name' = map toLower name
#endif
