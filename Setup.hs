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
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils          (cabalVersion,
                                                     installExecutableFile,
                                                     rawSystemExit,
                                                     rawSystemStdout)
import           Distribution.System
import           System.Directory                   (getCurrentDirectory)

#ifdef MIN_VERSION_Cabal
#if MIN_VERSION_Cabal(2, 0, 0)
import           Distribution.Version               (mkVersion)
#else
mkVersion :: [Int] -> Version
mkVersion = flip Version []

mkFlagName :: String -> FlagName
mkFlagName = FlagName
#endif
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
          , preSDist = updateLibsassVersion
        }
        -- Fix for Cabal-1.18 - it does not `copy` on `install`, so we `copy` on
        -- `install` manually. ;)
        hooksFix = if cabalVersion < mkVersion [1, 20, 0]
                       then hooks { postInst = installLibsass }
                       else hooks

makeLibsass :: Args -> ConfigFlags -> IO HookedBuildInfo
makeLibsass _ f = do
    let verbosity = fromFlag $ configVerbosity f
        external = getCabalFlag "externalLibsass" f
        target = if getCabalFlag "sharedLibsass" f then "shared" else "static"
    unless external $ rawSystemExit verbosity "env"
         ["BUILD=" ++ target, "make", "--directory=libsass"]
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
    rawSystemExit (fromFlag $ cleanVerbosity flags) "env"
        ["make", "--directory=libsass", "clean"]

updateLibsassVersion :: Args -> SDistFlags -> IO HookedBuildInfo
updateLibsassVersion _ flags = do
    let verbosity = fromFlag $ sDistVerbosity flags
    ver <- rawSystemStdout verbosity "env" [ "git", "-C", "libsass", "describe",
        "--abbrev=4", "--dirty", "--always", "--tags" ]
    writeFile "libsass/VERSION" ver
    return emptyHookedBuildInfo

getCabalFlag :: String -> ConfigFlags -> Bool
getCabalFlag name flags = fromMaybe False (lookup (mkFlagName name') allFlags)
    where allFlags = configConfigurationsFlags flags
          name' = map toLower name
