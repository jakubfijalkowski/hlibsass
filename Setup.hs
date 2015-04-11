import           Data.Maybe                         (fromJust)
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo (InstallDirs (..),
                                                     LocalBuildInfo (..),
                                                     absoluteInstallDirs,
                                                     localPkgDescr)
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils          (rawSystemExit)
import           System.Directory                   (getCurrentDirectory)

main = defaultMainWithHooks simpleUserHooks
  {
    preConf = \a f -> makeLibsass a f >> preConf simpleUserHooks a f
  , copyHook = copyLibsass
  , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
  , postClean = cleanLibsass
  }

makeLibsass :: Args -> ConfigFlags -> IO ()
makeLibsass _ flags =
    rawSystemExit (fromFlag $ configVerbosity flags) "env"
        ["make", "--directory=libsass"]

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let packageDescription = localPkgDescr localBuildInfo
        lib = fromJust $ library packageDescription
        libBuild = libBuildInfo lib
    dir <- getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            library = Just $ lib {
                libBuildInfo = libBuild {
                    extraLibDirs = (dir ++ "/libsass/lib") :
                        extraLibDirs libBuild
                }
            }
        }
    }

copyLibsass :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags
            -> IO ()
copyLibsass pkg_descr lbi _ flags = do
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
    rawSystemExit (fromFlag $ copyVerbosity flags) "cp"
        ["libsass/lib/libsass.a", libPref]


cleanLibsass :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanLibsass _ flags _ _ =
    rawSystemExit (fromFlag $ cleanVerbosity flags) "env"
        ["make", "--directory=libsass", "clean"]
