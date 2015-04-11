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
  , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
  , postCopy = copyLibsass
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

copyLibsass :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyLibsass _ flags pkg_descr lbi = do
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
    let verb = fromFlag $ copyVerbosity flags
    rawSystemExit verb "mkdir" ["-p", libPref]
    rawSystemExit verb "cp" ["libsass/lib/libsass.a", libPref]


cleanLibsass :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanLibsass _ flags _ _ =
    rawSystemExit (fromFlag $ cleanVerbosity flags) "env"
        ["make", "--directory=libsass", "clean"]
