import           Control.Monad                      (unless)
import           Data.Char                          (toLower)
import           Data.Maybe                         (fromJust, fromMaybe)
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo (InstallDirs (..),
                                                     LocalBuildInfo (..),
                                                     absoluteInstallDirs,
                                                     localPkgDescr)
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils          (installExecutableFile,
                                                     rawSystemExit,
                                                     rawSystemStdout)
import           System.Directory                   (getCurrentDirectory)

main = defaultMainWithHooks simpleUserHooks
  {
    preConf = \a f -> makeLibsass a f >> preConf simpleUserHooks a f
  , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
  , postCopy = copyLibsass
  , postClean = cleanLibsass
  , preSDist = \a f -> (updateLibsassVersion a f >> preSDist simpleUserHooks a f)
  }

makeLibsass :: Args -> ConfigFlags -> IO ()
makeLibsass _ f =
    let verbosity = fromFlag $ configVerbosity f
        external = getCabalFlag "externalLibsass" f
        target = if getCabalFlag "sharedLibsass" f then "shared" else "static"
    in unless external $ rawSystemExit verbosity "env"
         ["make", "--directory=libsass", target]

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs lbi
    | getCabalFlag "externalLibsass" $ configFlags lbi = return lbi
    | otherwise = do
        let packageDescription = localPkgDescr lbi
            lib = fromJust $ library packageDescription
            libBuild = libBuildInfo lib
        dir <- getCurrentDirectory
        return lbi {
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
copyLibsass _ flags pkg_descr lbi =
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
        verb = fromFlag $ copyVerbosity flags
        config = configFlags lbi
        external = getCabalFlag "externalLibsass" config
        ext = if getCabalFlag "sharedLibsass" config then "so" else "a"
    in unless external $
        installExecutableFile verb
            ("libsass/lib/libsass." ++ ext)
            (libPref ++ "/libsass." ++ ext)


cleanLibsass :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanLibsass _ flags _ _ =
    rawSystemExit (fromFlag $ cleanVerbosity flags) "env"
        ["make", "--directory=libsass", "clean"]

updateLibsassVersion :: Args -> SDistFlags -> IO ()
updateLibsassVersion _ flags = do
    let verbosity = fromFlag $ sDistVerbosity flags
    ver <- rawSystemStdout verbosity "env" [ "git", "-C", "libsass", "describe",
        "--abbrev=4", "--dirty", "--always", "--tags" ]
    writeFile "libsass/VERSION" ver

getCabalFlag :: String -> ConfigFlags -> Bool
getCabalFlag name flags = fromMaybe False (lookup (FlagName name') allFlags)
    where allFlags = configConfigurationsFlags flags
          name' = map toLower name
