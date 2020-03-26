{-# LANGUAGE EmptyDataDecls #-}
module Bindings.Libsass.Types where

import           Foreign
import           Foreign.C

-- From sass/base.h

-- | Defines output style of compiled CSS.
data SassOutputStyle = SassStyleNested
                     | SassStyleExpanded
                     | SassStyleCompact
                     | SassStyleCompressed
                     | SassStyleInspect -- ^ Marked as internal
                     | SassStyleToSass -- ^ Marked as internal
                     | SassStyleToCss -- ^ Marked as internal
                     deriving (Eq, Show)

instance Enum SassOutputStyle where
    fromEnum SassStyleNested     = 0
    fromEnum SassStyleExpanded   = 1
    fromEnum SassStyleCompact    = 2
    fromEnum SassStyleCompressed = 3
    fromEnum SassStyleInspect    = 4
    fromEnum SassStyleToSass     = 5
    fromEnum SassStyleToCss      = 6

    toEnum 0 = SassStyleNested
    toEnum 1 = SassStyleExpanded
    toEnum 2 = SassStyleCompact
    toEnum 3 = SassStyleCompressed
    toEnum 4 = SassStyleInspect
    toEnum 5 = SassStyleToSass
    toEnum 6 = SassStyleToCss
    toEnum u = error ("SassOutputStyle.toEnum: Cannot match " ++ show u)

-- From sass/context.h

data SassContext
data SassFileContext
data SassDataContext

data SassCompilerState = SassCompilerCreated
                       | SassCompilerParsed
                       | SassCompilerExecuted
                       deriving (Show, Eq)

instance Enum SassCompilerState where
    fromEnum SassCompilerCreated  = 0
    fromEnum SassCompilerParsed   = 1
    fromEnum SassCompilerExecuted = 2

    toEnum 0 = SassCompilerCreated
    toEnum 1 = SassCompilerParsed
    toEnum 2 = SassCompilerExecuted
    toEnum u = error ("SassCompilerState.toEnum: Cannot match " ++ show u)

-- From sass/functions.h

data SassEnv
data SassCallee
data SassImport
data SassOptions
data SassCompiler
data SassImporter
data SassFunction

type SassEnvFrame = Ptr SassEnv
type SassCalleeEntry = Ptr SassCallee
type SassImportEntry = Ptr SassImport
type SassImportList = Ptr (Ptr SassImport)
type SassImporterEntry = Ptr SassImporter
type SassImporterList = Ptr (Ptr SassImporter)

type SassImporterFnType =
     CString
  -> SassImporterEntry
  -> Ptr SassCompiler
  -> IO SassImportList
type SassImporterFn = FunPtr SassImporterFnType

type SassFunctionEntry = Ptr SassFunction
type SassFunctionList = Ptr (Ptr SassFunction)

type SassFunctionFnType =
     Ptr SassValue
  -> SassFunctionEntry
  -> Ptr SassOptions
  -> IO (Ptr SassValue)
type SassFunctionFn = FunPtr SassFunctionFnType

data SassCalleeType = SassCalleeMixin
                    | SassCalleeFunction
                    | SassCalleeCFunction
                    deriving (Eq, Show)

instance Enum SassCalleeType where
    fromEnum SassCalleeMixin = 0
    fromEnum SassCalleeFunction = 1
    fromEnum SassCalleeCFunction = 2

    toEnum 0 = SassCalleeMixin
    toEnum 1 = SassCalleeFunction
    toEnum 2 = SassCalleeCFunction
    toEnum u = error ("SassCalleeType.toEnum: Cannot match " ++ show u)

-- From sass/values.h

data SassValue

data SassTag = SassBoolean
             | SassNumber
             | SassColor
             | SassString
             | SassList
             | SassMap
             | SassNull
             | SassError
             | SassWarning
             deriving (Eq, Show)

instance Enum SassTag where
    fromEnum SassBoolean = 0
    fromEnum SassNumber  = 1
    fromEnum SassColor   = 2
    fromEnum SassString  = 3
    fromEnum SassList    = 4
    fromEnum SassMap     = 5
    fromEnum SassNull    = 6
    fromEnum SassError   = 7
    fromEnum SassWarning = 8

    toEnum 0 = SassBoolean
    toEnum 1 = SassNumber
    toEnum 2 = SassColor
    toEnum 3 = SassString
    toEnum 4 = SassList
    toEnum 5 = SassMap
    toEnum 6 = SassNull
    toEnum 7 = SassError
    toEnum 8 = SassWarning
    toEnum u = error ("SassTag.toEnum: Cannot match " ++ show u)

-- | Separator used in Sass lists.
data SassSeparator = SassSeparatorComma
                   | SassSeparatorSpace
                   | SassSeparatorHash -- ^ Marked as internal
                   deriving (Eq, Show)

instance Enum SassSeparator where
    fromEnum SassSeparatorComma = 0
    fromEnum SassSeparatorSpace = 1
    fromEnum SassSeparatorHash  = 2

    toEnum 0 = SassSeparatorComma
    toEnum 1 = SassSeparatorSpace
    toEnum 2 = SassSeparatorHash
    toEnum u = error ("SassSeparator.toEnum: Cannot match " ++ show u)

-- | Operator used to combine two 'SassValue's.
data SassOp = SassAnd
            | SassOr
            | SassEq
            | SassNeq
            | SassGt
            | SassGte
            | SassLt
            | SassLte
            | SassAdd
            | SassSub
            | SassMul
            | SassDiv
            | SassMod
            | SassNumOps
            deriving (Eq, Show)

instance Enum SassOp where
    fromEnum SassAnd    = 0
    fromEnum SassOr     = 1
    fromEnum SassEq     = 2
    fromEnum SassNeq    = 3
    fromEnum SassGt     = 4
    fromEnum SassGte    = 5
    fromEnum SassLt     = 6
    fromEnum SassLte    = 7
    fromEnum SassAdd    = 8
    fromEnum SassSub    = 9
    fromEnum SassMul    = 10
    fromEnum SassDiv    = 11
    fromEnum SassMod    = 12
    fromEnum SassNumOps = 13

    toEnum 0  = SassAnd
    toEnum 1  = SassOr
    toEnum 2  = SassEq
    toEnum 3  = SassNeq
    toEnum 4  = SassGt
    toEnum 5  = SassGte
    toEnum 6  = SassLt
    toEnum 7  = SassLte
    toEnum 8  = SassAdd
    toEnum 9  = SassSub
    toEnum 10 = SassMul
    toEnum 11 = SassDiv
    toEnum 12 = SassMod
    toEnum 13 = SassNumOps
    toEnum u  = error ("SassOps.toEnum: Cannot match " ++ show u)
