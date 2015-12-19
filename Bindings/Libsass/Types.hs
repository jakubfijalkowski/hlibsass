{-# LANGUAGE EmptyDataDecls #-}
module Bindings.Libsass.Types where

import           Foreign
import           Foreign.C.String

-- From sass.h

-- | Defines output style of compiled CSS.
data SassOutputStyle = SassStyleNested
                     | SassStyleExpanded
                     | SassStyleCompact
                     | SassStyleCompressed
                     deriving (Eq, Show)

instance Enum SassOutputStyle where
    fromEnum SassStyleNested     = 0
    fromEnum SassStyleExpanded   = 1
    fromEnum SassStyleCompact    = 2
    fromEnum SassStyleCompressed = 3

    toEnum 0 = SassStyleNested
    toEnum 1 = SassStyleExpanded
    toEnum 2 = SassStyleCompact
    toEnum 3 = SassStyleCompressed
    toEnum u = error ("SassOutputStyle.toEnum: Cannot match " ++ show u)

-- From sass_context.h

data SassCompiler
data SassOptions
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

-- From sass_functions.h

data SassImport
data SassImporter
data SassFunction

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

-- From sass_values.h

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

-- ^ Separator used in Sass lists.
data SassSeparator = SassSeparatorComma
                   | SassSeparatorSpace
                   deriving (Eq, Show)

instance Enum SassSeparator where
    fromEnum SassSeparatorComma = 0
    fromEnum SassSeparatorSpace  = 1

    toEnum 0 = SassSeparatorComma
    toEnum 1 = SassSeparatorSpace
    toEnum u = error ("SassSeparator.toEnum: Cannot match " ++ show u)

-- ^ Operator used to combine two 'SassValue's.
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
