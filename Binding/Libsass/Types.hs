{-# LANGUAGE EmptyDataDecls #-}
module Binding.Libsass.Types where

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

-- ^ Separator used in Sass lists.
data SassSeparator = SassSeparatorComma
                   | SassSeparatorSpace
                   deriving (Eq, Show)

instance Enum SassSeparator where
    fromEnum SassSeparatorComma = 0
    fromEnum SassSeparatorSpace  = 1

    toEnum 0 = SassSeparatorComma
    toEnum 1 = SassSeparatorSpace
