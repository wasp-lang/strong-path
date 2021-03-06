module StrongPath.Path
  ( -- * Parsers (from "Path".'Path.Path' to 'StrongPath.Path')
    -- $parsersPath
    fromPathRelDir,
    fromPathRelFile,
    fromPathAbsDir,
    fromPathAbsFile,
    fromPathRelDirW,
    fromPathRelFileW,
    fromPathAbsDirW,
    fromPathAbsFileW,
    fromPathRelDirP,
    fromPathRelFileP,
    fromPathAbsDirP,
    fromPathAbsFileP,

    -- * Conversion (from 'StrongPath.Path' to "Path".'Path.Path')
    -- $conversionPath
    toPathRelDir,
    toPathRelFile,
    toPathAbsDir,
    toPathAbsFile,
    toPathRelDirW,
    toPathRelFileW,
    toPathAbsDirW,
    toPathAbsFileW,
    toPathRelDirP,
    toPathRelFileP,
    toPathAbsDirP,
    toPathAbsFileP,
  )
where

import qualified Path as P
import qualified Path.Posix as PP
import qualified Path.Windows as PW
import StrongPath.Internal

-- $parsersPath
-- Functions for parsing "Path" paths into "StrongPath" paths.

-- Constructors
fromPathRelDir :: P.Path P.Rel P.Dir -> Path System (Rel a) (Dir b)
fromPathRelFile :: P.Path P.Rel P.File -> Path System (Rel a) (File f)
fromPathAbsDir :: P.Path P.Abs P.Dir -> Path System Abs (Dir a)
fromPathAbsFile :: P.Path P.Abs P.File -> Path System Abs (File f)
fromPathRelDirW :: PW.Path PW.Rel PW.Dir -> Path Windows (Rel a) (Dir b)
fromPathRelFileW :: PW.Path PW.Rel PW.File -> Path Windows (Rel a) (File f)
fromPathAbsDirW :: PW.Path PW.Abs PW.Dir -> Path Windows Abs (Dir a)
fromPathAbsFileW :: PW.Path PW.Abs PW.File -> Path Windows Abs (File f)
fromPathRelDirP :: PP.Path PP.Rel PP.Dir -> Path Posix (Rel a) (Dir b)
fromPathRelFileP :: PP.Path PP.Rel PP.File -> Path Posix (Rel a) (File f)
fromPathAbsDirP :: PP.Path PP.Abs PP.Dir -> Path Posix Abs (Dir a)
fromPathAbsFileP :: PP.Path PP.Abs PP.File -> Path Posix Abs (File f)
---- System
fromPathRelDir p = RelDir p NoPrefix

fromPathRelFile p = RelFile p NoPrefix

fromPathAbsDir = AbsDir

fromPathAbsFile = AbsFile

---- Windows
fromPathRelDirW p = RelDirW p NoPrefix

fromPathRelFileW p = RelFileW p NoPrefix

fromPathAbsDirW = AbsDirW

fromPathAbsFileW = AbsFileW

---- Posix
fromPathRelDirP p = RelDirP p NoPrefix

fromPathRelFileP p = RelFileP p NoPrefix

fromPathAbsDirP = AbsDirP

fromPathAbsFileP = AbsFileP

-- $conversionPath
-- Functions for converting paths from "StrongPath" paths into "Path" paths.

-- TODO: Should I go with MonadThrow here instead of just throwing error? Probably!
--       I could, as error, return actual Path + info on how many ../ were there in StrongPath,
--       so user can recover from error and continue, if they wish.
-- Deconstructors
toPathRelDir :: Path System (Rel a) (Dir b) -> P.Path P.Rel P.Dir
toPathRelFile :: Path System (Rel a) (File f) -> P.Path P.Rel P.File
toPathAbsDir :: Path System Abs (Dir a) -> P.Path P.Abs P.Dir
toPathAbsFile :: Path System Abs (File f) -> P.Path P.Abs P.File
toPathRelDirW :: Path Windows (Rel a) (Dir b) -> PW.Path PW.Rel PW.Dir
toPathRelFileW :: Path Windows (Rel a) (File f) -> PW.Path PW.Rel PW.File
toPathAbsDirW :: Path Windows Abs (Dir a) -> PW.Path PW.Abs PW.Dir
toPathAbsFileW :: Path Windows Abs (File f) -> PW.Path PW.Abs PW.File
toPathRelDirP :: Path Posix (Rel a) (Dir b) -> PP.Path PP.Rel PP.Dir
toPathRelFileP :: Path Posix (Rel a) (File f) -> PP.Path PP.Rel PP.File
toPathAbsDirP :: Path Posix Abs (Dir a) -> PP.Path PP.Abs PP.Dir
toPathAbsFileP :: Path Posix Abs (File f) -> PP.Path PP.Abs PP.File
---- System
toPathRelDir (RelDir p NoPrefix) = p
toPathRelDir (RelDir _ _) = relativeStrongPathWithPrefixToPathError
toPathRelDir _ = impossible

toPathRelFile (RelFile p NoPrefix) = p
toPathRelFile (RelFile _ _) = relativeStrongPathWithPrefixToPathError
toPathRelFile _ = impossible

toPathAbsDir (AbsDir p) = p
toPathAbsDir _ = impossible

toPathAbsFile (AbsFile p) = p
toPathAbsFile _ = impossible

---- Windows
toPathRelDirW (RelDirW p NoPrefix) = p
toPathRelDirW (RelDirW _ _) = relativeStrongPathWithPrefixToPathError
toPathRelDirW _ = impossible

toPathRelFileW (RelFileW p NoPrefix) = p
toPathRelFileW (RelFileW _ _) = relativeStrongPathWithPrefixToPathError
toPathRelFileW _ = impossible

toPathAbsDirW (AbsDirW p) = p
toPathAbsDirW _ = impossible

toPathAbsFileW (AbsFileW p) = p
toPathAbsFileW _ = impossible

---- Posix
toPathRelDirP (RelDirP p NoPrefix) = p
toPathRelDirP (RelDirP _ _) = relativeStrongPathWithPrefixToPathError
toPathRelDirP _ = impossible

toPathRelFileP (RelFileP p NoPrefix) = p
toPathRelFileP (RelFileP _ _) = relativeStrongPathWithPrefixToPathError
toPathRelFileP _ = impossible

toPathAbsDirP (AbsDirP p) = p
toPathAbsDirP _ = impossible

toPathAbsFileP (AbsFileP p) = p
toPathAbsFileP _ = impossible

relativeStrongPathWithPrefixToPathError :: a
relativeStrongPathWithPrefixToPathError =
  error "Relative StrongPath.Path with prefix can't be converted into Path.Path."
