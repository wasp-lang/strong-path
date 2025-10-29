module Test.Utils
  ( systemFpRoot,
    posixToSystemFp,
    posixToWindowsFp,
  )
where

import System.FilePath as FP
import System.FilePath.Windows as FPW

systemFpRoot :: FilePath
systemFpRoot = if FP.pathSeparator == '\\' then "C:\\" else "/"

-- | Takes posix path and converts it into windows path if running on Windows or leaves as it is if on Unix.
posixToSystemFp :: FilePath -> FilePath
posixToSystemFp = convertPosixPath systemFpRoot FP.pathSeparator

-- | Takes posix path and converts it into windows path.
posixToWindowsFp :: FilePath -> FilePath
posixToWindowsFp = convertPosixPath "C:\\" FPW.pathSeparator

convertPosixPath :: FilePath -> Char -> FilePath -> FilePath
convertPosixPath rootReplacement separator posixFp =
  case posixFp of
    "" -> ""
    '/' : rest -> rootReplacement ++ map convertSeparator rest
    _ -> map convertSeparator posixFp
  where
    convertSeparator '/' = separator
    convertSeparator c = c
