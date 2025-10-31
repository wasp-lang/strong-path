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
posixToSystemFp = convertPosixFp systemFpRoot FP.pathSeparator

-- | Takes posix path and converts it into windows path.
posixToWindowsFp :: FilePath -> FilePath
posixToWindowsFp = convertPosixFp "C:\\" FPW.pathSeparator

convertPosixFp :: FilePath -> Char -> FilePath -> FilePath
convertPosixFp newRoot newSeparator posixFp =
  case posixFp of
    "" -> error "Empty string is not a valid posix path."
    '/' : restOfAbsPosixFp -> newRoot ++ map convertSeparator restOfAbsPosixFp
    relPosixFp -> map convertSeparator relPosixFp
  where 
    convertSeparator c = if c == '/' then newSeparator else c
