{-# LANGUAGE TemplateHaskell #-}

module StrongPathQQ
  ( absdir, absdirP, absdirW
  , absfile, absfileP, absfileW
  , reldir, reldirP, reldirW
  , relfile, relfileP, relfileW
  ) where

import Language.Haskell.TH.Quote  (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Lift(..))

import qualified Path         as P
import qualified Path.Posix   as PP
import qualified Path.Windows as PW

import StrongPath

qq :: (Lift t, Show err)
   => (res -> t) -> (String -> Either err res) -> QuasiQuoter
qq from parse = QuasiQuoter
    { quoteExp  = either (error . show) (lift . from) . parse
    , quotePat  = err "pattern"
    , quoteType = err "type"
    , quoteDec  = err "declaration"
    }
  where
    err what x = fail ("unexpected " ++ what ++ ", must be expression: " ++ x)

absdir, absdirP, absdirW :: QuasiQuoter
absdir  = qq fromPathAbsDir  P.parseAbsDir
absdirP = qq fromPathAbsDirP PP.parseAbsDir
absdirW = qq fromPathAbsDirW PW.parseAbsDir

absfile, absfileP, absfileW :: QuasiQuoter
absfile  = qq fromPathAbsFile  P.parseAbsFile
absfileP = qq fromPathAbsFileP PP.parseAbsFile
absfileW = qq fromPathAbsFileW PW.parseAbsFile

reldir, reldirP, reldirW :: QuasiQuoter
reldir  = qq fromPathRelDir  P.parseRelDir
reldirP = qq fromPathRelDirP PP.parseRelDir
reldirW = qq fromPathRelDirW PW.parseRelDir

relfile, relfileP, relfileW :: QuasiQuoter
relfile  = qq fromPathRelFile  P.parseRelFile
relfileP = qq fromPathRelFileP PP.parseRelFile
relfileW = qq fromPathRelFileW PW.parseRelFile
