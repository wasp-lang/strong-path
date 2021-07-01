{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}

module StrongPath.TH
  ( -- ** QuasiQuoters
    -- $quasiQuoters
    absdir,
    absdirP,
    absdirW,
    absfile,
    absfileP,
    absfileW,
    reldir,
    reldirP,
    reldirW,
    relfile,
    relfileP,
    relfileW,
  )
where

import Control.Monad ((>=>))
import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (..))
import qualified Language.Haskell.TH.Syntax as TH
import StrongPath.FilePath
import StrongPath.Internal

-- $quasiQuoters
-- StrongPath provides quasi quoters that enable you to construct 'Path' in compile time.
-- You will need to enable 'QuasiQuotes' language extension in order to use them.
-- With quasi quoters, you can define paths like this:
--
-- > dirFooAbsPath :: Path System Abs (Dir FooDir)
-- > dirFooAbsPath = [absdir|/foo/bar|]
--
-- > someFile :: Path Posix (Rel FooDir) File ()
-- > someFile = [relfileP|some/file.txt|]
--
-- These will run at compile-time and underneath use the appropriate parser, ensuring that paths are valid and throwing compile-time error if not.

-- TODO: Split these into a separate module, StrongPath.QuasiQuoters, that will be reexported from this module.
--   This will also need extraction of some other parts of this module, in order to avoid cyclic imports.

qq ::
  (Lift p, Show err) =>
  (String -> Either err p) ->
  (p -> TH.ExpQ) ->
  QuasiQuoter
qq parse liftP =
  QuasiQuoter
    { quoteExp = either (fail . show) liftP . parse,
      quotePat = err "pattern",
      quoteType = err "type",
      quoteDec = err "declaration"
    }
  where
    err what x = fail ("unexpected " ++ what ++ ", must be expression: " ++ x)

liftPath :: TH.TypeQ -> TH.TypeQ -> TH.TypeQ -> Path s b t -> TH.ExpQ
liftPath s b t p = [|$(lift p) :: Path $s $b $t|]

typeVar :: String -> TH.TypeQ
typeVar = TH.newName >=> TH.varT

absdir, absdirP, absdirW :: QuasiQuoter
absdir = qq parseAbsDir (liftPath [t|System|] [t|Abs|] [t|Dir $(typeVar "d")|])
absdirP = qq parseAbsDirP (liftPath [t|Posix|] [t|Abs|] [t|Dir $(typeVar "d")|])
absdirW = qq parseAbsDirW (liftPath [t|Windows|] [t|Abs|] [t|Dir $(typeVar "d")|])

absfile, absfileP, absfileW :: QuasiQuoter
absfile = qq parseAbsFile (liftPath [t|System|] [t|Abs|] [t|File $(typeVar "f")|])
absfileP = qq parseAbsFileP (liftPath [t|Posix|] [t|Abs|] [t|File $(typeVar "f")|])
absfileW = qq parseAbsFileW (liftPath [t|Windows|] [t|Abs|] [t|File $(typeVar "f")|])

reldir, reldirP, reldirW :: QuasiQuoter
reldir = qq parseRelDir (liftPath [t|System|] [t|Rel $(typeVar "d1")|] [t|Dir $(typeVar "d2")|])
reldirP = qq parseRelDirP (liftPath [t|Posix|] [t|Rel $(typeVar "d1")|] [t|Dir $(typeVar "d2")|])
reldirW = qq parseRelDirW (liftPath [t|Windows|] [t|Rel $(typeVar "d1")|] [t|Dir $(typeVar "d2")|])

relfile, relfileP, relfileW :: QuasiQuoter
relfile = qq parseRelFile (liftPath [t|System|] [t|Rel $(typeVar "d")|] [t|File $(typeVar "f")|])
relfileP = qq parseRelFileP (liftPath [t|Posix|] [t|Rel $(typeVar "d")|] [t|File $(typeVar "f")|])
relfileW = qq parseRelFileW (liftPath [t|Windows|] [t|Rel $(typeVar "d")|] [t|File $(typeVar "f")|])
