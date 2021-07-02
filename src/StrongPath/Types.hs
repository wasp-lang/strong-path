{-# OPTIONS_HADDOCK hide #-}

module StrongPath.Types
  ( -- ** Types

    -- *** Path
    Path,

    -- **** 'Path' type
    Dir,
    File,

    -- **** 'Path' base
    Abs,
    Rel,

    -- **** 'Path' standard

    -- | TLDR: If you are not sure which standard to use, go with 'System' since that is the most
    -- common use case, and you will likely recognize the situation in which you need
    -- system-indepenent behaviour ('Posix', 'Windows') when it happens.
    Posix,
    Windows,
    System,

    -- **** 'Path' aliases
    Path',
    Rel',
    Dir',
    File',
  )
where

import StrongPath.Internal
