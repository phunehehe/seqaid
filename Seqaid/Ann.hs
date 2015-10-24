
-------------------------------------------------------------------------------

  {-# LANGUAGE CPP #-}
  {-# LANGUAGE DeriveDataTypeable #-}

-------------------------------------------------------------------------------

-- |
-- Module      :  Seqaid.Ann
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC (portable, though not usefully)
--
-- There are a few user-level annotations here, but mostly
-- @seqaid@ is a non-invasive tool, so it uses a separate
-- @seqaid.config@ file instead.
--
-- See "Seqaid.Config" for details about the @seqaid.config@ file.

  module Seqaid.Ann (

    -- * User annotations

    -- | These are the Seqaid annotations a user can use in their source.

      SeqaidAnnExclude(..)
    , SeqaidAnnManual(..)

    -- * For internal use only

    -- | These annotations are for internal use.  They communicate
    -- information between the TH and GHC plugin stages of seqaid.

    , SeqaidAnnIncludeList(..)
    , SeqaidAnnTypes(..)
    , SeqaidAnnAvailableInstances(..)
    , SeqaidAnnBindsIncluded(..)

  ) where

  import Data.Typeable ( Typeable )
  import Data.Data ( Data )

#if TH_TYPE_IN_TYPES_ANN
  import Language.Haskell.TH ( Type )
--import Type ( Type )
#endif

-------------------------------------------------------------------------------

  -- I don't know why the Data instance is needed, but it is (GHC 7.8.1),
  -- but the polymorphism of the ANN pragma expression is likely responsible.

-------------------------------------------------------------------------------

  -- | With blanket top-level bind harnessing, this is a means
  -- to exclude select binds from harness.
  -- The @String@ argument need not (but may) be fully qualified.
  data SeqaidAnnExclude = SeqaidAnnExclude String
    deriving ( Typeable, Data, Show )

  -- | This was used for technical reasons, and is hopefully going
  -- to be deprecated very soon.
  -- At present, when you use 'seqaid' (not a real function, so no link)
  -- to manually wrap an expression for harnessing, you must also give
  -- a 'SeqaidAnnManual' annotation naming the bind you're editing.
  data SeqaidAnnManual = SeqaidAnnManual String
    deriving ( Typeable, Data, Show )

-------------------------------------------------------------------------------

  data SeqaidAnnIncludeList = SeqaidAnnIncludeList [String]
    deriving ( Typeable, Data, Show )

  data SeqaidAnnTypes = SeqaidAnnTypes [String]
    deriving ( Typeable, Data, Show )

#if TH_TYPE_IN_TYPES_ANN
  -- XXX Problem is, getting from TH.Type to Type.Type.
  -- We /could/ send [TH.Type] in the ANN, and try to cope
  -- with translation of it in Core -- at least it would be
  -- more principled than String/regex ad hoc parsing!
  -- But for now will stick to just String/regex.
  data SeqaidAnnAvailableInstances = SeqaidAnnAvailableInstances [Type]
    deriving ( Typeable, Data )
#else
  data SeqaidAnnAvailableInstances = SeqaidAnnAvailableInstances [String]
    deriving ( Typeable, Data, Show )
#endif

  data SeqaidAnnBindsIncluded = SeqaidAnnBindsIncluded [String]
    deriving ( Typeable, Data, Show )

-------------------------------------------------------------------------------

