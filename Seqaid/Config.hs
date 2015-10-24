
-------------------------------------------------------------------------------

  {-# LANGUAGE CPP #-}

-- |
-- Module      :  Seqaid.Config
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Details concerning the @seqaid.config@ file.
--
-- The purpose of the @seqaid.config@ file is to control
-- the extent of coverage of the automatically injected
-- instrumentation harness.
--
-- At this early stage, additional Cabal flags and even some
-- per-module CPP switches are used to further vary the
-- behaviour of seqaid.  These alternatives will become
-- documented as they find a reflection in the @seqaid.config@.
-- Also, see "Seqaid.Ann" for a few user annotations, which
-- will also be deprecated soon...
--
-- The format of @seqaid.config@ is a seqence of lines, which
-- can be one of three kinds:
--
--  * Blank lines (containing only whitespace)
--  * Comment lines (first non-whitespace character is @#@)
--  * Configuration lines
--
-- Configuration lines begin with a keyword, and are followed by
-- one or more arguments.  Arguments are comma-separated.
-- Layout is used when keywords generate nested structure.
--
-- For instance, here is the @seqaid.config@ file from the
-- <http://hackage.haskell.org/package/leaky leaky> package:
--
-- >  package    leaky
-- >  module     Main
-- >    binds    duty
-- >      types  Types.TA
-- >  instances  Types.TA, Types.TB, Types.TC
--
-- (The extra whitespace after the keywords is purely cosmetic.)
--
-- More documentation is pending, but for additional explanations
-- about this particular example, please refer to
-- <http://www.fremissant.net/seqaid#using this document>.

  module Seqaid.Config where

-------------------------------------------------------------------------------

