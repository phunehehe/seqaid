
{-# LANGUAGE CPP #-}

-- For a SYB worker type signature (probably unnecessary):
{-  LANGUAGE FlexibleContexts #-}

{-  LANGUAGE PatternGuards #-}

-- XXX In case didn't get around to it, there's some nonsense
-- with SiteID fst3 component: The only assignments that matters
-- are any made by the SYB traversal looking for manual injections.
-- And this traversal will take care of auto-injected ones, too,
-- they will not even need to be distinguished.
-- LATER: Also now, types-based injection sets the id.
-- My point above is that, we set it "early" to essentially bogus value
-- which is ... need to find a moment to review that stuff.  Some old
-- code can definitely be removed.

-------------------------------------------------------------------------------

#define DBG 0
#define DBG_BINDS 0
#define DBG_MAP_CREATION 0
#define DBG_MAP_LOOKUP 0
#define DBG_ANNOTATIONS 0
#define DBG_MANUAL 0
#define DBG_SEQINJECT_FUNC 0
#define DBG_OMNI 0

#define SILENT 0
#define MENTION_EXCLUDED 0

-------------------------------------------------------------------------------

-- XXX This (set =1) is probably a bad idea for a default.
-- Try to minimise spurious warnings instead; although we
-- don't expect spurious warnings come to think of it...
#define NO_WARN_SITE_MISSING_INSTANCE 0

-- This should be equal to Seqaid.TH's
-- INJECT_DUMMY_CLASS_AND_INSTANCE_TO_BLOCK_DEAD_CODE_ELIMINATION
#define DO_NOT_ELIDE_ANY_OF_THE_INJECTED_TH_SPLICES 1

-- Hack to try using class method instead of seqaidDispatch intermediary.
#define TRY_NO_SEQAIDDISPATCH_INTERMEDIARY 0
#define TRY_SIMPLY_NFDATA 0

-- Run "as much of thie module's code as possible" without
-- altering the original bindings. (There may be lack-of-demand
-- issues here...).
#define DRY_RUN 0

-- Return guts unmodified; skips most of this module's code.
#define BYPASS 0

-- Not necessary (was trying to simplify debugging at one point).
#define EXCLUDE_COLON_MAIN 0

-------------------------------------------------------------------------------

-- XXX Initially templated from the
-- strict-ghc-plugin (Austin Seipp / thoughtpolice).

-- XXX Comments below are now very old (from like, 1st or 2nd day
-- of working on the plugin):
--
-- -- Overview of the Problem
-- -- =======================
-- --
-- -- Want to inject CoreSyn Expr App of an imported, polymorphic function
-- -- (which uses methods of a certain class), to any suitable expression.
-- -- By suitable, meaning we may assume that the class instance exists
-- -- for the type of the expression, so that (or rather, because) the class
-- -- type constraint is satisfied.
-- --
-- -- The problem is, how to express this with hand-coded core injection?
-- --
-- -- I have some notion that, in Core, an overloaded function is really
-- -- a monomorphic function taking an additional dictionary argument.
-- -- What I can't find a good explanation of is, what, more concretely
-- -- (as befits a person trying to write a plugin, say...) can a person
-- -- do to inject such a function application to (perhaps annotated)
-- -- expressions?  I feel this is a good question (even for SO), since
-- -- it's such a natural and powerful thing to want to be able to do
-- -- in a plugin.
-- --
-- -- Well, I could test that I have a good grasp on doing this
-- -- for a MONOMORPHIC function -- but then, aren't I already
-- -- doing that in effect, with my single, Float-specialised seqaid_dud?

-------------------------------------------------------------------------------

-- -- What follows is a wretched hack until further notice.
-- 
-- -- 20140930 Getting closer. Dictionary manipulation (or Rules)
-- -- is probably the way to go -- unless there actually are API
-- -- functions to help do this?
-- --   In the meantime, can try to examine at the ctor level,
-- -- what I get as input to Core level ("simplifier?") ... should
-- -- actually double-check where in the pipeline this plugin
-- -- plugs-in...
-- --   And from Seqaid.Plugin.install:
-- --     return $ CoreDoPluginPass "Seqinject" seqinjectProgram : todos
-- -- So we inject (er, another use of the term) the seqaid plugin
-- -- at the very front. So we're interested in what manually-coded
-- -- calls to the injection function will look like at that point.
-- -- In principle, if can imitate that, ab initio, if we're "pure" [?]
-- -- (though we're in a monad...) we should get the same behaviour.
-- -- But environment ... the state of the CoreM monad must be consistent.
-- 
-- -- 20140929 A few days into this; so far, it is finally working
-- -- but only for the type of the seqaid_dud declaration (which
-- -- must be monomorphic or you get an error when plugin runs).
-- --   So the code contains working seqaidDispatch injections
-- -- at the original, manually-injected @Bool site, and at (the)
-- -- two @Float sites where we lucked out and had the right type
-- -- specialisation for the expression.
-- --   The other cases seem to "do nothing" (trace in seqaidDispatch
-- -- not printed), but no harm ensues.

  {-# LANGUAGE PatternGuards #-}

  {-# LANGUAGE BangPatterns #-}   -- debugging only!

-------------------------------------------------------------------------------

-- |
-- Module      :  Seqaid.Core
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC
--
-- Instrument a program with dynamic forcing functions.
--
-- This module is for internal use with the seqaid GHC plugin.
--
-- Refer to
-- <http://hackage.haskell.org/package/deepseq-bounded deepseq-bounded>
-- for more information about this methodology.

-------------------------------------------------------------------------------

  module Seqaid.Core ( seqinjectProgram ) where

  -- XXX for debugging only (prefer deepseq-bounded import, since then
  -- no need to state deepseq (or deepseq-generics) dep in .cabal.
  import Control.DeepSeq.Bounded ( force )
--import Control.DeepSeq.Generics ( force )  -- XXX for debugging only
--import Control.DeepSeq ( force )  -- XXX for debugging only

--import Data.Generics.Shape.SYB

  import GhcPlugins

  import Control.Monad
  import Data.Generics

  import Seqaid.Ann
  import Seqaid.Runtime ( SiteID )

--import qualified Var as GHC
  import qualified GHC
  import qualified FastString as GHC
  import qualified RdrName as GHC
  import qualified Id as GHC
  import qualified RnEnv as GHC
  import qualified HscMain as GHC
--import qualified GHC.Paths as GHC ( libdir )  -- package ghc-paths
  import qualified Type as GHC
  import qualified TyCon as GHC
  import qualified TypeRep as GHC

  import Data.Maybe

  import Data.Dynamic

  import Data.Data

--import GhcMonad ( liftIO )

  import Debug.Trace ( trace )

  import System.IO.Unsafe ( unsafePerformIO )

#if 1
  import qualified Data.Map as Map
  import           Data.Map ( Map )
#else
  import Prelude hiding ( lookup )
  import Data.Map hiding ( null, map, (!), filter )
#endif

  import Text.Regex.PCRE
  import Data.Array ( (!) )
  import Data.Array ( indices )
#if 0
  import Text.Regex.PCRE.String
  import Text.Regex.Base
  import Text.Regex.Base.RegexLike
#endif

  import Data.List ( intercalate )
  import Data.List ( deleteBy )
  import Data.List ( isPrefixOf )
  import Data.List ( foldl' )
  import Data.List ( nub )
  import Data.List ( sort )
  import Data.List ( group )

  import Data.Char ( isUpper )
  import Data.Char ( isLower )

#if ! DEMO_MODE
  import Data.Hashable ( hash )
#endif

  import Control.Monad.State.Lazy

-------------------------------------------------------------------------------

  data CoreBindMeta =
           -- (Bool,Bool) = (do_wrap_RHS, do_all_manual_within)
           Incl SiteID (Bool,Bool,Bool) CoreBind
         | Excl SiteID CoreBind
---      | Manual      CoreBind
--data CoreBindMeta = Incl SiteID CoreBind | Excl SiteID CoreBind

  unMeta :: CoreBindMeta -> CoreBind
  unMeta (Incl _ _ b) = b
--unMeta (Incl _ b) = b
  unMeta (Excl _ b) = b

-------------------------------------------------------------------------------

  normalMode = 1 :: Int
  preproMode = 2 :: Int

-------------------------------------------------------------------------------

  seqinjectProgram :: [String] -> ModGuts -> CoreM ModGuts
  seqinjectProgram opts' guts = do

   dflags <- getDynFlags

   let thismodname = showSDoc dflags $ ppr $ moduleName $ mg_module guts

   let opts = reverse opts'
#if DBG
   putMsgS $ "plugin opts = " ++ show opts
-- error "Core.hs-DEVEXIT"
#endif
   let (mode,mode_module)
         = let len = length opts in
           if 0 == len then (normalMode,"")
           else
             let modeopt = opts!!0
                 (modeopt_mode,modeopt_module)
                   = ( takeWhile (/='=') modeopt
                     , drop 1 $ dropWhile (/='=') modeopt
                     )
             in
             case len of
              1 -> case modeopt_mode of
                    "normal" -> (normalMode,"")
                    _ -> error "seqinj: incorrect plugin options (error #1)"
              3 -> case modeopt_mode of
                    "prepro" -> (preproMode,modeopt_module)
                    _ -> error "seqinj: incorrect plugin options (error #2)"
              _ -> error "seqinj: incorrect plugin options (error #3)"
   if mode == preproMode && mode_module == thismodname
   then do
    dflags <- getDynFlags
    -- XXX Here we need to traverse the binds, collect the
    -- types of all subexpressions, and dump the (nubbed)
    -- collection to a temp file for seqaidpp to pick up.
--  putMsgS "\nBoo!!\n"
    let omni_types_pname = opts!!1
    let omni_imports_pname = opts!!2
--  putMsgS $ "WOULD WRITE " ++ omni_types_pname
    seqaid_instance_strings <- get_seqaid_instance_strings guts
#if DBG_OMNI
    putMsgS $ " XXX seqaid_instance_strings =\n" ++ intercalate "\n" seqaid_instance_strings
--  error "DEVEXIT"
#endif
    omni_types' <- collectSubexpressionTypes guts
#if DBG_OMNI
    let tmp2 = nubsort $ map (sanitiseTypeString . showSDoc dflags . ppr) $ filter (not . isFunTy) omni_types'
    putMsgS $ " UUU omni_types =\n" ++ intercalate "\n" tmp2
#endif
    let omni_types'' = filter (not . isFunTy) omni_types'
    let omni_types''' = filter (not . isForAllTy) omni_types''
    let omni_types = filter (not . isDictLikeTy) omni_types'''
    -- (Can't nub the [Type], there's no Eq instance for Type.)
    let omni_types_strs = map (showSDoc dflags . ppr) omni_types
#if DBG_OMNI
    putMsgS $ " VVV omni_types_strs =\n" ++ intercalate "\n" omni_types_strs
#endif
    -- sort not nec. to nub, but it's helpful to see them sorted;
    -- wrote nubsort as it's more efficient than (nub . sort).
    let omni_types_strs_nubbed = nubsort omni_types_strs
    let omni_types_strs_nubbed' = map (map (\x -> if x == '\n' then ' ' else x)) omni_types_strs_nubbed
    let compactWhite [] = []
        compactWhite (' ':' ':t) = compactWhite (' ':t)
        compactWhite (h:t) = h:compactWhite t
    let omni_types_strs_nubbed'' = map compactWhite omni_types_strs_nubbed'
    let omni_types_strs_nubbed''' = filter (\x -> not (null x) && not (isLower (head x))) omni_types_strs_nubbed''
    let omni_types_strs_nubbed'''' = omni_types_strs_nubbed'''
    -- XXX Not sure if we should exclude boxed types or what...
    let omni_types_strs_nubbed''''' = filter (not . elem '#') omni_types_strs_nubbed''''
    let omni_types_strs_nubbed'''''' = filter (instancesAvailable seqaid_instance_strings) omni_types_strs_nubbed'''''
    let omni_types_string = intercalate "\n" omni_types_strs_nubbed''''''
#if DBG_OMNI
    putMsgS $ " YYY omni_types_string =\n" ++ omni_types_string
--  error "DEVEXIT"
#endif
    liftIO $ writeFile omni_types_pname omni_types_string
    let omni_imports_string' = generateOmniImports dflags omni_types
    let omni_imports_string = intercalate "\n" omni_imports_string'
--  let omni_imports_string = ""  -- XXX thack!
    liftIO $ writeFile omni_imports_pname omni_imports_string
    return guts

   else do

-- XXX Why am I content to just look at the head?
-- I /have/ seen more than one module name in a list!
-- (In fact, there's code someplace below to work around this,
-- or is it in the TH code?...)
    anns <- getAnnotations deserializeWithData guts
            :: CoreM (UniqFM [SeqaidAnnIncludeList])  -- (signature required)
    let annseltss = eltsUFM anns :: [[SeqaidAnnIncludeList]]
    let annselts_ = concat annseltss :: [SeqaidAnnIncludeList]
    let annselts = filter ( \ x -> let SeqaidAnnIncludeList y = x in (not $ null y) && thismodname == takeWhile (/='.') (head y)) annselts_ :: [SeqaidAnnIncludeList]

    -- Support for manually-instrumented code (Seqaid.Runtime.seqaid)
    -- requires also giving the SeqaidAnnManual ANN pragma, for now.
    mananns <- getAnnotations deserializeWithData guts
               :: CoreM (UniqFM [SeqaidAnnManual])  -- (signature required)
    let manannseltss = eltsUFM mananns :: [[SeqaidAnnManual]]
    let manannselts_ = concat manannseltss :: [SeqaidAnnManual]
    -- Drop Manual ANN's which name something outside the current module!
    let manannselts' = filter ( \ (SeqaidAnnManual y) -> not (elem '.' y) || thismodname /= takeWhile (/='.') y ) manannselts_ :: [SeqaidAnnManual]
    -- Now assure all are fully-qualified:
    let manannselts = map ( \ (SeqaidAnnManual y) -> SeqaidAnnManual (if elem '.' y then y else thismodname++"."++y) ) manannselts' :: [SeqaidAnnManual]

    typanns <- getAnnotations deserializeWithData guts
               :: CoreM (UniqFM [SeqaidAnnTypes])  -- (signature required)
    let typannseltss = eltsUFM typanns :: [[SeqaidAnnTypes]]
    let typannselts = foldl' (\ (SeqaidAnnTypes acclst) (SeqaidAnnTypes lst)->SeqaidAnnTypes (acclst++lst)) (SeqaidAnnTypes []) $ concat typannseltss :: SeqaidAnnTypes

    let types = (\ (SeqaidAnnTypes tslst) -> tslst ) typannselts
    let dotypes = not $ null types

    bianns <- getAnnotations deserializeWithData guts
              :: CoreM (UniqFM [SeqaidAnnBindsIncluded])  -- (sig. required)
    let biannseltss = eltsUFM bianns :: [[SeqaidAnnBindsIncluded]]
    let biannselts_ = concat biannseltss :: [SeqaidAnnBindsIncluded]
    let biannselts = map (\ (SeqaidAnnBindsIncluded x) -> SeqaidAnnBindsIncluded $ map (assureFQN thismodname) x) biannselts_ :: [SeqaidAnnBindsIncluded]

#if DBG_ANNOTATIONS
--- putMsgS $ "anns = " ++ show anns
--- putMsgS $ "anns = " ++ showSDoc dflags (ppr anns)
    putMsgS $ "annseltss = " ++ concatMap (('\n':) . show) annseltss
    putMsgS $ "annselts = " ++ concatMap (('\n':) . show) annselts
--  putMsgS $ "annselts = " ++ intercalate " " ( map show annselts )
--  putMsgS $ "annseltss = " ++ intercalate " " ( map (showSDoc dflags . ppr) annseltss )
--  putMsgS $ "annselts = " ++ intercalate " " ( map (showSDoc dflags . ppr) annselts )
    putMsgS $ "manannseltss = " ++ concatMap (('\n':) . show) manannseltss
    putMsgS $ "manannselts = " ++ concatMap (('\n':) . show) manannseltss
    putMsgS $ "typannseltss = " ++ concatMap (('\n':) . show) typannseltss
    putMsgS $ "typannselts = " ++ show typannselts
    putMsgS $ "biannseltss = " ++ concatMap (('\n':) . show) biannseltss
    putMsgS $ "biannselts = " ++ concatMap (('\n':) . show) biannselts
#endif

    if null annselts && null manannselts && ( null types || null biannselts )
--  if null annselts && null manannselts && null types
--  if null annselts && null manannselts && null typannselts
--  if null annselts
    then do
#if MENTION_EXCLUDED
#if ! SILENT
      putMsgS $ "Excluded from seqaid harness: " ++ thismodname
#endif
#endif
      return $ guts
    else do

#if ! SILENT
-- Used to print "Included in seqaid harness: ..." message here.
-- Then, it was moved downstream to become per-bind.
-- But finally, now it's done in TH.hs, so it can appear before
-- other messages from TH.
#endif

      let inclstrs'
           = let l = length annselts in
             case l of
              0 -> []
              1 -> let SeqaidAnnIncludeList inlst = head annselts in inlst
              _ -> error $ "seqaid: seqaid internal error!\nAt most one SeqaidAnnIncludeList annotation per module (you have " ++ show l ++ ")."

      let inclstrs
           = inclstrs' ++
             let l = length biannselts in
             case l of
              0 -> []
              1 -> let SeqaidAnnBindsIncluded inlst = head biannselts in inlst
              _ -> error $ "seqaid: seqaid internal error!\nAt most one SeqaidAnnBindsIncluded annotation per module (you have " ++ show l ++ ")."

      let maninclstrs = map ( \ (SeqaidAnnManual manin) -> manin ) manannselts

      let binds_ = mg_binds guts

#if DBG
      putmess "ppr inclstrs"
      putMsgS $ intercalate "\n" inclstrs
      putendmess
#endif

#if DBG_BINDS
      putmess "ppr binds_"
--    mapM (putStrLn . (prBindWithType dflags)) binds_
      mapM (prBindWithType dflags) binds_
--    mapM (printBind dflags) binds_
--    putMsgS $ showSDoc dflags (ppr binds_)
      putendmess
--    error "STOP"
#endif

      let names = map (nameOfBind dflags) binds_
      let (binds,dbinds,mbinds)
           = separateDummyInstanceDecls thismodname names binds_ ([],[],[])

#if DBG_BINDS
      putmess "ppr binds"
      mapM (printBind dflags) binds
--    putMsgS $ showSDoc dflags (ppr binds)
      putmess "ppr dbinds"
      mapM (printBind dflags) dbinds
      putmess "ppr mbinds"
      mapM (printBind dflags) mbinds
      putendmess
--    error "STOP"
#endif

      let (seqinj_noninst_binds, non_seqinj_binds'')
#if NO_TOP_LEVEL_SEQINJ_DUMMIES
            = collectSeqinjBinds dflags "$cseqinj" mbinds ([],[])
---         = collectSeqinjBinds dflags "$cseqinj" binds ([],[])
----        = collectSeqinjBinds dflags (thismodname++".$cseqinj") binds ([],[])
#else
            = collectSeqinjBinds dflags (thismodname++".seqinj") binds ([],[])
#endif
#if DBG_BINDS
      putmess "ppr seqinj_noninst_binds"
      mapM (printBind dflags) seqinj_noninst_binds
--    putMsgS $ showSDoc dflags (ppr seqinj_noninst_binds)
      putendmess
      putmess "ppr non_seqinj_binds''"
      mapM (printBind dflags) non_seqinj_binds''
--    putMsgS $ showSDoc dflags (ppr non_seqinj_binds'')
      putendmess
#endif
      let (seqinj_inst_binds, non_seqinj_binds')
            = collectSeqinjBinds dflags (thismodname++".seqinjinst") non_seqinj_binds'' ([],[])
#if DBG_BINDS
      putmess "ppr seqinj_inst_binds"
      mapM (printBind dflags) seqinj_inst_binds
--    putMsgS $ showSDoc dflags (ppr seqinj_inst_binds)
      putendmess
      putmess "ppr non_seqinj_binds'"
      mapM (printBind dflags) non_seqinj_binds'
--    putMsgS $ showSDoc dflags (ppr non_seqinj_binds')
      putendmess
#endif
      let seqinj_binds = seqinj_inst_binds ++ seqinj_noninst_binds
#if EXCLUDE_COLON_MAIN
      let deleteColonMainmain [] bs = error "deleteColonMainmain: unexpected!"
          deleteColonMainmain (h:t) bs
           | x@(NonRec n e) <- h, (showSDoc dflags $ ppr n) == ":Main.main"
              = (reverse bs++t, h)
           | otherwise
              = deleteColonMainmain t (h:bs)
      let (non_seqinj_binds, colon_main_bind)
           = deleteColonMainmain non_seqinj_binds' []
#else
      let non_seqinj_binds = non_seqinj_binds'
#endif
#if DBG_BINDS
#if 1
      putmess "ppr seqinj_noninst_binds"
      mapM (printBind dflags) seqinj_noninst_binds
--    putMsgS $ showSDoc dflags (ppr seqinj_noninst_binds)
      putendmess
#endif
#if 1
      putmess "ppr seqinj_inst_binds"
      mapM (printBind dflags) seqinj_inst_binds
--    putMsgS $ showSDoc dflags (ppr seqinj_inst_binds)
      putendmess
#endif
#if 0
      putmess "length seqinj_binds"
      putMsgS $ show $ length seqinj_binds
      putendmess
#endif
#if 1
      putmess "ppr non_seqinj_binds"
      mapM (printBind dflags) non_seqinj_binds
--    putMsgS $ showSDoc dflags (ppr non_seqinj_binds)
      putendmess
#endif
#endif
      let (non_seqinj_binds_synthetic, non_seqinj_binds_user)
            = splitSynthUser dflags non_seqinj_binds ([],[])
#if BYPASS
      return $ guts
#else
      non_seqinj_binds_meta
-- XXX By sending 1 as initial siteid, I'm implicitly subscribing
-- to the dangerous idea that reserving siteid=0 might be useful...
-- As for -ve Int's, I have (according to warning message the TH
-- part issues if user attempts to auto-harness a polymorphic bind)
-- that these are reserved for manual injections (where assuring
-- their uniqueness unfortunately becomes the user's responsibility).
        <- markNonSeqinjBinds dflags dotypes inclstrs maninclstrs non_seqinj_binds_user [] (1,"",0)
#if DBG_BINDS
      putmess "ppr non_seqinj_binds_included"
      mapM (printBind dflags) $ map unMeta non_seqinj_binds_meta
--    putMsgS $ showSDoc dflags (ppr non_seqinj_binds_included)
      putendmess
#endif
-- This is all the local variables referring to some binds!
-- (I'm debugging why my Map is empty at the moment...).
-- Apparently, seqinj_binds is [] at this point!
-- But anyway, seqinj_binds = seqinj_inst_binds ++ seqinj_noninst_binds.
-- And ... and, yeah, both of those on the RHS are indeed (of course), []...
---  binds
---  seqinj_noninst_binds
---  non_seqinj_binds''
---  seqinj_inst_binds
---  non_seqinj_binds'
---  seqinj_binds
---  non_seqinj_binds
---  non_seqinj_binds_synthetic
---  non_seqinj_binds_user
---  non_seqinj_binds_included
---  non_seqinj_binds_excluded
      let seqinj_map = makeMapSeqinjBinds dflags seqinj_binds
      let !_ = force seqinj_map
#if DBG
#if 1
      putmess "ppr seqinj_map"
      putMsgS $ show $ size seqinj_map
      putMsgS $ intercalate "\n" $ map show $ Map.toList seqinj_map
--    putMsgS $ showSDoc dflags (ppr seqinj_map)
      putendmess
#endif
#endif
#if DBG_BINDS
      putmess "ppr non_seqinj_binds_meta"
      mapM (prMetaBind dflags) non_seqinj_binds_meta
--    putMsgS $ showSDoc dflags (ppr non_seqinj_binds_meta)
      putendmess
#endif
--    error "STOP"
      newBinds' <- mapM
                     (seqinjectFuncPlus seqinj_map seqinj_binds guts types)
                     non_seqinj_binds_meta
--                   (map unMeta non_seqinj_binds_meta)  -- oops!
      -- Don't do any unnecessary reordering of included and excluded;
      -- implement so can preserve (or restore) original ordering to
      -- declarations, to the greatest possible extent...
      let newBinds''
           =    non_seqinj_binds_synthetic
#if DO_NOT_ELIDE_ANY_OF_THE_INJECTED_TH_SPLICES
             -- XXX all are needed! (when last I checked)
             --     or you need some environment modifcn...
             -- (This is when using
             -- INJECT_DUMMY_CLASS_AND_INSTANCE_TO_BLOCK_DEAD_CODE_ELIMINATION
             -- in Seqaid/TH.hs.)
-- As for this, if you don't, then you get duplicate mbinds.
#if ! NO_TOP_LEVEL_SEQINJ_DUMMIES
             ++ seqinj_binds
#endif
             ++ mbinds
             ++ dbinds
#endif
             ++ newBinds'  -- which now includes non_seqinj_binds_excluded
                           -- interleaved in the original order (nec.)
#if EXCLUDE_COLON_MAIN
      let newBinds = newBinds'' ++ [colon_main_bind]
#else
      let newBinds = newBinds''
#endif
#if DBG_BINDS
#if 1
      putmess "ppr newBinds"
--    mapM (printBind dflags) newBinds          -- names only
      putMsgS $ showSDoc dflags (ppr newBinds)  -- full decls
      putendmess
#endif
#endif
      return $ guts { mg_binds = newBinds }
#endif
   where
    printBind :: DynFlags -> CoreBind -> CoreM CoreBind
    printBind dflags bndr@(NonRec b _) = do
      putMsgS $ "Non-recursive binding named " ++ showSDoc dflags (ppr b)
      return bndr
    printBind dflags bndr@(Rec bes) = do
      putMsgS $ "Recursive binding named " ++ showSDoc dflags (ppr (fst (head bes)))
      return bndr
    printBind _ bndr = return bndr

-------------------------------------------------------------------------------

  prBind :: DynFlags -> CoreBind -> CoreM ()
  prBind dflags bndr@(NonRec b _) = do
    putMsgS $ "Non-recursive binding named " ++ showSDoc dflags (ppr b)
  prBind dflags bndr@(Rec bes) = do
    putMsgS $ "Recursive binding named " ++ showSDoc dflags (ppr (fst (head bes)))
  prBind _ bndr = error "prBind: unexpected!"

  prBindWithType :: DynFlags -> CoreBind -> CoreM String
  prBindWithType dflags bndr = do
    let name_part
         = case bndr of
             NonRec b _ -> "NR " ++ showSDoc dflags (ppr b)
             Rec bes -> " R " ++ showSDoc dflags (ppr $ fst $ head bes)
    let type_part'' = typeOfBind bndr
    let type_part' = showSDoc dflags (ppr type_part'')
    let type_part = map (\c->if c == '\n' then ' ' else c) type_part'
    let rslt = name_part ++ " :: " ++ type_part
    putMsgS rslt
    return rslt
  prBindWithType _ bndr = error "prBindWithType: unexpected!"

  prMetaBind :: DynFlags -> CoreBindMeta -> CoreM ()
  prMetaBind dflags inex = do
    let (inexstr,sid,dododo,bndr) = case inex of
          Incl sid dododo@(do_wrap,do_man,do_typ) bndr -> ("INCL",sid,dododo,bndr)
          Excl sid                       bndr -> ("EXCL",sid,(False,False,False),bndr)
    case bndr of
      NonRec b _ -> putMsgS $ inexstr ++ " NR name=" ++ showSDoc dflags (ppr b)
                    ++ " sid=" ++ show sid ++ " (do_wrap,do_man,do_typ)=" ++ show dododo
      Rec bes    -> putMsgS $ inexstr ++ "  R name=" ++ showSDoc dflags (ppr (fst (head bes)))
                    ++ " sid=" ++ show sid ++ " (do_wrap,do_man,do_typ)=" ++ show dododo
  prMetaBind _ bndr = error "prMetaBind: unexpected!"

-------------------------------------------------------------------------------

  splitSynthUser :: DynFlags -> [CoreBind] -> ([CoreBind],[CoreBind]) -> ([CoreBind],[CoreBind])
  splitSynthUser dflags [] (ss,us) = (reverse ss,reverse us)
--splitSynthUser dflags [] r@(ss,us) = r
  splitSynthUser dflags (h:t) (ss,us)
    = case h of
        bndr@(NonRec b _) ->
          if head (showSDoc dflags (ppr b)) == '$'
--        if head b == '$'
          then splitSynthUser dflags t (h:ss,  us)
          else splitSynthUser dflags t (  ss,h:us)
        bndr@(Rec bes) ->
          if head (showSDoc dflags (ppr bes)) == '$'
          then splitSynthUser dflags t (h:ss,  us)
          else splitSynthUser dflags t (  ss,h:us)
--        error "#2093857"

-------------------------------------------------------------------------------

  markNonSeqinjBinds :: DynFlags -> Bool -> [String] -> [String] -> [CoreBind] -> [CoreBindMeta] -> SiteID -> CoreM [CoreBindMeta]
  markNonSeqinjBinds dflags dotypes exorinstrs maninclstrs [] acc siteid = return $ reverse acc
  markNonSeqinjBinds dflags dotypes exorinstrs maninclstrs (h:t) acc siteid
    | bndr@(NonRec b _) <- h
       = do
            let bname = showSDoc dflags (ppr b)
            let dododo@(do_wrap, do_man, do_typ)
                 = ( bname `elem` exorinstrs
                   , bname `elem` maninclstrs
                   , dotypes && do_wrap
                   )
            if do_wrap || do_man || do_typ
            then
#if DBG
                 trace ("!i! "++bname) $
#endif
                 markNonSeqinjBinds dflags dotypes exorinstrs maninclstrs t (Incl siteid dododo h : acc) siteid_next
            else
#if DBG
                 trace ("!x! "++bname) $
#endif
                 markNonSeqinjBinds dflags dotypes exorinstrs maninclstrs t (Excl siteid h : acc) siteid_next
    | bndr@(Rec bes) <- h
       = do
            let (b,_) = head bes
            let bname = showSDoc dflags (ppr b)
            let dododo@(do_wrap, do_man, do_typ)
                 = ( bname `elem` exorinstrs
                   , bname `elem` maninclstrs
                   , dotypes && do_wrap
                   )
            if do_wrap || do_man || do_typ
            then
#if DBG
                 trace ("!i! "++bname) $
#endif
                 markNonSeqinjBinds dflags dotypes exorinstrs maninclstrs t (Incl siteid dododo h : acc) siteid_next
            else
#if DBG
                 trace ("!x! "++bname) $
#endif
                 markNonSeqinjBinds dflags dotypes exorinstrs maninclstrs t (Excl siteid h : acc) siteid_next
--- | bndr@(Rec bes) <- h     = error "markNonSeqinjBinds: Rec unimplemeted!"
    | otherwise               = error "markNonSeqinjBinds: unexpected!"
    where
-- XXX I'm pretty sure all but the name part (snd3 component) are
-- going to get overwritten downstream, now, since planning to
-- support manual injection via SYB renumbering...
-- (That's okay though; and doing this still here will help
-- keep old code working that bit longer while transition...)
     (siteid_idx,siteid_name,_) = siteid
#if DEMO_MODE
     siteid_hash = 0
#else
     siteid_hash = hash (siteid_name++show siteid_idx)
#endif
     siteid_next = (1+siteid_idx, siteid_name, siteid_hash)
--   siteid_next = (1 + fst siteid, snd siteid)

-------------------------------------------------------------------------------

  -- Note this is not general-purpose; it is for getting the RHS's
  -- of seqinj_* binds.
  getBindRHS :: ModGuts -> DynFlags -> CoreBind -> CoreM CoreExpr
  getBindRHS guts dflags bind = do
-- I don't recall seeing these traces for some time, actually?...
#if DBG
    let !_ = trace ( "#$#-bind " ++ (showSDoc dflags $ ppr bind)) $ ()
#endif
    let ecb | x@(NonRec b e) <- bind  = e
            | x@(Rec bes) <- bind     = snd $ head bes  -- shouldn't happen
            | otherwise               = trace "BOO!!" $ undefined :: Expr CoreBndr
#if DBG
    let !_ = trace ( "#$#-rhs  " ++ (showSDoc dflags $ ppr ecb)) $ ()
#endif
    return ecb

-------------------------------------------------------------------------------

  collectSeqinjBinds :: DynFlags -> String -> [CoreBind] -> ([CoreBind],[CoreBind]) -> ([CoreBind],[CoreBind])
  collectSeqinjBinds dflags nam [] acc@(seqinjbs, nonseqinjbs)
-- XXX not sure why I'm not reversing here; compensating for elsewhere??...
   =
#if DBG
     trace "&&-[]-&&" $
#endif
     (seqinjbs, nonseqinjbs)
-- = (reverse seqinjbs, reverse nonseqinjbs)
  collectSeqinjBinds dflags nam (h:t) acc@(seqinjbs, nonseqinjbs)
    | x@(NonRec n e) <- h
       =
#if DBG
         trace (" &&-NonRec-&& " ++ (showSDoc dflags $ ppr n)) $
#endif
         if (takeWhile (/='_') (showSDoc dflags $ ppr n)) == nam
         then recurs True
         else recurs False
    | x@(Rec nes) <- h
       =
         let (n,e) = head nes in
#if DBG
         trace (" &&-Rec-&& " ++ (showSDoc dflags $ ppr n)) $
#endif
         if (takeWhile (/='_') (showSDoc dflags $ ppr n)) == nam
         then recurs True
         else recurs False
    | otherwise            = error $ "collectSeqinjBinds: unexpected!"
    where
     recurs False = collectSeqinjBinds dflags nam t (seqinjbs, h:nonseqinjbs)
     recurs True = collectSeqinjBinds dflags nam t (h:seqinjbs, nonseqinjbs)

-------------------------------------------------------------------------------

  -- We already know it's all NonRec's.
  -- Okay, this is almost right, but it's going to need types.
  -- No use for keys "seqinj_7" etc.! Need keys to be whatever
  -- exprType returns, and we can find out exactly by using
  -- it on those seqinj's RHS's (complicated by the fact that
  -- it's <type> -> <type>, and we only really want <type>).
  makeMapSeqinjBinds :: DynFlags -> [CoreBind] -> Map String Int
  makeMapSeqinjBinds dflags lst
-- = error (intercalate "\n" $ map show $ Map.toList themap2)
   =
#if DBG_MAP_CREATION
     trace ("length lst=" ++ show (length lst) ++ "\n" ++ (intercalate "\n" $ map show $ Map.toList themap2)) $
#endif
     themap2
   where
    themap0 = Map.fromList $ go 0 lst
    themap2 = themap0
    go i [] = []
    go i ((Rec bes):t) = trace ("NonRec: "++sn) $ (se', i) : go (1+i) t
--  go i ((Rec bes):t) = (se', i) : go (1+i) t
     where
-- We only need the name of the bind, and the type of the RHS. Since every
-- binding in the group must have the same type, we only look at the first.
      (n,e) = head bes
      !_ = trace se' $ ()
      e_ = gExpandTypeSynonyms e
      se_ = showSDoc dflags $ ppr e_
      se = sanitiseTypeString se_
      marr = (se =~ "Seqaid.Runtime.seqaidDispatch *@ ") :: MatchArray
      (a,b) = (marr!0)
      se'' = drop (a+b) se
      se''' = reverse $ dropWhile (==' ') $ (\(h:t) -> if h == '.' then dropWhile (/=' ') t else (h:t)) $ reverse $ takeWhile (\c->c/='$'&&c/='\n') se''
      se' = if head se''' == '(' then se''' else "("++se'''++")"
      sn = showSDoc dflags $ ppr n
--  go i ((Rec _):t) = error "makeMapSeqinjBinds: Rec's not yet handled!!"
    go i ((h@(NonRec n e)):t) = (se', i) : go (1+i) t
     where
#if 0 && DBG
      sh = showSDoc dflags $ ppr h
      !_ = trace ("\n%%%%%%%%%%%%%%% sh  %\n"++sh) $ ()
      !_ = trace ("\n%%%%%%%%%%%%%%% sn  %\n"++sn) $ ()
      !_ = trace ("\n%%%%%%%%%%%%%%% se  %\n"++se) $ ()
      !_ = trace ("\n%%%%%%%%%%%%%%% se' %\n"++se') $ ()
---   !_ = trace se' $ ()
----  !_ = trace (showSDoc dflags $ ppr e) $ error "" :: ()
#endif
      e_ = gExpandTypeSynonyms e
      se_ = showSDoc dflags $ ppr e_
      se = sanitiseTypeString se_
#if TRY_NO_SEQAIDDISPATCH_INTERMEDIARY
-- XXX Not tested in a long while.
#if SEQABLE_ONLY
      marr = (se =~ "Control.DeepSeq.Bounded.Seqable.force_ *@ ") :: MatchArray
#else
#if TRY_SIMPLY_NFDATA
      marr = (se =~ "Control.DeepSeq.force *@ ") :: MatchArray
#else
#if NFDATAN_ONLY
      marr = (se =~ "Control.DeepSeq.Bounded.NFDataN.forcen *@ ") :: MatchArray
#else
      marr = (se =~ "Control.DeepSeq.Bounded.NFDataP.forcep *@ ") :: MatchArray
--    marr = (se =~ "Control.DeepSeq.Bounded.NFDataN.forcen *@ ") :: MatchArray
#endif
#endif
#endif
#else
      marr = (se =~ "Seqaid.Runtime.seqaidDispatch *@ ") :: MatchArray
--    marr = (se =~ "Seqaid.Runtime.seqaidDispatch\n  *@ ") :: MatchArray
#endif
      (a,b) = (marr!0)
      se'' = drop (a+b) se
                                            -- this needed for imported types
      se''' = reverse $ dropWhile (==' ') $ (\(h:t) -> if h == '.' then dropWhile (/=' ') t else (h:t)) $ reverse $ takeWhile (\c->c/='$'&&c/='\n') se''
      se' = if head se''' == '(' then se''' else "("++se'''++")"
      sn = showSDoc dflags $ ppr n

-------------------------------------------------------------------------------

  cleanupMap :: [(String,String)] -> Map String Int -> Map String Int
  cleanupMap [] m = m
  cleanupMap ((x,r):t) m = cleanupMap t m'
   where
    mmv = Map.lookup x m
    m' | isNothing mmv  = m
       | otherwise      = m'''
     where
      Just lav = mmv
      m'' = Map.delete x m
      m''' = Map.insert r lav m''

-------------------------------------------------------------------------------

  seqinjectFuncPlus :: Map String Int -> [CoreBind] -> ModGuts -> [String] -> CoreBindMeta -> CoreM CoreBind
  seqinjectFuncPlus seqinj_map seqinj_binds guts types (Excl siteid b) = return b
  seqinjectFuncPlus seqinj_map seqinj_binds guts types cbm@(Incl siteid dododo@(do_wrap,do_man,do_typ) x) = do
    dflags <- getDynFlags
#if DBG_SEQINJECT_FUNC
    putMsgS "seqinjectFuncPlus:"
    prBind dflags x
#endif
#if INFER_TOP_LEVEL_TYPES
    cb' <- if do_wrap
           then seqinjectFunc seqinj_map seqinj_binds guts cbm
           else return x
#else
    let cb' = x
#endif
    cb <- if do_man || do_typ
          then setManualSiteIDsAndDoTypesBasedSubexpressionInjections dflags dododo seqinj_map seqinj_binds guts types cb'
          else return cb'
    return cb

-------------------------------------------------------------------------------

  seqinjectFunc :: Map String Int -> [CoreBind] -> ModGuts -> CoreBindMeta -> CoreM CoreBind
  seqinjectFunc seqinj_map seqinj_binds guts (Excl siteid b) = return b
  seqinjectFunc seqinj_map seqinj_binds guts (Incl siteid dododo@(do_wrap,do_man,do_typ) (x@(NonRec b e))) = do 
    dflags <- getDynFlags
    let nb = nameOfBind dflags x
#if DBG_SEQINJECT_FUNC
    putMsgS $ "!!seqinjectFunc!! " ++ nb
#endif
    tstr <- sanitiseTypeStringExpr dflags e
#if 0 || DBG
    let !_ = putMsgS $ "tstr = " ++ tstr
#endif
    let banned_list = []  -- (vestigial)
    if or $ map (flip isPrefixOf tstr) banned_list
    then do
#if DBG_MAP_LOOKUP
      let !_ = trace "-->>banned<<--" $ ()
#endif
      return x
    else do
      let midx =
#if DBG_MAP_LOOKUP
                 trace (" ++++++>> " ++ tstr ++ " << " ++ (showSDoc dflags $ ppr (exprType e)) ++ " >> ") $
#endif
                 if head tstr == '('
                 then Map.lookup (     tstr     ) seqinj_map
                 else Map.lookup ("("++tstr++")") seqinj_map
      if isNothing midx
      then do
#if ! NO_WARN_SITE_MISSING_INSTANCE
--- #if ! SILENT
#if SEQABLE_ONLY
        putMsgS $ "seqaid: warning: couldn't find SOP Generic instance for type\n  " ++ tstr
#else
#if NFDATAN_ONLY
        putMsgS $ "seqaid: warning: couldn't find NFDataN instance for type\n  " ++ tstr
#else
        putMsgS $ "seqaid: warning: couldn't find NFDataP instance for type\n  " ++ tstr
#endif
#endif
#endif
        return x
      else do
        let idx = fromJust midx
        the_force_var <- getBindRHS guts dflags $ seqinj_binds!!idx
#if 0 || DBG
        let !_ = trace ("\n@@@@@@@@@@@@@@@@\n"++(showSDoc dflags $ ppr x)) $ ()
        let !_ = trace ("\n!!!!!!!!!!!!!!!!\n"++(showSDoc dflags $ ppr the_force_var)) $ ()
        let !_ = trace ("\n????????????????\n"++(showSDoc dflags $ ppr e )++"\n") $ ()
#endif
#if DRY_RUN
        let e' = e
#else
        -- XXX hardcode (however, pad never truncates)
        let bs = pad 10 $ showSDoc dflags $ ppr b
--      let bs = showSDoc dflags $ ppr b
        bse <- mkStringExpr bs
-- We need to inject to the right of any initial lambda bindings;
-- joining us for this occasion:
--   collectBinders :: Expr b -> ([b], Expr b)
--   mkCoreLams :: [CoreBndr] -> CoreExpr -> CoreExpr
        let (ebs,ee) = collectBinders e
        let e'' = mkCoreApp
                   ( mkCoreApp
                       the_force_var
                       ( let idx = fst3 siteid in
                         ( mkCoreTup
                            [ mkIntExprInt dflags idx
                            , bse
#if DEMO_MODE
                            , mkIntExprInt dflags 0
#else
                            , mkIntExprInt dflags $ hash $ bs ++ show idx
#endif
                            ]
                         )
                       )
                   )
                   ee
        let e' = mkCoreLams ebs e''
#endif
        return $ NonRec b e'
  seqinjectFunc seqinj_map seqinj_binds guts (Incl siteid dododo (x@(Rec bes))) = do 
    bes' <- mapM (\ (b,e) -> do { (NonRec b' e') <- seqinjectFunc seqinj_map seqinj_binds guts (Incl siteid dododo (NonRec b e)) ; return (b',e') } ) bes
    return $ Rec bes'

-------------------------------------------------------------------------------

  -- A bit later: Now I'm going to try adding the types-based site
  -- selection to this (seqiad.config). But I'm going to clone the
  -- existing SYB traversal and run a second traversal instead?...
  --   Er, no.
  -- Keep them together, and then can avoid doing double (which would
  -- be a user error anyhow -- if they manually injected, as well as
  -- listed the type in their seqaid.config, they're asking for double,
  -- but may as well code it right and prevent redundant wrapping even
  -- in such cases...
  ------
  -- Note that, at present, this function is only called for binds
  -- which have a {-# ANN module (SeqaidAnnManual "funcname") #-}
  -- present in the same file (typically, but not necessarily,
  -- at the declaration head).  Would prefer to auto-detect the
  -- seqaid applications, but don't know how yet (TH staging woes).
  setManualSiteIDsAndDoTypesBasedSubexpressionInjections :: DynFlags -> (Bool,Bool,Bool) -> Map String Int -> [CoreBind] -> ModGuts -> [String] -> CoreBind -> CoreM CoreBind
  setManualSiteIDsAndDoTypesBasedSubexpressionInjections dflags dododo@(do_wrap,do_man,do_typ) seqinj_map seqinj_binds guts types cb = do
    let nb = nameOfBind dflags cb
#if DBG_SEQINJECT_FUNC
    prBind dflags cb
    putMsgS $ nb
#endif
    let nb'= dropQuals nb 
    if do_man
    then putMsgS $ "Manual seqaid instrumentation found: " ++ nb'
    else putMsgS $ "Harnessing bind: " ++ nb'
    nbe <- mkStringExpr nb
    (cb',!_) <- flip runStateT (1::Int) . everywhereM (mkM (mfg do_man seqinj_map seqinj_binds guts types nb nbe)) $ cb
--  let (cb',!_) = flip runStateT (1::Int) . everywhereM (mkM (mfg do_man seqinj_map seqinj_binds guts types nb nbe)) $ cb
--  let (cb',!_) = flip runState (1::Int) . everywhereM (mkM (mfg do_man seqinj_map seqinj_binds guts types nb nbe)) $ cb
    return cb'

   where

    {-  NOINLINE mfg #-}
    -- XXX It would be nice to nudge the SYB traversal along, past
    -- this application of seqaid*, since otherwise there will be
    -- testing some descendants in vain ... but never mind for now;
    -- that's an optimisation...
    mfg ::
              Bool                       -- process for Manual injection
           -> Map String Int             -- seqinj_map ("the Map")
           -> [CoreBind]                 -- seqinj_binds (seqinj_[0-9]*)
           -> ModGuts                    -- guts ("the guts")
           -> [String]                   -- types (from seqaid.config)
           -> String                     -- nb (name of bind being processed)
           -> CoreExpr                   -- nbe (mkStringExpr nb)
           -> CoreExpr                   -- node in Core AST (SYB traversal)
           -> StateT Int CoreM CoreExpr  -- result (refactored Core AST)
--  mfg seqinj_map seqinj_binds guts types nb nbe app@App{} = do
--  mfg :: Map String Int -> [CoreBind] -> ModGuts -> [String] -> String -> CoreExpr -> CoreExpr -> State Int CoreExpr

-- XXX decided to just make Manual and types-based mutually-exclusive
-- to start with here... When get to allow both at once again,
-- definitely a single traversal is appropriate. Just for the SiteID
-- indexing if nothing else!...
--- #if ! SEQAIDPP_TYPES

-- XXX note that now, with the types-based coming in, we're not
-- only wrapping App nodes; so the catch-all case below this
-- needs to do wrapping too.  But we do need an App-node case
-- because want to avoid redundant wrapping.
    mfg doman@True seqinj_map seqinj_binds guts types nb nbe app@App{} = do
     let (!fun@(Var q1),q2s) = collectArgs app
     let q1str = showSDoc dflags $ ppr q1
     let len = length q2s
#if DBG_MANUAL
     !_ <- trace ("q1str="++q1str++" |q2s|="++show len) $ return ()
--   if q1 /= nam then error "UNEXPECTED!" else return ()
     !_ <- if q1str == "Seqaid.Runtime.seqaidDispatch" then trace ("len="++show len) $ return () else return ()
#endif
     if q1str == "Seqaid.Runtime.seqaidDispatch"
     then do
#if DBG_MANUAL
       !_ <- trace (showSDoc dflags $ ppr app) $ return ()
#endif
       return ()
     else return ()
     if len /= 7  -- XXX won't catch the "seqaid $" wrapper pattern, though!
     then return app
     else do
      if q1str == "Seqaid.Runtime.seqaidDispatch"
      then do
       i <- get
#if 0 && DEMO_MODE
-- This caused Prelude.undefined errors at runtime last I checked.
       -- XXX note that, with this, the State "put" below is conditional,
       -- which is a recipe for a space leak... ("ironically")
       -- XXX This is totally fragile; it's very much a hack demo mode thing!
       if i /= 6 then return app
       else do
#else
       do
#endif
        put (1+i)
--      let tupcon = tupleCon UnboxedTuple 3
        let tupcon = tupleCon BoxedTuple 3
#if DBG_MANUAL
        !_ <- trace "=1==============================" $ return ()
        !_ <- trace (showSDoc dflags $ ppr app) $ return ()
        !_ <- trace "=2==============================" $ return ()
        !_ <- trace (showSDoc dflags $ ppr q2s) $ return ()
        !_ <- trace "=3==============================" $ return ()
#endif
#if DEMO_MODE
        let h = 0
#else
        let h = hash $ nb ++ show i
#endif
#if DBG_MANUAL
        !_ <- trace (show i++" "++show nb++" "++show h) $ return ()
#endif
--      let tuptycon = mkTupleTy UnboxedTuple (map (Type . exprType) blahs)
        let blahs = [ mkIntExprInt dflags i
                    , nbe
                    , mkIntExprInt dflags h ]
        let arg = mkConApp tupcon $ (map (Type . exprType) blahs) ++ blahs
#if DBG_MANUAL
        !_ <- trace (showSDoc dflags $ ppr arg) $ return ()
        !_ <- trace "=4==============================" $ return ()
#endif
        let fun' = fun
        let app' = mkCoreApps fun' $ (take (-2+len) q2s) ++ [arg] ++ [last q2s]
#if DBG_MANUAL
        !_ <- trace ("app before:\n"++(showSDoc dflags $ ppr app)) $ return ()
        !_ <- trace ("app after:\n"++(showSDoc dflags $ ppr app')) $ return ()
---     !_ <- trace (showSDoc dflags $ ppr app') $ return ()
        !_ <- trace "=5==============================" $ return ()
#endif
#if 0
        (\x -> (return $! x) >>= return) app'
        return app
#else
        return app'
#endif
      else do
#if DBG_MANUAL
        !_ <- trace "= *** 1 ==============================" $ return ()
        !_ <- trace (showSDoc dflags $ ppr app) $ return ()
        !_ <- trace "= *** 2 ==============================" $ return ()
#endif
        return app
    mfg doman@True _ _ _ _ _ _ x = return x

--- #else

    -- x is any CoreExpr except for an App node (which is covered above)
    -- Later: No it isn't covered above! I vaguely recall what I meant
    -- by that at the time, but I don't see it now...
--  mfg doman@False seqinj_map seqinj_binds guts types nb nbe x = do
    mfg doman@False seqinj_map seqinj_binds guts types nb nbe x@App{} = do
--isSaturatedApp :: DynFlags -> Type -> Bool
      let tx = exprType x
      if not $ isSaturatedApp tx
      then do
        return x
      else do
#if 0
        txstr <- showSDoc dflags $ ppr tx
#else
        txstr <- lift $ sanitiseTypeStringExpr dflags x
#endif
#if DBG_SEQINJECT_FUNC
        !_ <- trace (">> App >>>> " ++ txstr ++ " /// " ++ intercalate " " types) $ return ()
#endif
        if not $ txstr `elem` types
        then return x
        else do
         -- XXX User should refrain from ()-ing in seqaid.config; this
         -- is not enough anyway, as they could even go types: (((Int)))...
         let midx =
              if head txstr == '('
              then Map.lookup (     txstr     ) seqinj_map
              else Map.lookup ("("++txstr++")") seqinj_map
         if isNothing midx
         then do
#if ! NO_WARN_SITE_MISSING_INSTANCE
--- #if ! SILENT
#if SEQABLE_ONLY
           !_ <- trace ("seqaid: warning: couldn't find SOP Generic instance for type\n  " ++ txstr) $ return ()
#else
#if NFDATAN_ONLY
           !_ <- trace ("seqaid: warning: couldn't find NFDataN instance for type\n  " ++ txstr) $ return ()
#else
           !_ <- trace ("seqaid: warning: couldn't find NFDataP instance for type\n  " ++ txstr) $ return ()
#endif
#endif
#endif
           return x
         else do
#if DBG_SEQINJECT_FUNC
           !_ <- trace ("<< App <<<< " ++ txstr) $ return ()
#endif
           let idx = fromJust midx
           the_force_var <- lift $ getBindRHS guts dflags $ seqinj_binds!!idx
           i <- get
           put (1+i)
--         let tupcon = tupleCon UnboxedTuple 3
           let tupcon = tupleCon BoxedTuple 3
#if DEMO_MODE
           let h = 0
#else
           let h = hash $ nb ++ show i
#endif
           let blahs = [ mkIntExprInt dflags i
                       , nbe
                       , mkIntExprInt dflags h ]
           let sidarg = mkCoreTup blahs
--         let sidarg = mkConApp tupcon $ (map (Type . exprType) blahs) ++ blahs
           let x' = mkCoreApp
                      ( mkCoreApp
                          the_force_var
                          sidarg
                      )
                      x
           return x'

    mfg doman@False seqinj_map seqinj_binds guts types nb nbe x@Var{} = do
     if exprIsVarWithFunctionType x
     then return x
     else do
       txstr <- lift $ sanitiseTypeStringExpr dflags x
#if DBG_SEQINJECT_FUNC
       !_ <- trace (">> Var >>>> " ++ txstr ++ " /// " ++ intercalate " " types) $ return ()
#endif
       if not $ txstr `elem` types
       then return x
       else do
        
        -- XXX User should refrain from ()-ing in seqaid.config; this
        -- is not enough anyway, as they could even go types: (((Int)))...
        let midx =
             if head txstr == '('
             then Map.lookup (     txstr     ) seqinj_map
             else Map.lookup ("("++txstr++")") seqinj_map
        if isNothing midx
        then do
#if ! NO_WARN_SITE_MISSING_INSTANCE
--- #if ! SILENT
#if SEQABLE_ONLY
          !_ <- trace ("seqaid: warning: couldn't find SOP Generic instance for type\n  " ++ txstr) $ return ()
#else
#if NFDATAN_ONLY
          !_ <- trace ("seqaid: warning: couldn't find NFDataN instance for type\n  " ++ txstr) $ return ()
#else
          !_ <- trace ("seqaid: warning: couldn't find NFDataP instance for type\n  " ++ txstr) $ return ()
#endif
#endif
#endif
          return x
        else do
#if DBG_SEQINJECT_FUNC
          !_ <- trace ("<< Var <<<< " ++ txstr) $ return ()
#endif
          let idx = fromJust midx
          the_force_var <- lift $ getBindRHS guts dflags $ seqinj_binds!!idx
          i <- get
          put (1+i)
--        let tupcon = tupleCon UnboxedTuple 3
          let tupcon = tupleCon BoxedTuple 3
#if DEMO_MODE
          let h = 0
#else
          let h = hash $ nb ++ show i
#endif
          let blahs = [ mkIntExprInt dflags i
                      , nbe
                      , mkIntExprInt dflags h ]
          let sidarg = mkCoreTup blahs
--        let sidarg = mkConApp tupcon $ (map (Type . exprType) blahs) ++ blahs
          let x' = mkCoreApp
                     ( mkCoreApp
                         the_force_var
                         sidarg
                     )
                     x
          return x'

    mfg doman@False seqinj_map seqinj_binds guts types nb nbe x = return x

--- #endif

-------------------------------------------------------------------------------

  dropQuals :: String -> String
#if 0
  dropQuals = id
#else
  -- XXX WARNING: This is causing errors for some types!
  -- For instance, a type which contains a type-subexpression
  -- which itself contains a qualified type name.
  dropQuals = reverse . takeWhile (/= '.') . reverse
#endif

-------------------------------------------------------------------------------

  sanitiseTypeStringExpr :: DynFlags -> CoreExpr -> CoreM String
  sanitiseTypeStringExpr dflags e = do
    let te__ = exprType e
              -- XXX is there not a GHC API function I saw for this?...
    let te_ = followArrows te__  -- use the result type
    let te = GHC.expandTypeSynonyms te_
    let tstr = showSDoc dflags $ pprType te
#if DBG
    let !_ = trace ("--- " ++ tstr) $ ()
#endif
    let santstr = removeForallPartHack tstr
--  let santstr = tstr
--  let santstr = sanitiseTypeString_old tstr
#if DBG
    let !_ = trace ("+++ " ++ santstr) $ ()
#endif
    return santstr

-------------------------------------------------------------------------------

  -- XXX VERY BAD INDEED!!!
  removeForallPartHack :: String -> String
  removeForallPartHack s = sanitiseTypeString s_
   where
#if 1
    marr1 = (s =~ "^forall ") :: MatchArray
    (a1,b1) = (marr1!0)
    marr2 = (s =~ "=> ") :: MatchArray
    (a2,b2) = (marr2!0)
    s_ | null $ indices marr1  = s
       | null $ indices marr2  = s
       | otherwise
          =   dropWhile (\x -> x==' '||x=='\t'||x=='\n')
            $ drop (a2+b2) s
#else
    s_ | let s' = "forall " in s' == take (length s') s
          =   dropWhile (\x -> x==' '||x=='\t'||x=='\n')
            $ drop 2
            $ dropWhile (/='=') s
       | otherwise
          = s
#endif

-------------------------------------------------------------------------------

  sanitiseTypeString :: String -> String
  sanitiseTypeString s = s''
   where
    s' = map (\c -> if c=='\n' then ' ' else c) s
    s'' = f s'
    f [] = []
    f (' ':' ':cs) = f (' ':cs)
    f (c:cs) = c : f cs

-------------------------------------------------------------------------------

  putmess :: String -> CoreM ()
  putmess s = do
    let s' = "== " ++ s ++ " "
    let l = length s'
    let n = max (79 - l) 0
    putMsgS $ s' ++ take n (repeat '=')

  putendmess :: CoreM ()
  putendmess = do
    putMsgS $ take 79 (repeat '=')

-------------------------------------------------------------------------------

  -- Not the most efficient but whatever.
  -- Also, I don't like writing monadic code, so the caller computes
  -- the String names of the binds, and passes them.
  separateDummyInstanceDecls :: String -> [String] -> [CoreBind] -> ([CoreBind],[CoreBind],[CoreBind]) -> ([CoreBind],[CoreBind],[CoreBind])
  separateDummyInstanceDecls _ [] [] (acc_bs,acc_dbs,acc_mbs)
   = (reverse acc_bs,reverse acc_dbs,reverse acc_mbs)
  separateDummyInstanceDecls modname (n:ns) (b:bs) (acc_bs,acc_dbs,acc_mbs)
   = if let x = (elision_targets!!0) in x == take (length x) n
     then separateDummyInstanceDecls modname ns bs (acc_bs,acc_dbs,b:acc_mbs)
     else
      if let x = (elision_targets!!1) in x == take (length x) n
      then separateDummyInstanceDecls modname ns bs (acc_bs,b:acc_dbs,acc_mbs)
      else separateDummyInstanceDecls modname ns bs (b:acc_bs,acc_dbs,acc_mbs)
   where
    modname' = map (\x -> if x == '.' then '_' else x) modname
    elision_targets = [ "$cseqinj_meth_" , modname++".$fSeqinjDummyClass_"++modname'++"()" ]
--  elision_targets = [ "$cseqinj_meth_" , modname++".$fSeqinjDummyClass()" ]
  separateDummyInstanceDecls _ _ _ _ = error "39489387"

-------------------------------------------------------------------------------

  nameOfBind :: DynFlags -> CoreBind -> String
  nameOfBind dflags bndr@(NonRec b _) = showSDoc dflags (ppr b)
  nameOfBind dflags bndr@(Rec ((b,_):_)) = showSDoc dflags (ppr b)
  nameOfBind dflags _ = error "nameOfBind: unexpected"

-------------------------------------------------------------------------------

  typeOfBind :: CoreBind -> Type
  typeOfBind bndr@(NonRec _ e) = exprType e
  typeOfBind bndr@(Rec ((_,e):_)) = exprType e
  typeOfBind _ = error "typeOfBind: unexpected"

-------------------------------------------------------------------------------

  pad :: Int -> String -> String
  pad n s = s ++ (take (n-(length s)) $ repeat ' ')

-------------------------------------------------------------------------------

  fst3 :: (a,b,c) -> a
  fst3 (x,_,_) = x
  snd3 :: (a,b,c) -> b
  snd3 (_,y,_) = y
  thd3 :: (a,b,c) -> c
  thd3 (_,_,z) = z

-------------------------------------------------------------------------------

  -- These two used to be in different spots in this file.
  -- I'm not sure they're both needed/wanted?...

  isSaturatedApp :: Type -> Bool
  isSaturatedApp ty = b
   where
    b = if isFunTy ty
        then {- trace ("isSaturatedApp: FALSE ") $ -} False
        else {- trace ("isSaturatedApp: TRUE " ) $ -} True

  exprIsVarWithFunctionType :: CoreExpr -> Bool
  exprIsVarWithFunctionType e = b
   where
    t = exprType e
    b = case t of
         GHC.FunTy arg res -> True
         _ -> False

-------------------------------------------------------------------------------

  followArrows :: Type -> Type
  followArrows (GHC.FunTy arg res) = followArrows res
  followArrows t = t

-------------------------------------------------------------------------------

  gExpandTypeSynonyms :: CoreExpr -> CoreExpr
  gExpandTypeSynonyms e = e'
   where
    e' = everywhere (mkT fg) e
    fg :: Type -> Type
    fg x = GHC.expandTypeSynonyms x

-------------------------------------------------------------------------------

-- XXX The SYB traversal need not be monadic.
-- Possibly, the CoreM monad is not even needed...

  collectSubexpressionTypes :: ModGuts -> CoreM [Type]
  collectSubexpressionTypes guts = do
    let binds = mg_binds guts
    tyss <- mapM collectSubexpressionTypesBind binds
    let tys = concat tyss
    return tys

  collectSubexpressionTypesBind :: CoreBind -> CoreM [Type]

  collectSubexpressionTypesBind (NonRec _ e) = do
    (_,tys) <- flip runStateT ([]::[Type]) . everywhereM (mkM mfg) $ e

    let tys' = map GHC.expandTypeSynonyms tys
    return tys'
--  return tys
--  return $ nub tys  -- no Eq instance!
   where
    mfg :: CoreExpr -> StateT [Type] CoreM CoreExpr
    mfg e = do
     case e of
      Type _ -> return e
      _ -> do
              tys <- get
              let et = exprType e
              put (et:tys)
              return e

  collectSubexpressionTypesBind (Rec bes) = do 
    tyss <- mapM (\ (b,e) -> collectSubexpressionTypesBind (NonRec b e)) bes
    let tys = concat tyss
    return tys

-------------------------------------------------------------------------------

  generateOmniImports :: DynFlags -> [Type] -> [String]
  generateOmniImports dflags lst = imps'
   where
    imps = generateOmniImports' dflags lst
    -- This is ridiculously costly, but at this point that's
    -- not a concern.
    imps' = nub imps  -- have to, even though the lst was pre-nubbed,
                      -- because we're extracting subtypes in here

  -- XXX In here we have some very brutal hacks, just to get
  -- proof-of-concept happening.  Do something better!...
  generateOmniImports' :: DynFlags -> [Type] -> [String]
  generateOmniImports' dflags [] = []
  generateOmniImports' dflags (t:ts)
   = all_imps ++ generateOmniImports' dflags ts
   where
    tstr = showSDoc dflags $ ppr t
    all_typenames =   getAllTextMatches
                    $ tstr =~ "[A-Z][.A-Za-z0-9_']*" :: [String]
    all_typenames' = filter (/= "GHC.Prim.Void") all_typenames
    all_typenames'' = filter (/= "GHC.Prim.Addr") all_typenames'
#if 1
    all_typenames''' = filter (/= "Control.DeepSeq.Bounded.Pattern.Pattern") all_typenames''
    all_typenames'''' = filter (/= "Seqaid.Global.SiteID") all_typenames'''
#if 1
    all_splits' = catMaybes $ map splitFQN all_typenames''''
#else
    -- (we don't handle tuples with more than 7 components at this time)
    all_typenames''''' = filter (/= "(Int, Int, Int, Blob Int, [Int], Int, Int, Int, Int)") all_typenames''''
    all_splits' = catMaybes $ map splitFQN all_typenames'''''
#endif
#else
    all_splits' = catMaybes $ map splitFQN all_typenames''
#endif
    all_splits_ = map (\ (x,y) -> if x == "GHC.Prim" then ("GHC.Exts",y) else (x,y)) all_splits'
#if 1
    all_splits = all_splits_
#else
    all_splits = map (\ (x,y) -> if x == "GHC.Integer.Type" then ("GHC.Integer",y) else (x,y)) all_splits_
#endif
    all_imps = map generateOmniImport all_splits

  generateOmniImport :: (String,String) -> String
  generateOmniImport (m,mt) = ( "import " ++ m ++ " ( " ++ mt ++ " )" )

  splitFQN :: String -> Maybe (String,String)
  splitFQN s
   | '.' `elem` s  = Just ( reverse $ drop 1 $ dropWhile (/='.') rs
                          , reverse $ takeWhile (/='.') rs
                          )
   | otherwise     = Nothing
   where rs = reverse s

-------------------------------------------------------------------------------

  get_seqaid_instance_strings :: ModGuts -> CoreM [String]
  get_seqaid_instance_strings guts = do
    anns <- getAnnotations deserializeWithData guts
            :: CoreM (UniqFM [SeqaidAnnAvailableInstances])  -- (signature required)
    let annseltss = eltsUFM anns :: [[SeqaidAnnAvailableInstances]]
    let annselts = concat annseltss :: [SeqaidAnnAvailableInstances]
#if TH_TYPE_IN_TYPES_ANN
    let ts = concat $ map (\ (SeqaidAnnAvailableInstances tslst) -> tslst ) annselts :: [Type]
    let tstrs' = map (showSDoc dflags . ppr) ts :: [String]
#else
    let tstrs' = concat $ map (\ (SeqaidAnnAvailableInstances tslst) -> tslst ) annselts :: [String]
#endif
    let tstrs'' = map (map (\x -> if x == '\n' then ' ' else x)) tstrs'
    let compactWhite [] = []
        compactWhite (' ':' ':t) = compactWhite (' ':t)
        compactWhite (h:t) = h:compactWhite t
    let tstrs = map ( id
                    . sanitiseAgain
                    . remove_class_str
                    . compactWhite
                    ) tstrs''
    return tstrs

  remove_class_str :: String -> String
  remove_class_str str
   | ss == take lenss str  = dropWhile (==' ') $ drop lenss str
   | otherwise             = str
   where
#if SEQABLE_ONLY
    ss = "Generics.SOP.Universe.Generic"
#else
#if NFDATAN_ONLY
    ss = "Control.DeepSeq.Bounded.NFDataN.NFDataN"
#else
    ss = "Control.DeepSeq.Bounded.NFDataP.NFDataP"
#endif
#endif
    lenss = length ss

  -- Some types are wrapped in (...).
  -- Others are not.
  -- And even some (but not all!) tuples are double-wrapped [eg. ((a,b))].
  -- Not sure what can do about the latter.
  -- But the former, as done before, can just assure wrap if not wrapped...
  sanitiseAgain :: String -> String
  sanitiseAgain str
   | ( not $ null str ) && '(' /= head str  = "(" ++ str ++ ")"
   | otherwise                              =        str
-- I just can't stand this!
--- | ( not $ null str ) && '(' /= head str  = '(' : ( str ++ ")" )

-------------------------------------------------------------------------------

  -- This isn't the best name; but it is obviously a predicate, so it's okay.
  -- Also, this is not really internally (logically) consistent,
  -- with respect to parentheses. (But I'm grasping here...)
  instancesAvailable :: [String] -> String -> Bool
  instancesAvailable iss ts
   | elem ts iss  = True
   -- ... and yes, this bit us (kinda expected; welcome
   -- to see it actually): 9-tuple in leaky.hs!...
   -- So we will count commas for now, as this DOES at least
   -- form a safe bound; there can only be MORE commas due
   -- to any nesting we're not accounting for!
   | "(" == take 1 ts  = if length (filter (==',') ts) > 7 then False else True
   | elem ("("++ts++")") iss  = True
   -- XXX this should at least go as far as to recursively call
   -- instancesAvailable on the argument(s) to type constructors.
   | mcnm <- parse ts  = if isNothing mcnm then False
                         else let cnm = fromJust mcnm in
                         cnm `elem` instance_cnms
   | otherwise    = False
   where
    parse :: String -> Maybe String
    parse ('(':c:cs) = if isUpper c then Just $ parse' (c:cs)
                       else Nothing
    parse s = Just $ parse' s
    parse' s = takeWhile (/=' ') s
    instance_cnms = catMaybes $ map parse iss

-------------------------------------------------------------------------------

  -- O(n*logn + n), versus (nub . sort) or (sort . nub), both of
  -- which are O(n^2) because nub is.  Just maybe, nub has lower
  -- average case complexity when fed a sorted list? Here is
  -- the GHC source for nub.
  ---------------------------------------
  --  -- stolen from HBC
  --  nub l                   = nub' l []
  --    where
  --      nub' [] _           = []
  --      nub' (x:xs) ls
  --          | x `elem` ls   = nub' xs ls
  --          | otherwise     = x : nub' xs (x:ls)
  ---------------------------------------
  --  elem _ []       = False
  --  elem x (y:ys)   = x==y || elem x ys
  ---------------------------------------
  -- So nub will behave pathologically badly for a list with
  -- no two equal elements, due to worst-case linear complexity
  -- of elem.

  nubsort :: Ord a => [a] -> [a]
  nubsort = map head . group . sort  -- O(nlogn)
--nubsort = nub . sort               -- O(n^2)!

-------------------------------------------------------------------------------

  assureFQN :: String -> String -> String
  assureFQN thismodname name
   | '.' `elem` name  = name
   | otherwise        = thismodname ++ ('.':name)

-------------------------------------------------------------------------------

