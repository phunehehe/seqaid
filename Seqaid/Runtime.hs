
-------------------------------------------------------------------------------

  {-  OPTIONS_GHC -O2 #-}

  {-# LANGUAGE CPP #-}

  {-# LANGUAGE BangPatterns #-}   -- for debugging only (maybe for more...)

  {-# LANGUAGE ScopedTypeVariables #-}

-------------------------------------------------------------------------------

-- |
-- Module      :  Seqaid.Runtime
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC (unsafePerformIO)
--
-- This module is for seqaid internal use.
--
-- /The seemingly redundant superclass constraints are necessary/
-- /due to some weirdness in the TH or Core code (I forget the/
-- /details); should try to get rid of them in case it's/
-- /since become possible.../

  module Seqaid.Runtime (

      SiteID  -- re-export

    , seqaidDispatch

    , seqaidDispatchDyn

-- would be best if could... (less for plugin/lib user to do)
--  , module Control.DeepSeq.Bounded
--  , Typeable

  ) where

-------------------------------------------------------------------------------

  import Control.DeepSeq.Bounded

  import Data.Typeable ( typeOf )
  import Data.Typeable ( Typeable )

  import Seqaid.Global (
                           SiteID
---                      , run_IO_SM  -- moved to Optim [not ideal, but...]
--                       , sample_period
                         , max_depth
--                       , fixed_pat_sequence
                         , fixed_pat
                       )

  import Seqaid.Optim

  -- (for monitoring resource use, and computing objective function)
  import System.IO.Unsafe ( unsafePerformIO )

  import Debug.Trace ( trace )

#if SEQABLE_ONLY
  import Generics.SOP ( Generic )
#endif

  import Control.Exception
  import System.IO
  import System.IO.Error

-------------------------------------------------------------------------------

  {-# NOINLINE seqaidDispatch #-}

#if SEQABLE_ONLY

  seqaidDispatch :: (
#if SHOW_TYPE
--- #warning "WARNING-2"
                     Typeable a,
#endif
                     Generic a) =>
                      SiteID ->
                      a -> a
  seqaidDispatch
                 sid
                     x =
#if DBG_SEQAID
                        if {- True || -} i >= t
                        then
                         trace (">>> S  "
                                ++snd3 sid++"\t"
                                ++show stats_query_idx++"  "
                                ++show (i,t,size)++"  "
#if SHOW_TYPE
                                ++show (typeOf x)
#endif
                               ) $
#endif
                                   x'
                        else x'

#else

#if NFDATAN_ONLY
--- #warning "WARNING-1"

-- It's very tempting to write this with the CPP subconditionals deeper in...
  seqaidDispatch :: (
#if SHOW_TYPE
--- #warning "WARNING-2"
                     Typeable a,
#endif
                     NFDataN a) =>
                      SiteID ->
                      a -> a
  seqaidDispatch
                 sid
                     x =
#if DBG_SEQAID
                        if {- True || -} i >= t
                        then
                         trace (">>> N  "
--                       trace ("SEQAIDDISPATCH  N  "
                                ++snd3 sid++"\t"
                                ++show stats_query_idx++"  "
                                ++show (i,t,size)++"  "
#if SHOW_TYPE
                                ++show (typeOf x)
#endif
                               ) $
#endif
                                   x'
                        else x'

#else

  -- XXX Question: Are all the superclass constraints still needed?
  -- There was a time when they were.  (Although they're implicit
  -- for normal Haskell compilation, for whatever I was doing either
  -- in TH or in Core, they were needed to get them in scope.
  seqaidDispatch :: forall a. (NFData a,NFDataN a,Typeable a,NFDataP a) =>
                    SiteID ->
                    a -> a
  seqaidDispatch
                 sid
                     x =
#if DBG_SEQAID
#if 1
-- 80 cols
                        if i >= t
--                      if True || i >= t
                        then
                         trace ((if stats_query_idx == 0 then "                                                live      alloc\n" else "")++(if stats_query_idx <= (1+max_depth) then " N  " else " P  ")
                                ++(if stats_query_idx <= (1+max_depth) then (padr 30 (show depth)) else padr 30 (showPat pat'))++"  "
--                              ++padl 2 (show (fst3 sid))++"  "
                                ++padr 8 (dropQuals (snd3 sid))
--                              ++padl 3 (show stats_query_idx)++"  "
                                ++padl 8 (show cbu)++"  "
                                ++padl 9 (show size)++"  "
--                              ++show (cbu, size)++"  "
--                              ++show (i,   size)++"  "
--                              ++show (i,t',size)++"  "
                                ++show (typeOf x)
                               ) $
                                   x'
                        else x'
#else
-- 110 cols
                        if i >= t
--                      if True || i >= t
                        then
#if 1
                         trace ((if stats_query_idx == 0 then "                                                                               live heap        alloc\n" else "")++(if stats_query_idx <= (1+max_depth) then ">>> N  " else ">>> P  ")
#else
                         trace (">>> P  "
#endif
--                       trace ("SEQAIDDISPATCH  P  "
                                ++(if stats_query_idx <= (1+max_depth) then (padr 40 (show depth)) else padr 40 (showPat pat'))++"  "
                                ++padl 2 (show (fst3 sid))++"  "
--                              ++(if fst3 sid > 9 then "" else " ")++show (fst3 sid)++"  "
                                ++snd3 sid++"\t"
--                              ++show sid++"\t"
                                ++padl 3 (show stats_query_idx)++"  "
                                ++padl 11 (show cbu)++"  "
                                ++padl 11 (show size)++"  "
--                              ++show (cbu, size)++"  "
--                              ++show (i,   size)++"  "
--                              ++show (i,t',size)++"  "
                                ++show (typeOf x)
                               ) $
                                   x'
                        else x'
#endif
#endif

#endif
#endif

   where

    ( stats_query_idx, depth, pat, snk, i, t, size, cbu, t')
--  (!stats_query_idx,!depth,!pat,!snk,!i,!t,!size,!cbu,!t')
      = unsafePerformIO $! run_IO_SM sid x

-- Later: Now "EXCEPTION ACTION" message /is/ printed -- twice.
-- And then we see "leaky: DeepSeqBounded_PingException",
-- on which leaky seems to die.
-----
-- Doesn't appear to work -- exception still kills whole program,
-- and no "EXCEPTION ACTION" message is printed.
#if 0
#elif 1
    x' = unsafePerformIO $ handle f $ do
                              evaluate x''
                              return x''
    f = (\ (DeepSeqBounded_PingException msg)
             -> do
#if 1
                   putStr (msg::String)  -- b/c showRose ends '\n'
--                 putStrLn (msg::String)
#else
                   hPutStrLn stderr msg
                   hFlush stderr
#endif
#if 0
                   hPutStrLn stderr "EXCEPTION ACTION"
                   hFlush stderr
#endif
--                 evaluate x''
                   return x'')
#elif 0
    x' = unsafePerformIO $ evaluate x'' `catch` f
-- catchJust :: Exception e => (e -> Maybe b) -> IO a -> (b -> IO a) -> IO a  
    f = (\ () -> do
#if 0
                    hPutStrLn stderr "EXCEPTION ACTION"
                    hFlush stderr
#endif
--                  evaluate x''
                    return x'')
#elif 0
    x' = unsafePerformIO $ evaluate x'' `catchJust'` f
    catchJust' = catchJust ep
--- x' = unsafePerformIO $! evaluate x'' `catchJust'` ep f
--- catchJust' = flip catchJust
---  ep :: forall e. Exception e => e -> Maybe a
    ep = (\e -> if isDeepSeqBounded_PingException e then Just () else error "BOO!" {-Nothing-})
---  ep = (\ (e::DeepSeqBounded_PingException) -> if isDeepSeqBounded_PingException $ fromException e then Just x'' else Nothing)
---  f :: DeepSeqBounded_PingException -> IO a
    f = (\ () -> do
#if 0
                    hPutStrLn stderr "EXCEPTION ACTION"
                    hFlush stderr
#endif
                    evaluate x''
                    return x'')
#endif
    isDeepSeqBounded_PingException DeepSeqBounded_PingException{} = True
    isDeepSeqBounded_PingException _ = False

-- The constants 6 and fixed_pat are specific to the leaky-1.0 package.
-- They are minimal sufficient depth and pattern (respectively) to plug leaky.
#if SEQABLE_ONLY
    x'' = force_ snk x
--  x'' = force_ Insulate x
#else
#if NFDATAN_ONLY
    x'' = forcen depth x
--  x'' = forcen 6 x
#else
    pat' = pat
    x'' | stats_query_idx <= (1+max_depth)
           = forcep pat x  -- trying to use *n patterns instead
---        = forcep_ pat x  -- deprecated function!
--         = forcen depth x
        | otherwise
           = forcep pat x
---        = forcep_ pat x  -- deprecated function!
#endif
#endif

-------------------------------------------------------------------------------

#if SEQABLE_ONLY
  seqaidDispatchDyn :: Generic a => SiteID -> a -> a
  seqaidDispatchDyn _ x = x'
   where
--- !_ = trace t ()
    t = show $ typeOf x
    x' |         False = undefined
---    | t == "State"  = force_ Propagate x  -- never, right? (type synonym)
       | t == "TA"     = force_ Propagate x
       | t == "TB"     = force_ Propagate x
       | t == "TC"     = force_ Propagate x
       | otherwise     =                  x
---    | otherwise     = force_ Insulate  x
#else
#if NFDATAN_ONLY
  seqaidDispatchDyn :: (Typeable a,NFDataN a) => SiteID -> a -> a
  seqaidDispatchDyn _ x = x'
   where
--- !_ = trace t ()
    t = show $ typeOf x
    x' | t == "TA"     = forcen max_depth x
--  x' | t == "State"  = forcen max_depth x
       | otherwise     = x
#else
  -- [Very old function, never really used.]
  -- Note that NFDataP already has Typeable superclass.
  -- (This is not ideal perhaps, as a lot of NFDataP's
  -- functionality doesn't depend on Typeable...).
  -- See note to seqaidDispatch,
  seqaidDispatchDyn :: (NFData a,NFDataN a,Typeable a,NFDataP a) =>
                       SiteID -> a -> a
--seqaidDispatchDyn :: NFDataP a => SiteID -> a -> a
--seqaidDispatchDyn :: NFDataP a => a -> a
--seqaidDispatchDyn :: (Typeable a,NFDataP a) => a -> a
  seqaidDispatchDyn _ x = x'
   where
--- !_ = trace t ()
    t = show $ typeOf x
    x' | t == "TA"     = forcep fixed_pat x
--  x' | t == "State"  = forcep fixed_pat x
--- x' | t == "TA"     = forcep_ fixed_pat x  -- deprecated function!
       | otherwise     = x
#endif
#endif

-------------------------------------------------------------------------------

  padr :: Int -> String -> String
  padr n s = s ++ (take (n-(length s)) $ repeat ' ')
  padl :: Int -> String -> String
  padl n s = (take (n-(length s)) $ repeat ' ') ++ s

-------------------------------------------------------------------------------

  -- use it on (String-ified) types!... (See caveat in Core.hs.)
  dropQuals :: String -> String
  dropQuals = reverse . takeWhile (/= '.') . reverse

-------------------------------------------------------------------------------

  fst3 :: (a,b,c) -> a
  fst3 (x,_,_) = x
  snd3 :: (a,b,c) -> b
  snd3 (_,y,_) = y
  thd3 :: (a,b,c) -> c
  thd3 (_,_,z) = z

-------------------------------------------------------------------------------

