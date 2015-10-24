
-------------------------------------------------------------------------------

  {-  OPTIONS_GHC -O2 #-}

  {-# LANGUAGE CPP #-}

-- XXX Should scour the code for "max_depth" etc. -- all those
-- names which are specific to this DEMO_MODE hack!... And get
-- them into CPP guards. And get something sane in the OTHER
-- branches of those guards!!...
-- (Switch now promoted to .cabal flag.)
--- #define DEMO_MODE 1

  {-# LANGUAGE BangPatterns #-}   -- for debugging only? (maybe more...)

-------------------------------------------------------------------------------

-- |
-- Module      :  Seqaid.Optim
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC (uses global IORefs)
--
-- Harness morphological code.
--
-- The optimiser is just barely begun, but implementing it is
-- straight-forward Haskell programming, as contrasted with most
-- of the supporting infrastructure.

  module Seqaid.Optim (

    run_IO_SM ,

    optimIO ,

  ) where

-------------------------------------------------------------------------------

  import Control.DeepSeq.Bounded
  import Data.Typeable ( Typeable )

  -- Stuff for monitoring resource use (i.e. computing objective function):
  import GHC.Int ( Int64 )
  import Data.IORef

#if SEQABLE_ONLY
  -- I've seen this one displayed, so going with the more formal name:
  import Generics.SOP.Universe ( Generic )
--import Generics.SOP ( Generic )
#endif

#if 0
#if ! DEMO_MODE
  import qualified Data.HashTable.IO as H
#endif
#endif

  import Control.Concurrent ( myThreadId )

--import Debug.Trace ( trace )

#if SAI_FORK
  import Seqaid.Optim_sai
#endif

-------------------------------------------------------------------------------

  import Seqaid.Global ( depth_ioref                  )
#if DEMO_MODE
  import Seqaid.Global ( pattern_ioref                )
#else
  import Seqaid.Global ( patterns_ioref               )
#endif
  import Seqaid.Global ( snk_ioref                    )
  import Seqaid.Global ( stats_query_idx_ioref        )
  import Seqaid.Global ( counter_ioref                )
  import Seqaid.Global ( next_sample_at_ioref         )
  import Seqaid.Global ( bytes_allocated_ioref        )
  import Seqaid.Global ( bytes_allocated_prev_ioref   )
  import Seqaid.Global ( current_bytes_used_ioref     )
  import Seqaid.Global ( update_bytes_allocated_ioref )

--depth_ioref                   :: IORef Int
--pattern_ioref                 :: IORef Pattern
--patterns_ioref                :: IORef (HashTable Int Pattern)
--snk_ioref                     :: IORef SeqNode
--stats_query_idx_ioref         :: IORef Int
--counter_ioref                 :: IORef Int64
--next_sample_at_ioref          :: IORef Int64
--bytes_allocated_ioref         :: IORef Int64
--bytes_allocated_prev_ioref    :: IORef Int64
--current_bytes_used_ioref      :: IORef Int64
--update_bytes_allocated_ioref  :: IO Int64

--type HashTable k v = H.CuckooHashTable k v

-------------------------------------------------------------------------------

  import Seqaid.Global ( update_current_bytes_used_ioref )
  import Seqaid.Global ( max_depth )
  import Seqaid.Global ( sample_period )
  import Seqaid.Global ( fixed_pat_sequence )
  import Seqaid.Global ( SiteID )

-------------------------------------------------------------------------------

  -- The plan is to keep this module pure.
  -- So, it must be up the call chain someplace that
  -- the IORef operations occur.
  --
  -- XXX Since that sounds like boilerplate, it might be
  -- a good idea to offer a wrapper to the pure, here,
  -- which also takes care of the IORef stuff...

-------------------------------------------------------------------------------
  -- | This is for internal use only.
  --
  -- /The seemingly redundant superclass constraints are necessary/
  -- /due to some weirdness in the TH or Core code (I forget the/
  -- /details); should try to get rid of them in case it's/
  -- /since become possible.../
{--}
  -- Okay!
  -- Now it is time to use the hash values
{--}
  -- XXX This is simply the collected unsafePerformIO calls
  -- that were initially scattered throughout seqaidDispatch.
  -- It happened to behave the same as that did, without change;
  -- but a once-over reorganising the logic slightly would be good...
{--}
  -- is this pragma necessary? does it even make sense with IO?...
  {-# NOINLINE run_IO_SM #-}
#if SEQABLE_ONLY
  run_IO_SM :: (Generic a,Typeable a)
               => SiteID -> a -> IO (Int,Int,Pattern,SeqNode,Int64,Int64,Int64,Int64,Int64)
#else
#if NFDATAN_ONLY
  run_IO_SM :: (NFDataN a,Typeable a)
               => SiteID -> a -> IO (Int,Int,Pattern,SeqNode,Int64,Int64,Int64,Int64,Int64)
#else
  run_IO_SM :: (NFData a,NFDataN a,Typeable a,NFDataP a)
               => SiteID -> a -> IO (Int,Int,Pattern,SeqNode,Int64,Int64,Int64,Int64,Int64)
#endif
#endif
  run_IO_SM sid x = do

--- myThreadId >>= writeIORef deepseq_bounded_ioref__main_thread_id

    stats_query_idx <- do
                        sqi <- readIORef stats_query_idx_ioref
                        return sqi

    depth <- do
              d <- readIORef depth_ioref
              return d

#if 0
-- XXX a code fragment expected to be useful (see seqaidDispatch where clause)
    tmp = stats_query_idx-(2+max_depth)
    pat' | tmp < length fixed_pat_sequence
            = fixed_pat_sequence!!tmp
         | otherwise
            = last fixed_pat_sequence
#endif

#if SEQABLE_ONLY
    let pat = emptyPat
#else
#if NFDATAN_ONLY
--- #error "NFDATA_ONLY is not valid at this time."
    let pat = emptyPat
#else
    pat <- do
#if 1 || DEMO_MODE
            let tmp = stats_query_idx
--          let tmp = stats_query_idx-(2+max_depth)
            let p | tmp < length fixed_pat_sequence
                     = fixed_pat_sequence!!tmp
                  | otherwise
                     = last fixed_pat_sequence
            tid <- myThreadId
            let p' = fmap (setPatNodePingParentTID tid) p
#if 0
            H.insert ht sid_hash p'
#endif
            return p'
#else
            ht <- readIORef patterns_ioref
            let sid_hash = thd3 sid
            mp <- H.lookup ht sid_hash
            if isNothing mp
            then do
              let p = emptyPat
              H.insert ht sid_hash p
              return p
            else do
              return $ fromJust mp
#endif
#endif
#endif

#if SEQABLE_ONLY
    let snk = Propagate
#else
    let snk = Propagate  -- just whatever
#endif

    i <- do
          ii <- readIORef counter_ioref
          modifyIORef' counter_ioref (1+)
          return ii
    t <- do
          tt <- readIORef next_sample_at_ioref
          return tt
    (size,cbu,t') <-
     if i >= t
     then do
        modifyIORef' next_sample_at_ioref (+sample_period)
        -- (the snd component of result is for repairing
        -- a lag in the value for t shown in trace lines)
        tt <- readIORef next_sample_at_ioref
        ba <- update_bytes_allocated_ioref
        cbu <- update_current_bytes_used_ioref
        return (ba,cbu,tt)
      else do
        ba <- readIORef bytes_allocated_ioref
        cbu <- readIORef current_bytes_used_ioref
        return (ba,cbu,t)

    if i >= t
    then do
      modifyIORef' stats_query_idx_ioref (1+)
      return ()
    else return ()

#if 0
    if stats_query_idx >= max_depth && i >= t
    then do
      let j = stats_query_idx - max_depth
      writeIORef pattern_ioref (fixed_pat_sequence!!j)
      return ()
    else return ()
#endif

    if depth <= max_depth && i >= t
    then do
#if 1
      modifyIORef' depth_ioref (1+)
#else
-- XXX need Data instances...
--    writeIORef pattern_ioref (mkPatN depth x)
#endif
      return ()
    else return ()

    optimIO
--- (!_,!_,!_,!_,!_,!_,!_,!_,!_) <- optimIO  -- XXX (as if XXX is nec. lol!)

   -- At the moment, this no longer seems necessary at all.
   -- (But maybe it was for when first getting it working,
   -- and sample_period was 1 [or at least small]; and
   -- when the test program did little work...).
   -- XXX Note that if we can avoid this, there is another benefit:
   -- We don't necessarily WANT to force the head of x! (forcen 0,
   -- or forcep "#")...
--- _ <- return $! x  -- works as well?...
    !_ <- return x  -- magic! thank you!!

    return (stats_query_idx,depth,pat,snk,i,t,size,cbu,t')
--  return ()

-------------------------------------------------------------------------------

#if ! SAI_FORK
  optimIO :: IO ()
  optimIO = return ()
#endif

-------------------------------------------------------------------------------

