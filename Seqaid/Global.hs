
-------------------------------------------------------------------------------

-- XXX I'm not entirely happy with the way Haskell has
-- twisted my arm to arrange the modules, but there you go.

  {-  OPTIONS_GHC -O2 #-}

  {-# LANGUAGE CPP #-}

-- XXX Should scour the code for "max_depth" etc. -- all those
-- names which are specific to this DEMO_MODE hack!... And get
-- them into CPP guards. And get something sane in the OTHER
-- branches of those guards!!...
-- (Switch now promoted to .cabal flag.)
--- #define DEMO_MODE 1

  {-  LANGUAGE BangPatterns #-}   -- for debugging only

-------------------------------------------------------------------------------

-- |
-- Module      :  Seqaid.Global
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC (uses global IORefs)
--
-- Collects 'IORef's used by the seqaid runtime.
--
-- This will be substantially reorganised and clarified soon.

  module Seqaid.Global (

#if DEMO_MODE
    depth_ioref                      ,
    pattern_ioref                    ,
    snk_ioref                        ,
#endif

#if ! DEMO_MODE
    patterns_ioref                   ,
#endif

    stats_query_idx_ioref            ,
    counter_ioref                    ,
    next_sample_at_ioref             ,
    bytes_allocated_ioref            ,
    bytes_allocated_prev_ioref       ,
    current_bytes_used_ioref         ,
    update_bytes_allocated_ioref     ,
    update_current_bytes_used_ioref  ,

    SiteID                           ,
    sample_period                    ,
    max_depth                        ,
    fixed_pat                        ,
    fixed_pat_sequence               ,

  ) where

-------------------------------------------------------------------------------

  import Control.DeepSeq.Bounded
  import Data.Typeable ( Typeable )

  -- Stuff for monitoring resource use (i.e. computing objective function):
--import Data.Word ( Word64 )
  import GHC.Int ( Int64 )
  import Data.IORef
  import System.IO.Unsafe ( unsafePerformIO )
  import System.Mem ( performGC )
  import GHC.Stats ( GCStats(..), getGCStats )

--import Debug.Trace ( trace )

--import Data.Array

#if ! DEMO_MODE
  import qualified Data.HashTable.IO as H
#endif

-------------------------------------------------------------------------------

#if ! DEMO_MODE
  type HashTable k v = H.CuckooHashTable k v

  {-# NOINLINE patterns_ioref #-}
  patterns_ioref :: IORef (HashTable Int Pattern)
  patterns_ioref = unsafePerformIO $ do
                     ht <- H.new
                     ioref <- newIORef ht
                     return ioref
#endif

-------------------------------------------------------------------------------

  -- This group is very temporary!)

  -- definitely temporary hack (uniform forcing depth regardless of SiteID)
  {-# NOINLINE depth_ioref #-}
  depth_ioref :: IORef Int
  depth_ioref = unsafePerformIO $ newIORef 0

  -- definitely temporary hack (uniform Pattern regardless of SiteID)
  {-# NOINLINE pattern_ioref #-}
  pattern_ioref :: IORef Pattern
  pattern_ioref = unsafePerformIO $ newIORef $ emptyPat

  -- definitely temporary hack (uniform SeqNode regardless of SiteID)
  {-# NOINLINE snk_ioref #-}
  snk_ioref :: IORef SeqNode
  snk_ioref = unsafePerformIO $ newIORef Insulate

-------------------------------------------------------------------------------

  {-# NOINLINE stats_query_idx_ioref #-}
  stats_query_idx_ioref :: IORef Int
--stats_query_idx_ioref :: IORef Int64
  stats_query_idx_ioref = unsafePerformIO $ newIORef 0

  {-# NOINLINE counter_ioref #-}
  counter_ioref :: IORef Int64
  counter_ioref = unsafePerformIO $ newIORef 0

  {-# NOINLINE next_sample_at_ioref #-}
  next_sample_at_ioref :: IORef Int64
  next_sample_at_ioref = unsafePerformIO $ newIORef sample_period
--next_sample_at_ioref = unsafePerformIO $ newIORef 0

  {-# NOINLINE bytes_allocated_ioref #-}
  bytes_allocated_ioref :: IORef Int64
  bytes_allocated_ioref = unsafePerformIO $ newIORef 0

  {-# NOINLINE bytes_allocated_prev_ioref #-}
  bytes_allocated_prev_ioref :: IORef Int64
  bytes_allocated_prev_ioref = unsafePerformIO $ newIORef 0

  {-# NOINLINE current_bytes_used_ioref #-}
  current_bytes_used_ioref :: IORef Int64
  current_bytes_used_ioref = unsafePerformIO $ newIORef 0

-------------------------------------------------------------------------------

  {-# NOINLINE update_bytes_allocated_ioref #-}
  update_bytes_allocated_ioref :: IO Int64
  update_bytes_allocated_ioref = do
    oldsize <- readIORef bytes_allocated_ioref
    writeIORef bytes_allocated_prev_ioref oldsize
    performGC
    gcstats <- getGCStats
    let newsize = bytesAllocated gcstats
#if 0
    putStrLn $ "\nsize="++show size
    i <- readIORef counter_ioref
    t <- readIORef next_sample_at_ioref
    putStrLn $ "i="++show i++"\nt="++show t
#endif
    writeIORef bytes_allocated_ioref newsize
    return $! newsize - oldsize
--  return newsize

-------------------------------------------------------------------------------

  {-# NOINLINE update_current_bytes_used_ioref #-}
  update_current_bytes_used_ioref :: IO Int64
  update_current_bytes_used_ioref = do
    performGC
    gcstats <- getGCStats
    let cbu = currentBytesUsed gcstats
    writeIORef
      current_bytes_used_ioref
      cbu
    return cbu

-------------------------------------------------------------------------------

  sample_period :: Int64
--sample_period = 1
--sample_period = 10
--sample_period = 400
--sample_period = 1000
  sample_period = 4000    -- the usual for leaky-full
--sample_period = 10000

  -- XXX quack! (6 is minimal; 5 is too small; the leak remains)
  max_depth = 7 :: Int

  fixed_pat_sequence
   =    ( map (\i -> compilePat ('*':show i)) [0,1..8] )
     ++ ( reverse $ condenseEq shrinkPat fixed_pat )

  fixed_pat
   = setPatternPatNodeUniqueIDs 0 $ compilePat "((!(!)(((!).!(!))))!(!(!)))"

-------------------------------------------------------------------------------

  -- From SAI.Data.Generics.Shape.SYB.Filter:
  condenseEq :: Eq a => (a -> a) -> a -> [a]  -- beware this can diverge
  condenseEq f z = condenseEq' $ iterate f z
   where
    condenseEq' (x:y:t) | x == y     = [x]
                        | otherwise  = x : condenseEq' (y:t)
    -- no other cases needed -- we know the argument is infinite

-------------------------------------------------------------------------------

  -- XXX Using a triple (esp. the 3rd component) is probably not
  -- the most performant choice... optimisation pending...
  -- XXX Would prefer 1 <-> 2, but to do that would require more
  -- code change to test...
  -- XXX This is no doubt located here to avoid a cyclical import!
  -- 1 (Int)    Contains index of forcing site in the AST of the binding.
  -- 2 (String) Contains site binding variable name.
  -- 3 (Int)    Caches the "unique" Int hash of name extended by index.
  type SiteID = (Int,String,Int)

-------------------------------------------------------------------------------

