
-- |
-- Module      :  Seqaid.Plugin
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC >= 7.4 (uses GhcPlugins)
--
-- Standard GHC Core plugin stub.
--
-- Use GHC option -fplugin=Seqaid.Plugin to activate.
-- (Other options, as well as a configuration file, are also required.
-- Refer to the included HTML documentation for more information; or online
-- at the seqaid <http://www.fremissant.net/seqaid#using homepage>.)

  module Seqaid.Plugin ( plugin ) where

  import Seqaid.Core ( seqinjectProgram )

  import GhcPlugins

  plugin :: Plugin
  plugin = defaultPlugin { installCoreToDos = install }

  install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
  install clopts todos = do
    reinitializeGlobals
--  putMsgS $ show clopts
--  error "Plugin.hs-DEVEXIT"
    let seqaidpass = CoreDoPluginPass "Seqinject" $ seqinjectProgram clopts
    -- XXX Comment: The alternate insertion points did not help me
    -- with the problems I was having (which were solved via
    -- a text-preprocessor, ultimately). The rules we wanted to apply
    -- will only fire when building the target, if they are IN the
    -- target (not in Seqaid.Runtime.seqaid). [Same applies to inlining
    -- it seems?] Finally solved the problem using GHC -F (text preprocessor).
    -- XXX I forgot I'd left this set to POST-Simplifier plugin insertion!
    -- Can you imagine?? I've been developing for like a solid week
    -- with it set that way, and never managed to notice....  Things
    -- should become a lot more staightforward now, hopefully...
#if 0
#elif 1
    return $ [ seqaidpass ] ++ todos  -- pre-simplifier (most stable)
#elif 0
    return $ todos ++ [ seqaidpass ]  -- XXX post-simplifier XXX
#elif 0
    -- Tells me: "Rule check results: no rule application sites"
    let rulepass = CoreDoRuleCheck InitialPhase "seqaid_internal/2"
--  let rulepass = CoreDoRuleCheck InitialPhase "seqaid_internal/1"
    return $ rulepass : seqaidpass : todos       -- pre-simplifier
--  return $ CoreDoStaticArgs : rulepass : seqaidpass : todos
#elif 0
    let simpmode
         = SimplMode {
               sm_names = []
--             sm_names = ["Simplifier"]
             , sm_phase = InitialPhase
             , sm_rules = False
             , sm_inline = True
             , sm_case_case = False
             , sm_eta_expand = False
             }
    let inlinerpass = CoreDoSimplify 1 simpmode
    return $ inlinerpass : seqaidpass : todos
#endif

#if 0
     data CoreToDo = CoreDoSimplify Int SimplifierMode | ...
     data SimplifierMode
           = SimplMode {
                 sm_names :: [String]
               , sm_phase :: CompilerPhase
               , sm_rules :: Bool
               , sm_inline :: Bool
               , sm_case_case :: Bool
               , sm_eta_expand :: Bool
               }
#endif

