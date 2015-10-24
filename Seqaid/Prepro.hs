
-------------------------------------------------------------------------------

  {-# LANGUAGE CPP #-}

  {-# LANGUAGE BangPatterns #-}  -- temporary... (debugging/testing)

#define PKGNAMEVERS 0

#define DBG_OMNI 0

-- Sadly it seems there is a bad interaction between SOP.TH and Seqaid.TH,
-- so at least for the time being, data types you need to force will
-- need to be declared in a module which is not itself harnessed.
#define CAN_MIX_SOP_DERIVING_WITH_SEQAID_TH 0

-------------------------------------------------------------------------------

-- |
-- Module      :  Seqaid.Prepro
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC
--
-- Seqaid preprocessor, run via GHC -F.
-- In order to use seqaid there is some configuration required,
-- both in your project .cabal file, and in a seqaid.config file.
-- Refer to the project
-- <http://www.fremissant.net/seqaid#using homepage>
-- (also distributed locally in the HTML directory of the seqaid package)
-- for more details.

  module Main where
--module Seqaid.Prepro where

  import Seqaid.Config

  import System.Environment

  import Text.Regex.PCRE
  import Text.Regex.Base.RegexLike
  import Data.List ( foldl' )

  import System.Process ( system )  -- debugging only

  import GHC.Exts ( sortWith )
  import Data.List ( groupBy )
  import Data.List ( foldl1' )
  import Data.List ( intercalate )
  import Data.List ( nub )
  import Data.List ( sort )
  import Data.List ( group )
  import Data.Maybe

  import Control.Concurrent ( threadDelay )

--import System.Directory ( removeFile )
  import System.Directory ( createDirectoryIfMissing )
  import System.Directory ( getTemporaryDirectory )
  import System.Directory ( doesFileExist )

  import Distribution.PackageDescription
  import Distribution.PackageDescription.Parse
  import Distribution.Package
  import Distribution.Verbosity
  import Distribution.Compiler

  import Data.Char ( toUpper )
  import Data.Char ( isSpace )

  import System.IO ( openFile, IOMode(ReadMode), hFileSize, hClose )

-------------------------------------------------------------------------------

  data SeqaidConfig
        = SeqaidConfig {
              seqaid_cfg_package   :: String
            , seqaid_cfg_modules   :: [SeqaidConfigModule]
            , seqaid_cfg_instances :: [(String,[String])]
          } deriving ( Show )
  data SeqaidConfigModule
        = SeqaidConfigModule {
              seqaid_cfg_fqname :: String
            , seqaid_cfg_types :: [String]
            , seqaid_cfg_binds :: [String]
          } deriving ( Show )

-------------------------------------------------------------------------------

  breakOn :: Char -> String -> String -> [String] -> [String]
  breakOn c [] sacc acc
   | null sacc  = reverse acc
   | otherwise  = reverse (reverse sacc:acc)
  breakOn c (h:t) sacc acc
   | h == c     = breakOn c t [] (reverse sacc:acc)
   | otherwise  = breakOn c t (h:sacc) acc

  trim :: String -> String
  trim s = reverse $ dropWhile (==' ') $ reverse $ dropWhile (==' ') s

  parseTypes :: String -> [String]
  parseTypes s = map trim $ breakOn ',' s [] []

  parseBinds :: String -> [String]
  parseBinds s = map trim $ breakOn ',' s [] []

  parseInstancesStr :: String -> [(String,[String])]
  parseInstancesStr s = parseInstances mts
   where
    mts = parseInstances' s :: [(String,String)]
    parseInstances' :: String -> [(String,String)]
    parseInstances' s = map splitFQN $ map trim $ breakOn ',' s [] []

  parseInstances :: [(String,String)] -> [(String,[String])]
  parseInstances mts = mts'''
   where
    mts' = sortWith snd mts :: [(String,String)]
    mts'' = groupBy (\(x1,y1) (x2,y2) -> x1 == x2) mts' :: [[(String,String)]]
    mts''' = map (foldl' (\ (_,ys) (x,y) -> (x,ys++[y])) ("",[])) mts'' :: [(String,[String])]

  -- XXX I've seen GHC API (or TH?) code for this, no?
  splitFQN :: String -> (String,String)
  splitFQN s = ( reverse $ drop 1 $ dropWhile (/='.') $ reverse s
               , reverse $ takeWhile (/='.') $ reverse s ) -- heh

  parseConfigLines :: SeqaidConfig -> [String] -> SeqaidConfig
  parseConfigLines config [] = config
  parseConfigLines config (l:ls) = config'
   where
    -- XXX looks like I'm discarding all layout info right here, so far:
    l' = dropWhile (==' ') l
    config'
     | null l'  = parseConfigLines config ls
     | head l' == '#'  = parseConfigLines config ls
     | head l' == 'p'
        = parseConfigLines (config { seqaid_cfg_package = dropWhile (==' ') $ drop 1 $ dropWhile (/=' ') l' }) ls
     | head l' == 'm'
        = parseConfigLines (config { seqaid_cfg_modules = seqaid_cfg_modules config ++ [ SeqaidConfigModule { seqaid_cfg_fqname = dropWhile (==' ') $ drop 1 $ dropWhile (/=' ') l' , seqaid_cfg_types = [], seqaid_cfg_binds = [] } ] }) ls
     | head l' == 't'
        = let modules = seqaid_cfg_modules config in
          if null modules then error "#1 invalid seqaid.config"
          else let last_module = last modules in
               parseConfigLines (config { seqaid_cfg_modules = init modules ++ [ last_module { seqaid_cfg_types = parseTypes $ dropWhile (==' ') $ drop 1 $ dropWhile (/=' ') l' } ] }) ls
     | head l' == 'b'
        = let modules = seqaid_cfg_modules config in
          if null modules then error "#1 invalid seqaid.config"
          else let last_module = last modules in
               parseConfigLines (config { seqaid_cfg_modules = init modules ++ [ last_module { seqaid_cfg_binds = parseBinds $ dropWhile (==' ') $ drop 1 $ dropWhile (/=' ') l' } ] }) ls
     | head l' == 'i'
        = parseConfigLines (config { seqaid_cfg_instances = parseInstancesStr $ dropWhile (==' ') $ drop 1 $ dropWhile (/=' ') l' }) ls
     | otherwise  = error "#2 invalid seqaid.config"

  parseConfig :: IO SeqaidConfig
  parseConfig = do
    config_s <- readFile "seqaid.config"
    let config_dflt = SeqaidConfig { seqaid_cfg_package = "", seqaid_cfg_modules = [], seqaid_cfg_instances = [] }
    let config_dflt' = parseConfigLines config_dflt $ filter (not . isBlankLine) $ lines config_s
    return config_dflt'
   where
    isBlankLine :: String -> Bool
    isBlankLine s = null $ filter (not . isSpace) s

  lookupModule :: String -> SeqaidConfig -> SeqaidConfigModule
  lookupModule modname config = configmod
   where
    configmod = lookupModule' modname $ seqaid_cfg_modules config
    lookupModule' :: String -> [SeqaidConfigModule] -> SeqaidConfigModule
    lookupModule' modname [] = error "#3 invalid seqaid.config"
    lookupModule' modname (h@(SeqaidConfigModule fqname types binds):t)
     | modname == fqname  = h
     | otherwise          = lookupModule' modname t

-------------------------------------------------------------------------------

  main = do
   args <- getArgs
   if length args < 3
   then error "takes a minimum of 3 arguments (should not be run manually!)"
   else do

    let origname = args!!0
    let infile = args!!1
    let outfile = args!!2

#if 0
    ttt <- doesFileExist outfile
    if ttt
    then do
      putStrLn "exists"
      h <- openFile "outfile" ReadMode
      siz <- hFileSize h
      hClose h
      putStrLn $ "File size is " ++ show siz
    else putStrLn "doesn't exist"
    error "DEVEXIT"
#else
    outfile_already_exists <- doesFileExist outfile
    okay_to_proceed
     <- if outfile_already_exists
        then do
          h <- openFile "outfile" ReadMode
          siz <- hFileSize h
          hClose h
          if 0 == siz then return True else return False
        else return True
    if not okay_to_proceed
    then error "3rd argument exists (should not be run manually!)"
    else return ()
#endif

    let opts = drop 3 args
    let internal = "internal" `elem` opts
    let doomni = "omnitypic" `elem` opts
--  putStrLn $ "origname=" ++ origname ++ "\ninfile=" ++ infile ++ "\noutfile=" ++ outfile ++ "\ninternal=" ++ show internal ++ "\nomnitypic=" ++ show doomni

    config@(SeqaidConfig packagename modules instances') <- parseConfig
--  putStrLn $ show config

    let instances = instances'

#if PKGNAMEVERS
    (pkgname,pkgvers,cppopts_lst,ghcopts_lst) <- parseCabal $ packagename ++ ".cabal"
    let boo = (cppopts_lst,ghcopts_lst)
#else
    boo@(cppopts_lst,ghcopts_lst) <- parseCabal $ packagename ++ ".cabal"
#endif
--  putStrLn $ show ghcopts_lst

#if CAN_MIX_SOP_DERIVING_WITH_SEQAID_TH
    do
#else
    -- Test that no module mentioned in the "instances:" field
    -- is also mentioned in a "module:" field. (Probably a temporary
    -- restriction; due to a bad interaction between Generics.SOP.TH
    -- and Seqaid.TH. Considering Generics.SOP.TH output fails -dcore-lint,
    -- the problem might be a bug in SOP.)
    let sane = testSaneConfig config
    if not sane then error "seqaid.config is not sane"
    else do
#endif

     lexmod_ <- readFile infile

     let allmats = getAllTextMatches $ lexmod_ =~ "^ *module [^\n][^\n]*" :: [String]

     let missing_module_declaration = null allmats

     let indent
          | missing_module_declaration
             = replicate (fromJust $ guessBaseIndentationLevel lexmod_) ' '
---       | missing_module_declaration  = ""
          | otherwise
             = let n = length $ takeWhile (==' ') $ head allmats in
               replicate n ' '

     let lexmod_mod
          | missing_module_declaration
             = indent ++ "module Main ( main ) where\n"
---          = error $ "module missing \"module\" declaration: " ++ origname
          | otherwise
             = head allmats

     if missing_module_declaration
     then putStrLn $ "Missing \"module\" declaration: " ++ origname ++ "\n  Assuming module Main\n  Base indentation level appears to be " ++ show (length indent)
--   then putStrLn $ "Missing \"module\" declaration: " ++ origname ++ "\n  Assuming module Main and base indentation level 0"
     else return ()

     let lexmod__
          | missing_module_declaration  = lexmod_mod ++ lexmod_
          | otherwise                   =               lexmod_

     -- The takeWhile is an extra precaution, since may encounter
     --   module Foo(bar) where ...
     let modname = takeWhile (/='(') $ ((words lexmod_mod)!!1)  -- should be...

-- XXX Getting duplicate output from this (and output in
-- a seemingly-wrong order, too), so gave up on it for now.
-- (The point was only to avoid the TH "Loading pacakge" spam.)
#if 0
     if ( not internal ) && "-v0" `elem` ghcopts_lst
--- [2 of 2] Compiling Main             ( Seqaid/Prepro.hs, dist/dist-sandbox-c80c5f2e/build/seqaidpp/seqaidpp-tmp/Main.o )
     then putStrLn $ "Compiling " ++ modname
     else return ()
#endif

     (wasInstanceProcessed,lexmod) <-
      if modname `elem` map fst instances
      then do  -- SOP generic deriving of NFDataP and superclasses:

        let (mod,types) = lookupInstance modname instances

        let pragmas      = unlines $ map (indent++) $ makePragmas
        let imports      = unlines $ map (indent++) $ makeImports

        let seqaidvalidate = make_seqaid_validate types

        let dis = makeDIs types
        let is = makeIs types
        let dgens = makeDGens types

        let lexmod' = pragmas ++ lexmod__ :: String

        let go2 s = s ++ imports ++                 indent
        let lexmod'' = replaceAll False
                         (makeRegex "^ *module [^\n][^\n]*") go2 lexmod'

        let lexmod''' =    lexmod''
                        ++ (unlines (map (indent++) seqaidvalidate))
                        ++ (unlines (map (indent++) dis))
                        ++ (unlines (map (indent++) is))
                        ++ (unlines (map (indent++) dgens))

#if CAN_MIX_SOP_DERIVING_WITH_SEQAID_TH
        return (True,lexmod''')
#else
        writeFile outfile lexmod'''
---     _ <- system $ "/bin/cat " ++ outfile
        return (True,lexmod''')
#endif
      else return (False,lexmod__)

#if CAN_MIX_SOP_DERIVING_WITH_SEQAID_TH
     if    wasInstanceProcessed
        || ( not $ modname `elem` map seqaid_cfg_fqname modules )
#else
     if not $ modname `elem` map seqaid_cfg_fqname modules
#endif

     then do
        -- The module currently being processed is neither mentioned
        -- in the "instances" nor in a "module" field of seqaid.config.
        -- So do nothing.  This is identity, for a GHC -F prepro:
        writeFile outfile lexmod
---     _ <- system $ "/bin/cat " ++ outfile
        return ()

     else do

        -- XXX Some (maybe all) the !'s are probably unneeded.
        momnis <- if ( not doomni ) || internal
                   then return Nothing
                   else do
                     omnis <- omnitypic origname packagename modname boo
                     return $ Just omnis

        let (omni_types,omni_imports') = fromJust momnis

        let mod@(SeqaidConfigModule fqname types' binds) = lookupModule modname config

        let types = if isNothing momnis
                    then types'
                    else nub $ types' ++ omni_types  -- should suffice, here

        -- Inject the module currently being compiled:

        -- (1) Substitute all manual instrumentation with
        --     something more convenient to the plugin phase.
---     lexmod <- readFile infile
        let go [] = []  -- shouldn't be possible
            go (c:cs) = c : "seqaidDispatch undefined "
        let lexmod' = replaceAll True
                        (makeRegex "[^A-Za-z0-9'_]seqaid ") go lexmod

        let pragmas_ = unlines $ map (indent++) $ makePragmas_

        -- XXX It seems it is necessary to inject a
        -- {-# LANGUAGE TemplateHaskell #-} pragma, even though
        -- it may be given in .cabal with default-extensions: TemplateHaskell
        -- and with ghc-opts: -XTemplateHaskell!  And using
        -- {-# LANGUAGE CPP #-} also works, for some unknown reason!

        -- (2) Inject the pragmas:
        let go4 s = pragmas_ ++ s
        let lexmod''_ = replaceAll False
                          (makeRegex "^ *module ") go4 lexmod'

        let omni_imports
             = if isNothing momnis
               then []
               else unlines $ map (indent++) omni_imports'

        -- (3) Inject the Seqaid.TH import statement:
        -- Later: And also some type imports (for omnitypic wrapper injection).
-- XXX Later: Why is this #if 1 here??... If figure out that it should
-- be here, please leave a comment as to why!
#if 1
        let go2 s = indent ++ "import Seqaid.TH\n" ++ omni_imports ++ s
#else
#if __GLASGOW_HASKELL__ >= 710
        let go2 s = indent ++ "import Seqaid.TH_710\n" ++ omni_imports ++ s
#else
        let go2 s = indent ++ "import Seqaid.TH\n" ++ omni_imports ++ s
#endif
#endif
        let lexmod'' = replaceAll False
                           (makeRegex "^ *import ") go2 lexmod''_

        -- (4) Inject the seqaidTH splice call:
        let tqqs = map (\x -> "[t| " ++ x ++ " |]") types
-- Doesn't help, since when GHC reports the error, it re-ppr's onto one line.
-- (Also, the module qualifiers are absent!)
--      let lexmod'''' = lexmod'' ++ indent ++ "seqaidTH [ " ++ intercalate ("\n" ++ indent ++ "  , ") tqqs ++ " ]"
        let lexmod'''' = lexmod'' ++ indent ++ "seqaidTH [ " ++ intercalate ", " tqqs ++ " ]" ++ "\n"

        -- (5) Inject the strInstancesTH splice call:
        -- (Or should this be above (4)?...)
        let lexmod''''' = lexmod'''' ++ indent ++ "strInstancesTH" ++ "\n"

        -- (6) Inject the bindsIncludedTH splice call:
        -- (Or should this be above?...)
        let lexmod'''''' = lexmod''''' ++ indent ++ "bindsIncludedTH " ++ show binds ++ "\n"

#if TRY_INJECT_NOINLINE_ON_REQUESTED_BINDS
        let lexmod''''''' = lexmod'''''' ++ indent ++ "noinlineTH " ++ show binds ++ "\n"
#else
        let lexmod''''''' = lexmod''''''
#endif

        writeFile outfile lexmod'''''''
---     _ <- system $ "/bin/cat " ++ outfile
        return ()

-------------------------------------------------------------------------------

  -- Thanks to rampion in http://stackoverflow.com/questions/9071682/replacement-substition-with-haskell-regex-libraries
  replaceAll :: Bool -> Regex -> (String -> String) -> String -> String
  replaceAll all re f s = start end
    where
     allmats = getAllMatches $ match re s
     (_, end, start) = foldl' go (0, s, id) $ (if all || null allmats then id else take 1) allmats
     go (ind,read,write) (off,len) =
       let (skip, start) = splitAt (off - ind) read 
           (matched, remaining) = splitAt len start 
       in (off + len, remaining, write . (skip++) . (f matched ++))

-------------------------------------------------------------------------------

  make_seqaid_validate ts = lines $ make_seqaid_validate' ts
  make_seqaid_validate' ts = "seqaidValidate [ " ++ intercalate ", " (map ("''"++) ts) ++ " ]\n"

-------------------------------------------------------------------------------

  makeDIs ts = lines $ makeDIs' ts
  makeDIs' [] = []
  makeDIs' (t:ts) = concat
    [ "deriving instance Show " ++ t ++ "\n"
    , "deriving instance Generic " ++ t ++ "\n"
    , "deriving instance Typeable " ++ t ++ "\n"
    , "deriving instance Data " ++ t ++ "\n" ++ makeDIs' ts
    ]

-------------------------------------------------------------------------------

  makeIs ts = lines $ makeIs' ts
  makeIs' [] = []
  makeIs' (t:ts) = concat
    [ "instance NFDataP " ++ t ++ " where rnfp = grnfp\n"
    , "instance NFDataN " ++ t ++ " where rnfn = grnfn\n"
    , "instance NFData  " ++ t ++ " where rnf  = genericRnf\n" ++ makeIs' ts
    ]

-------------------------------------------------------------------------------

  makeDGens ts = lines $ makeDGens' ts
  makeDGens' [] = []
  makeDGens' (t:ts) = "deriveGeneric ''" ++ t ++ "\n" ++ makeDGens' ts

-------------------------------------------------------------------------------

  makeITs mod ts = lines $ makeITs' ts
   where
    makeITs' [] = []
    makeITs' (t:ts) = "import " ++ qual ++ " ( " ++ name ++ "(..)" ++ " )" ++ "\n" ++ makeITs' ts
     where
      qual = mod
      name = t

-------------------------------------------------------------------------------

  makePragmas
   = [ "\n"
     , "-- For NFDataP (which perforce includes NFDataN and NFData):\n"
     , "{-# LANGUAGE CPP #-}\n"
     , "{-# LANGUAGE TemplateHaskell #-}\n"
     , "{-  LANGUAGE ScopedTypeVariables #-}\n"
     , "{-# LANGUAGE DataKinds #-}\n"
     , "{-# LANGUAGE TypeFamilies #-}\n"
     , "{-  LANGUAGE ConstraintKinds #-}\n"
     , "{-# LANGUAGE GADTs #-}  -- for GHC 7.6.3\n"
     , "{-# LANGUAGE DeriveGeneric #-}\n"
     , "{-# LANGUAGE DeriveDataTypeable #-}\n"
     , "{-# LANGUAGE StandaloneDeriving #-}\n"
     , "\n"
     , "{-  LANGUAGE TypeSynonymInstances #-}\n"
     , "\n"
     , "-- RankNTypes wanted since some injected type signatures,\n"
     , "-- due to imported types, may require it.\n"
     , "{-# LANGUAGE RankNTypes #-}\n"
     ]

-------------------------------------------------------------------------------

  makePragmas_
   = [ "\n"
     , "{-  LANGUAGE CPP #-}\n"
     , "{-# LANGUAGE TemplateHaskell #-}\n"
     ]

-------------------------------------------------------------------------------

  makeImports
   = [ "\n"
     , "import Control.DeepSeq.Bounded\n"
     , "import Control.DeepSeq.Generics\n"
     , "\n"
#if 1
     , "import Seqaid.TH ( seqaidValidate )\n"
#else
#if __GLASGOW_HASKELL__ >= 710
     , "import Seqaid.TH_710 ( seqaidValidate )\n"
#else
     , "import Seqaid.TH ( seqaidValidate )\n"
#endif
#endif
     , "import Generics.SOP.TH\n"
     , "import GHC.Generics ( Generic )\n"
     , "import Data.Typeable ( Typeable )\n"
     , "import Data.Data ( Data )\n"
     ]

-------------------------------------------------------------------------------

  testSaneConfig :: SeqaidConfig -> Bool
  testSaneConfig config@(SeqaidConfig packagename modules instances) = b
   where
    bs = zipWith ($)
           (map (elem . fst) instances)
           (repeat $ map seqaid_cfg_fqname modules)
    b = null bs || not (or bs)

-------------------------------------------------------------------------------

  lookupInstance :: String -> [(String,[String])] -> (String,[String])
  lookupInstance modname [] = error "lookupInstance: unexpected!"
  lookupInstance modname (i@(nm,tys):t)
   | modname == nm  = i
   | otherwise      = lookupInstance modname t 

-------------------------------------------------------------------------------

  -- Use a trick to obtain the list of all subexpression types in the module.
  -- This has a high compile time cost (as much as doubles build time),
  -- but that may be acceptable for a diagnostic build, and it's
  -- potentially quite a powerful fine-grained blanket harness.
  omnitypic :: String -> String -> String -> ([String],[String]) -> IO ([String],[String])
  omnitypic origname packagename modname (cppopts_lst,ghcopts_lst)= do
    systmp <- getTemporaryDirectory
    let ghctmp = systmp ++ "/ghctmp-seqaidpp"
--- let ghctmp = "/media/ramdisk/ghctmp-seqaidpp"
    createDirectoryIfMissing False ghctmp
    -- XXX this tmpfile is actually not used at all (just as a /dev/null)
    let tmp_filename = "seqaid_tmp_321"
    let tmp_pname = ghctmp ++ "/" ++ tmp_filename
    let omni_types_pname = ghctmp ++ "/omnitypes.txt"
    let omni_imports_pname = ghctmp ++ "/omniimports.txt"
    let cppopts = intercalate " " cppopts_lst
    let ghcopts' = intercalate " " ghcopts_lst

-- XXX Alternatively, could use a shell script to delegate, as done
-- in leaky (to cope with this rogue).  But hopefully this suffices,
-- and hackage resolves this undesireable situation with build reporting
-- at which time we can simplify, and depend on build tools being
-- installed in places from which they will be visible.

    in_sandbox <- detectSandbox

    i_am_misconfigured <- doesFileExist
      "/home/builder/hackage-server/build-cache/tmp-install/bin/seqaidpp"

    let nowneeded = intercalate " "
          [ "-fplugin=Seqaid.Plugin"
          , "-F -pgmF " ++ ( if i_am_misconfigured
                             then "/home/builder/hackage-server/build-cache/tmp-install/bin/seqaidpp"  -- please fix your broken build reports, they make me, cpphs, and hackage look bad
                             else if in_sandbox
                                  then ".cabal-sandbox/bin/seqaidpp"
                                  else "seqaidpp" )
          , "-optF omnitypic"
          ]

-- The -c option together with --make will prevent linking, while
-- still building all dependencies.
    let ghcopts = intercalate " "
          [ ghcopts'
          , "--make "
          , "-c "
#if USE_CPPHS
          , "-pgmPcpphs -optP--cpp"
#else
--        , "-cpp"  -- (see comments in the seqaid.cabal file)
#endif
          , "" ++ nowneeded
          , "-fplugin-opt=Seqaid.Plugin:prepro=" ++ modname
          , "-fplugin-opt=Seqaid.Plugin:" ++ omni_types_pname
          , "-fplugin-opt=Seqaid.Plugin:" ++ omni_imports_pname
          , "-optF internal"
          , "-XTemplateHaskell"
          , "-with-rtsopts=-T"
          , "-outputdir " ++ ghctmp
          ]

-- These are either optional, or already required to be in the .cabal file.
--
-- Probably IS going to be wanted:
---     , " -threaded "
-- Already required to be in the .cabal file:
-- Later: But it's now guarded by a condition (flag), so
-- it doesn't show up for parseCabal! (So added to the above.)
---     , " -fplugin=Seqaid.Plugin "
---     , " -F "
---     , " -pgmF seqaidpp "
-- Optional:
---     , " -optP-Wundef "
---     , " -fno-warn-overlapping-patterns "
-- Seem not to be needed if supply -with-rtsopts (which we do):
---     , " -rtsopts "

#if 0
    putStrLn $ "CPPOPTS:\n" ++ show cppopts
    putStrLn $ "GHCOPTS:\n" ++ show ghcopts
--  error "DEVEXIT"
#endif

    let cmnd =    "cabal exec -- ghc "
               ++ ghcopts ++ " "
               ++ cppopts ++ " "
#if 1
                                        -- /dev/null would do ... if it exists
               ++ origname ++ " >> " ++ tmp_pname ++ " 2>&1"
#else
               ++ origname
#endif
#if DBG_OMNI
    putStrLn cmnd
#endif
    writeFile tmp_pname $ cmnd ++ "\n"
    _ <- system cmnd
--- _ <- system $ "cat " ++ tmp_pname
    omni_types' <- readFile omni_types_pname  -- as a String, one type per line
    -- (removing temp files, so stale temp files don't get read by wrong
    -- module pass, in case that's what's happening...)
--  removeFile omni_types_pname
    omni_imports' <- readFile omni_imports_pname
--  removeFile omni_imports_pname
#if DBG_OMNI
    putStrLn omni_types'
    putStrLn omni_imports'
--  error "DEVEXIT"
#endif
    let omni_types = lines omni_types'
    let omni_imports = lines omni_imports'
    return (omni_types,omni_imports)

-------------------------------------------------------------------------------

  mergeInstances :: [(String,[String])] -> [(String,[String])] -> [(String,[String])]
  mergeInstances instances instances' = instances''
   where
    sis = splitInstances instances
    sis' = splitInstances instances'
    sis'' = nub $ sis ++ sis'
    instances'' = parseInstances sis''

  splitInstances :: [(String,[String])] -> [(String,String)]
  splitInstances [] = []
  splitInstances (h@(m,ts):t) = map (\x -> (m,x)) ts ++ splitInstances t

-------------------------------------------------------------------------------

#if PKGNAMEVERS
  parseCabal :: String -> IO (String,String,[String],[String])
#else
  parseCabal :: String -> IO ([String],[String])
#endif
  parseCabal cabal_pname = do
    gpd <- readPackageDescription silent cabal_pname
    let pd = packageDescription gpd
    let flags = genPackageFlags gpd
    let flags' = map (\ (MkFlag (FlagName a) b c d) -> (map toUpper a, c)) flags
    let cppopts_flags = map (\ (x,y) -> " -D" ++ x ++ "=" ++ if y then "1" else "0") flags'
    let cppopts_flags_string = intercalate " " cppopts_flags
#if 0
MkFlag	 
flagName :: FlagName
flagDescription :: String
flagDefault :: Bool
flagManual :: Bool
#endif
    let condexes = condExecutables gpd
    let condexe@(_, condexetree) = head condexes
    let Executable _ _ buildinfo = condTreeData condexetree
#if DBG_OMNI
    putStrLn $ "BUILDINFO:\n" ++ show buildinfo
#endif
    let cppopts_cppopts = cppOptions buildinfo
    let cppopts = cppopts_flags ++ cppopts_cppopts
    let ghcopts = process_opts $ options buildinfo
#if PKGNAMEVERS
    let PackageName pkgname = pkgName $ package pd
    let pkgvers = show $ pkgVersion $ package pd
    return (pkgname,pkgvers,cppopts,ghcopts)
#else
    return (cppopts,ghcopts)
#endif
   where
    process_opts :: [(CompilerFlavor, [String])] -> [String]
    process_opts [] = []
    process_opts (h@(cf,ss):t)
     | GHC <- cf  = ss ++ process_opts t
     | otherwise  =       process_opts t

-------------------------------------------------------------------------------

  guessBaseIndentationLevel :: String -> Maybe Int
  guessBaseIndentationLevel str
   | null lst'  = Nothing
   | otherwise  = Just $ snd $ head lst'
   where
    lst =   map (length . takeWhile (==' '))
          $ filter (not . isDashDashCommentLine)
          $ filter (/="")
          $ lines str
--  lst' = sort $ zip lst [0..]
    lst' = map (\x -> (length x,head x)) $ group $ sort lst  -- histogram of indentation levels
    isDashDashCommentLine :: String -> Bool
    isDashDashCommentLine (' ':cs) = isDashDashCommentLine cs
    isDashDashCommentLine ('-':'-':_) = True
    isDashDashCommentLine _ = False

-------------------------------------------------------------------------------

  -- (I know of no better way.)
  detectSandbox :: IO Bool
  detectSandbox = doesFileExist "cabal.sandbox.config"

-------------------------------------------------------------------------------

