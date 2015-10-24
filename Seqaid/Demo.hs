
  {-# LANGUAGE CPP #-}

-- |
-- Module      :  Seqaid.Demo
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  POSIX, Cabal
--
-- Instrument a sample program (package
-- <http://hackage.haskell.org/package/leaky leaky>)
-- with dynamic forcing functions.  Refer to
-- <http://hackage.haskell.org/package/deepseq-bounded deepseq-bounded>
-- for more information about this methodology.

#ifdef mingw32_HOST_OS
#define USE_DOS 0
#else
#if IS_WINDOWS
#define USE_DOS 1
#else
#define USE_DOS 0
#endif
#endif

  module Main ( main ) where
--module Seqaid.Demo ( main ) where

  import System.Environment ( getArgs )
  import System.IO ( hGetContents, hClose )
  import Control.Exception ( evaluate )
  import qualified System.IO.Temp as T
  import qualified System.Directory as D
  import System.Process
  import System.Exit

  import Paths_seqaid
  import Data.Version
  import Data.List ( intercalate )
  import Data.Char ( isSpace )

#if USE_DOS
  -- Not using the constraint, because requires performing regex
  -- on the output of a system command.  I'd like to provide the
  -- demo for non-mingw32 Windows users, but I have no means of
  -- testing that (or mingw32 for that matter).  Hopefully what's
  -- here works (presuming there's even such a thing as Haskell
  -- on Windows without mingw32).
  leaky_hard_version_windows
   = (intercalate "." $ map show $ take 3 $ versionBranch version) ++ ".0"
-- = "0.1.6.0"  -- XXX need a better way!!....
#else
  leaky_constraint
   = let vlo = take 3 $ versionBranch version
         vhi = take 2 vlo ++ [1 + last vlo]
     in    "'leaky >= "
        ++ (intercalate "." $ map show vlo)
        ++ " && < "
        ++ (intercalate "." $ map show vhi)
        ++ "'"
#endif

  main :: IO ExitCode
  main = do
--          putStrLn $ leaky_version
--          error "DEVEXIT"
            args <- getArgs

            if args /= ["demo"]
            then do
              putStrLn "seqaid: \"seqaid demo\" is the only supported invocation at this time."
              return $ ExitFailure 1
            else do

              tdir <- T.createTempDirectory "." "leaky_"
              D.setCurrentDirectory tdir

#if ! USE_DOS

              -- Find the most recent available version of package leaky
              -- which satisfies the constraint computed above.  (We use
              -- cabal-install to do this for us.)
              let command_to_resolve_constraint_for_get
                   =    "#!/bin/bash\nset -e\n"
                     ++ "cabal install leaky --dry-run --constraint="
                     ++ leaky_constraint
                     ++ " | tail -1"
--                   ++ " | grep -e '^leaky-'"
                     ++ " | sed -e 's/\\([^ ][^ ]*\\)\\($|..*\\)/\\1/'"
                     ++ "\n"  -- so file ends with EOL (good practice)

              writeFile "seqaidresolveleaky.sh" command_to_resolve_constraint_for_get
              p <- D.getPermissions "seqaidresolveleaky.sh"
              D.setPermissions
                 "seqaidresolveleaky.sh" p { D.executable = True }
              -- not sure how to handle exit status this way
              (_, Just hout, _, _)
                <- createProcess (proc "seqaidresolveleaky.sh" [])
                                   { std_out = CreatePipe }
              resolved' <- hGetContents hout
              evaluate $ length resolved'  -- force reading of whole file!
              hClose hout
              let resolved = reverse $ dropWhile isSpace $ reverse resolved' 

#endif

#if USE_DOS

-- XXX Note that "cabal get" already extracts the tarball for you.
-- XXX Note mingw32 uses bash (not DOS/CMD).
              let dos_str = intercalate "\n"
                   [ "@echo off"
                   , "cabal get leaky-" ++ leaky_hard_version_windows
                   , "cd leaky-" ++ leaky_hard_version_windows
                   , "cabal configure"
                   , "cabal build"
                   , "cabal run 234"
                   , "echo."
                   , "echo \"(Please see " ++ tdir ++ "/leaky-" ++ leaky_hard_version_windows ++ "/README for the interpretation.)\""
                   , "echo."
                   , ""  -- so file ends with EOL (good practice)
                   ]

#else

              let nix_str = intercalate "\n"
                   [ "#!/bin/bash\nset -e"
#if 1
                   , "cabal get " ++ resolved
                   , "cd " ++ resolved
#else
                   , "cabal get leaky --constraint \"" ++ leaky_constraint ++ "\""  -- sadly not
                   , "cd leaky-*"
#endif
                   , "cabal configure"
                   , "cabal build"
                   , "cabal run 234"
                   , "echo"
                   , "echo \"(Please see " ++ tdir ++ "/" ++ resolved ++ "/README for the interpretation.)\""
--                 , "echo \"(Please see " ++ tdir ++ "/*" ++ "/README for the interpretation.)\""
                   , "echo"
                   , ""  -- so file ends with EOL (good practice)
                   ]

#endif

#if USE_DOS
              let seqaid_init_script_contents = dos_str
              let seqaid_init_script_name = "seqaidinit.bat"
#else
              let seqaid_init_script_contents = nix_str
              let seqaid_init_script_name = "seqaidinit.sh"
#endif

              writeFile seqaid_init_script_name seqaid_init_script_contents
              p <- D.getPermissions seqaid_init_script_name
              D.setPermissions seqaid_init_script_name p { D.executable = True }
              st <- system $ "./" ++ seqaid_init_script_name
              case st of
                ExitSuccess -> return ExitSuccess
                ExitFailure n -> do
                  putStrLn $ "Sorry, seqaid demo had a problem (status=" ++ show st ++ ").\nPlease report this bug to rasfar@gmail.com"
                  return $ ExitFailure 2
                _ -> error $ "unexpected ExitCode = " ++ show st ++ "\nPlease report this bug to rasfar@gmail.com"

