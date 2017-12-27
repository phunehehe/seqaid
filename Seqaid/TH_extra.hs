
{-
Copyright (c) 2013, Jonathan Fischoff

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Jonathan Fischoff nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

-- This is based on Language.Haskell.TH.Module.Magic from
-- the modulespection package (Jonathan Fischoff, BSD3).
-- Maintainer is not responding to 7.10 patches so, decided
-- to plop it in and start eliding/modifying.  This is
-- still pretty much verbatim from modulespection.

{-  LANGUAGE CPP #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

  module Seqaid.TH_extra
  (

      -- * Name Introspection
      names
    , moduleNames

#if 0
      -- * Declaration Introspection
    , declarations
    , moduleDeclarations
#endif

  )
  where

#include "ghcplatform.h"

  import Language.Haskell.TH as TH
  import Data.Maybe
  import GHC
  import Module
  import GHC.Paths ( libdir )
  import DynFlags
  import Name as Name
  import RdrName
  import MonadUtils
  import HsDecls as HsDecls
  import SrcLoc
  import Bag
  import Control.Monad
  import Data.Monoid
  import System.IO.Temp
  import HeaderInfo
  import DriverPipeline
  import SysTools
  import Packages
  import Config
  import qualified Control.Monad.IO.Class as MTL
  import Control.Monad.Catch
  import Exception (throwIO)
  import GhcMonad
  import GHC.IO.Handle
  import System.FilePath.Posix (takeBaseName)

  -- | Get all the top level declarations of the current file.
  --   All names are returned whether they are exported or not.
  names :: Q [TH.Name]
  names = moduleNames . loc_filename =<< location

  -- | Get all the top level names of a given module.
  --   If a file path is used, all names, exported and internal
  --   are returned. If a module name is used, only the exported
  --   names are returned.
  moduleNames :: String -> Q [TH.Name]
#if 0
  moduleNames target
    = runIO $ runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        lookupModuleNames target
#else
  moduleNames target = runIO $
     defaultErrorHandler
        defaultFatalMessager
        defaultFlushOut
        $ do
           runGhc (Just libdir) $ do
             dflags <- getSessionDynFlags
             setSessionDynFlags dflags
             lookupModuleNames target
#endif

#if 0

  -- | Look up a name, and get out the declaration
  --   or return nothing
  nameToMaybeDec :: TH.Name -> Q (Maybe Dec)
  nameToMaybeDec name = do
     info <- reify name
     return $ case info of
        TyConI dec -> Just dec
        _          -> Nothing

  -- | Get all the type declarations of the current file.
  --   Function and pattern declarations are ignored ... for now.
  declarations :: Q [Dec]
  declarations = mapMaybeM nameToMaybeDec =<< names

  -- | Get all the top level names of a given module.
  --   If a file path is used, all names, exported and internal
  --   are returned. If a module name is used, only the exported
  --   names are returned.
  --   Function and pattern declarations are ignored ... for now.
  moduleDeclarations :: String -> Q [Dec]
  moduleDeclarations = mapMaybeM nameToMaybeDec <=< moduleNames

#endif

#if 1

#if __GLASGOW_HASKELL__ < 707
  instance MTL.MonadIO Ghc where
    liftIO = MonadUtils.liftIO
#endif

  instance MonadThrow Ghc where
    throwM  = liftIO . throwIO

  instance MonadCatch Ghc where
    catch   = gcatch

  instance MonadMask Ghc where
    mask f =
       Ghc $ \s -> mask $ \io_restore ->
                              let
                                 g_restore (Ghc m) = Ghc $ \s -> io_restore (m s)
                              in
                                 unGhc (f g_restore) s
    uninterruptibleMask = error "uninterruptibleMask"

#endif

  -- | Either try to parse a source file or if the module is
  --   part of library, look it up and browse the contents
  lookupModuleNames :: (MTL.MonadIO m, MonadCatch m, MonadMask m, GhcMonad m)
                    => String -> m [TH.Name]
  lookupModuleNames mName = do
   target <- targetId <$> guessTarget mName Nothing
   case target of
      TargetModule moduleName -> getExistingModuleNames
                             =<< lookupModule moduleName Nothing
      TargetFile filePath _   -> do
         dflags            <- getSessionDynFlags
         opts              <- liftIO $ getOptionsFromFile dflags filePath
         (newDFlags, unhandledFlags, _) <-
            liftIO $ parseDynamicFilePragma dflags opts
         liftIO $ checkProcessArgsResult newDFlags unhandledFlags
         if (xopt Opt_Cpp newDFlags) then do
            withSystemTempFile (takeBaseName filePath <> ".cpp") $ \cppFilePath handle -> do
               liftIO $ hClose handle
               liftIO $ doCpp newDFlags True False filePath cppFilePath
               srcOpts <- liftIO $ getOptionsFromFile newDFlags cppFilePath
               (newestDFlags, unhandled_flags, warns)
                   <- liftIO $ parseDynamicFilePragma newDFlags srcOpts
               liftIO $ checkProcessArgsResult newestDFlags unhandled_flags
               parseFile newestDFlags cppFilePath
         else
            parseFile newDFlags filePath

  -- | Turn ErrorMessages into a String
  errString :: Show a => Bag a -> String
  errString = unlines
            . map show
            . foldBag (<>) (:[]) []

  -- | Parse a file and collect all of the declarations names
  parseFile :: GhcMonad m => DynFlags -> FilePath -> m [TH.Name]
  parseFile dflags filePath = do
     src    <- liftIO $ readFile filePath
     let (warns, L _ hsModule) =
           either (error . errString) id
                 $ parser src dflags filePath

         names = mapMaybe getNameMaybe $ hsmodDecls hsModule

     return $ map rdrNameToName names

  showModuleName :: Module -> String
  showModuleName = moduleNameString . moduleName

  getExistingModuleNames :: GhcMonad m => Module -> m [TH.Name]
  getExistingModuleNames modl = do
    moduleInfo <- getModuleInfo modl
    case moduleInfo of
      Nothing -> error $ "modulespection: Failed to find module info for "
                       <> showModuleName modl
                       <> " in getExistingModuleNames"
      Just mod_info -> fmap (map (occNameToName . nameOccName . getName))
                    .  mapMaybeM lookupName
                    $  modInfoExports mod_info

  -- | Simple Class for getting the name of things
  class GetNameMaybe a where
     getNameMaybe :: a -> Maybe RdrName

  instance GetNameMaybe (HsDecl RdrName) where
     getNameMaybe = \case
        TyClD x -> getNameMaybe x
        HsDecls.ValD  x -> getNameMaybe x
        _           -> Nothing

  instance GetNameMaybe (TyClDecl RdrName) where
     getNameMaybe = \case
        FamDecl x -> getNameMaybe $ fdLName x
        SynDecl  { tcdLName } -> getNameMaybe tcdLName
        DataDecl { tcdLName } -> getNameMaybe tcdLName
        x@(ClassDecl {})  -> getNameMaybe $ tcdLName x

  instance GetNameMaybe (HsBindLR RdrName RdrName) where
     getNameMaybe = \case
        x@(FunBind {}) -> getNameMaybe $ fun_id x
        _                  -> Nothing

  instance GetNameMaybe a => GetNameMaybe (GenLocated SrcSpan a) where
     getNameMaybe (L _ x) = getNameMaybe x

  instance GetNameMaybe RdrName where
     getNameMaybe = Just

  -- Name Utils
  occNameToName :: OccName -> TH.Name
  occNameToName = mkName . occNameString

  rdrNameToName :: RdrName -> TH.Name
  rdrNameToName = \case
     RdrName.Unqual x -> occNameToName x
     RdrName.Qual _ x -> occNameToName x
     RdrName.Orig _ x -> occNameToName x
     RdrName.Exact  x -> occNameToName $ nameOccName x

  doCpp :: DynFlags -> Bool -> Bool -> FilePath -> FilePath -> IO ()
  doCpp dflags raw include_cc_opts input_fn output_fn = do
     let hscpp_opts = getOpts dflags opt_P
     let cmdline_include_paths = includePaths dflags

     pkg_include_dirs <- getPackageIncludePath dflags []
     let include_paths = foldr (\ x xs -> "-I" : x : xs) []
                           (cmdline_include_paths ++ pkg_include_dirs)

     let verbFlags = getVerbFlags dflags

     let cc_opts
           | include_cc_opts = getOpts dflags opt_c
           | otherwise       = []

     let cpp_prog args | raw       = SysTools.runCpp dflags args
                       | otherwise = SysTools.runCc dflags (SysTools.Option "-E" : args)

     let target_defs =
           [ "-D" ++ HOST_OS     ++ "_BUILD_OS=1",
             "-D" ++ HOST_ARCH   ++ "_BUILD_ARCH=1",
             "-D" ++ TARGET_OS   ++ "_HOST_OS=1",
             "-D" ++ TARGET_ARCH ++ "_HOST_ARCH=1" ]
       -- remember, in code we *compile*, the HOST is the same our TARGET,
       -- and BUILD is the same as our HOST.

     cpp_prog       (   map SysTools.Option verbFlags
                     ++ map SysTools.Option include_paths
                     ++ map SysTools.Option hsSourceCppOpts
                     ++ map SysTools.Option target_defs
                     ++ map SysTools.Option hscpp_opts
                     ++ map SysTools.Option cc_opts
                     ++ [ SysTools.Option     "-x"
                        , SysTools.Option     "c"
                        , SysTools.Option     input_fn
       -- We hackily use Option instead of FileOption here, so that the file
       -- name is not back-slashed on Windows.  cpp is capable of
       -- dealing with / in filenames, so it works fine.  Furthermore
       -- if we put in backslashes, cpp outputs #line directives
       -- with *double* backslashes.   And that in turn means that
       -- our error messages get double backslashes in them.
       -- In due course we should arrange that the lexer deals
       -- with these \\ escapes properly.
                        , SysTools.Option     "-o"
                        , SysTools.FileOption "" output_fn
                        ])

  hsSourceCppOpts :: [String]
  -- Default CPP defines in Haskell source
  hsSourceCppOpts =
         [ "-D__GLASGOW_HASKELL__="++cProjectVersionInt ]

