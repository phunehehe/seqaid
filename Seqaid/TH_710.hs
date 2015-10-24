
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 710
--- #warning With GHC 7.10, Seqaid.TH is still broken.
#endif

-- See what names from the module being spliced are in scope to TH.
#define DBG_NAMES 0

-- These types come from arguments to the seqaidTH splice.
#define DBG_TYPES 0

-- XXX A bit later yet: This could be handled completely
-- automatically (without needing to depend on seqaid.config
-- "exclude:" field or something), with new seqaidpp-calls-TH
-- idea ... it can see whether "seqaid" is called anywhere
-- in a bind(ing group)'s RHS(s), and decide on exclusion
-- that way...
-------
-- XXX Later: This can now be handled gracefully by seqaidpp
-- but this has not yet been implemented...
-------
-- I'd love to be able to do this, but so far as I can see, we
-- don't get to look at the RHS's of Dec's using reify Name,
-- only get the types... (So user has to add an Exclude ANN
-- as well as their manual seqaidDispatch injection, to any
-- bind which would be excluded by this TH code on other grounds,
-- such as [at present] being polymorphic.)
--- #define EXCLUDE_MANUALLY_INSTRUMENTED 1

-- Now the norm (and the ONLY way I could find to make
-- seqaid blanket harnessing feasible).
#define INJECT_DUMMY_CLASS_AND_INSTANCE_TO_BLOCK_DEAD_CODE_ELIMINATION 1

-- We want this to be 0, but at present the plugin is broken unless it's 1.
#define EXCLUDE_POLYMORPHIC_TYPES 1
--- #define EXCLUDE_MULTIPARAM_TYPES 0

-- You'll need this for some kinds of debugging...
#define SHOW_CONSTRAINT 0

{-# LANGUAGE TemplateHaskell #-}

-- XXX Some sites where I'm using this, should be using TH API to
-- issue errors and warnings...
----
-- Helpful with code tracing.
{-# LANGUAGE BangPatterns #-}

-------------------------------------------------------------------------------

-- |
-- Module      :  Seqaid.TH_710
-- Copyright   :  Andrew G. Seniuk 2014-2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Andrew Seniuk <rasfar@gmail.com>
-- Stability   :  provisional
-- Portability :  GHC (TH)
--
-- /NOTE: This is a development branch (probably broken) of "Seqaid.TH" for GHC 7.10./
--
-- Template Haskell parts of seqaid.
--
-- None of these splices needs to (or should) be called by the user.

  module Seqaid.TH_710 (

#if __GLASGOW_HASKELL__ >= 710

    seqaidTH ,

    seqaidValidate ,

--  seqaidSOP ,

--  seqaidManTH ,

--  seqaidInstancesTH ,

    strInstancesTH ,

    bindsIncludedTH ,

#if TRY_INJECT_NOINLINE_ON_REQUESTED_BINDS
    noinlineTH ,
#endif

    module Seqaid.Runtime ,
    module Seqaid.Ann ,

#endif

  ) where

#if __GLASGOW_HASKELL__ >= 710

  import Language.Haskell.TH
  import Language.Haskell.TH.Syntax ( qLocation )
#if SEQAIDPP_TYPES
  -- actually we use it in both cases, if you want to still
  -- do the top-level RHS wrapping technique; but I'm headed
  -- towards more general type-based subexpression wrapping,
  -- now that the preprocessor has made that possible...
  --   Actually, I suppose TH could be fed a config file, too...
  -- Consider porting all the config-file oriented stuff there?
  -- seqaidpp would only handle injection of static content.
  -- But no, it won't work, we need to know the types in the prepro
  -- at least to create the "deriving instance" declarations.
  import Seqaid.TH_extra ( names )
#else
  import Seqaid.TH_extra ( names )
#endif

  import Data.Generics ( listify, everywhere, mkT )

#if SEQABLE_ONLY
  import Generics.SOP.Universe ( Generic )
#endif

#if NFDATAN_ONLY
  import Control.DeepSeq.Bounded ( NFDataN )
#else
  import Control.DeepSeq.Bounded ( NFDataP )
#endif

  import Seqaid.Runtime ( seqaidDispatch, SiteID )
--import Seqaid.Runtime ( seqaid_internal )
  import Seqaid.Ann

  import Data.Maybe
  import Data.Either
  import Data.List ( intercalate, nub, nubBy )
  import Control.Monad ( liftM, zipWithM, foldM )

#if 0
  import qualified GHC.Environment as GHC ( getFullArgs )
#endif

  -- For global Bool state, to indicate that a warning has already been issued.
  import Data.IORef
  import System.IO.Unsafe ( unsafePerformIO )

  -- Regex only used to beautify warning messages.
  import Text.Regex.PCRE
  import Data.Array ( (!) )
  import Data.Array ( indices )

  import Debug.Trace ( trace )

  import qualified Type as GHC ( Type )

-------------------------------------------------------------------------------

  firstWarningPassed :: IORef Bool
  firstWarningPassed = unsafePerformIO $ newIORef False

-------------------------------------------------------------------------------

  seqaidInstancesTH :: Q [Dec]
  seqaidInstancesTH = do
#if SEQABLE_ONLY
    is <- getInstances ''Generics.SOP.Universe.Generic
#else
#if NFDATAN_ONLY
    is <- getInstances ''NFDataN
#else
    is <- getInstances ''NFDataP
#endif
#endif
    dss <- instancesToSeqinjDecls is
    let ds = concat dss
    return ds

-------------------------------------------------------------------------------

  strInstancesTH :: Q [Dec]
  strInstancesTH = do
#if SEQABLE_ONLY
    is <- getInstances ''Generics.SOP.Universe.Generic
#else
#if NFDATAN_ONLY
    is <- getInstances ''NFDataN
#else
    is <- getInstances ''NFDataP
#endif
#endif
    ss <- instancesToTypeStrings is
#if 1
    exp <- [e| SeqaidAnnAvailableInstances ss |]
--  runIO $ putStrLn $ " :|: exp =\n" ++ pprint exp
    let pragma_decl = PragmaD (AnnP ModuleAnnotation exp)
    return [pragma_decl]
#else
-- > runQ [d| ss = [ "a", "bc" ] |]
-- [ValD (VarP ss_0) (NormalB (ListE [LitE (StringL "a"),LitE (StringL "bc")])) []]
    let lites = map (\x -> LitE (StringL x)) ss
    vp <- newName "seqaid_instance_strings"
    let dec = ValD (VarP vp) (NormalB (ListE lites)) []
    return [dec]
#endif

-------------------------------------------------------------------------------

  bindsIncludedTH :: [String] -> Q [Dec]
  bindsIncludedTH bns = do
    exp <- [e| SeqaidAnnBindsIncluded bns |]
--  runIO $ putStrLn $ " :|: exp =\n" ++ pprint exp
    let pragma_decl = PragmaD (AnnP ModuleAnnotation exp)
    return [pragma_decl]

-------------------------------------------------------------------------------

  -- (And this doesn't even require th-expand-syns.)
  seqaidValidate :: [Name] -> Q [Dec]
  seqaidValidate names = do
    infos' <- mapM reify names
    infos <- mapM seqaidValidate' infos'  -- but will it get forced?!?
                       -- also, this could be quite expensive in worst case!
--  runIO $! evaluate (force infos)  -- I'm doubtful of an instance...
-- No instance.
-- Maybe we'll get lucky and TH is strict in this part??...
    return []
   where
    seqaidValidate' :: Info -> Q Info
    seqaidValidate' t = do
      case t of
        TyConI (TySynD name tyVarBndrs typ) -> error $ "seqaidpp: type synonym in instances list (seqaid.config):\n        " ++ pprint name
--      TyConI (TySynD name tyVarBndrs typ) -> error $ "seqaidpp: type synonym in instances list (seqaid.config):\n" ++ pprint t
        _ -> return t

-------------------------------------------------------------------------------

-- Always takes the seqaidpp-provided Type list now, even if
-- it does nothing with them for some reason (eg. leaky FS=4).
#if 1 || SEQAIDPP_TYPES
#if 1
  -- XXX To do this, I'm injecting [q| |] quasis [?] as argument
  -- in the module being spliced. (Injection by seqaidpp.)
  -- ... XXX ... except that doesn't quite work, because
  -- [t| etc. are in the Q monad. So we need to inject a bit
  -- more TH (slippery slope!) -- or is it even possible?...
#if 1
  -- After: And even better haha! Well, this works in the
  -- standalone test, so hang on...
  seqaidTH :: [Q Type] -> Q [Dec]
  seqaidTH types_actions = do
#else
  seqaidTH :: [Type] -> Q [Dec]
  seqaidTH types = do
#endif
#else
  seqaidTH :: [String] -> Q [Dec]
  seqaidTH type_strs = do
#endif
#else
  seqaidTH :: Q [Dec]
  seqaidTH = do
#endif
#if SEQAIDPP_TYPES
    types <- mapM runQ types_actions
#else
    let types = []
#endif
    modname <- fmap loc_module qLocation
    runIO $ putStrLn $ "Included in seqaid harness: " ++ modname
--  runIO $ putStrLn $ "Included in seqaid harness: " ++ dropQuals modname
    do
#if 0
    fullargs <- runIO $ GHC.getFullArgs
--  runIO $ putStrLn $ intercalate "\n" fullargs
--  error "STOP"
{-
-fplugin=Seqaid.Plugin
-fplugin-opt=Seqaid.Plugin:S03
-}
--  runIO $ putStrLn $ show modname
--  error "STOP"
--  modname <- fmap loc_module qLocation >>= \mod -> return (LitE (StringL mod))
--  Module pkgname modname <- reifyModule thisModule
  -- This prevents splicing anything in, unless the plugin is active,
  -- in in particular, is active for the module we're compiling.
    if not $    elem "-fplugin=Seqaid.Plugin" fullargs
             && elem ("-fplugin-opt=Seqaid.Plugin:" ++ modname) fullargs
    then return []
    else do
#endif

#if 1 || SEQAIDPP_TYPES
#if 0
#if 0
     Just types_name <- lookupValueName "seqaid_types"
     types <- reify types_name
--   types <- [d| seqaid_types |]
#endif
     types <- mapM (liftM fromJust . lookupTypeName) type_strs
#else
     let type_strs = map pprint types
#endif
#endif

-- XXX We would like to pass [Type.Type] in the SeqaidAnnTypes ANN.
-- What I can't figure out is how to get Type.Type from TH.Type.
#if 1 || SEQAIDPP_TYPES
#if TH_TYPE_IN_TYPES_ANN
-- I whish this had worked out, but I don't see how to do it...
#if 0
#elif 1
     ghc_types <- $(types)
     sats_exp <- [e| SeqaidAnnTypes ghc_types |]
#elif 0
     sats_exp <- [e| SeqaidAnnTypes $(types) |]
#elif 0
     let ghc_types = map (\x -> $(x)) types
     sats_exp <- [e| SeqaidAnnTypes ghc_types |]
#endif
#else
     sats_exp <- [e| SeqaidAnnTypes type_strs |]
#endif
#if DBG_TYPES
     runIO $ putStrLn $ " :|: sats_exp =\n" ++ pprint sats_exp
#endif
     let sats_pragma_decl = PragmaD (AnnP ModuleAnnotation sats_exp)
#endif

-- XXX Trying to still use the IncludeList, even for blanket
-- by-types wrapping in Core, since want to be able to distinguish
-- synthetic and original binds.
#if INFER_TOP_LEVEL_TYPES
     ns <- names :: Q [Name]
--makeNameType :: Either Name Type -> Q (Either Name (Maybe Name,Type))
     ents_names <- mapM ( makeNameType . (\x->Left x) ) ns :: Q [Either Name (Maybe Name,Type)]
--   ents_names <- mapM makeNameType ns  -- :: Q [Either Name (Name,Type)]
#if DBG_NAMES
     runIO $ putStrLn $ "Top-level names in scope:\n" ++ show ns
#endif
#else
     let ents_names = []
#endif

#if SEQAIDPP_TYPES
     -- This is returning Left's, but should be Right (Nothing,ty):
     ents_types <- mapM ( makeNameType_types . (\x->Right x) ) types :: Q [Either Name (Maybe Name,Type)]
#else
     let ents_types = []
#endif

#if DBG_TYPES
     runIO $ putStrLn $ " !! length ents_names=" ++ (show $ length ents_names)
     runIO $ putStrLn $ " !! length ents_types=" ++ (show $ length ents_types)
#endif

     let ents = ents_names ++ ents_types

#if DBG_TYPES || DBG_NAMES
     runIO $ putStrLn $ show ents
#endif
     let nts = rights ents

     if null nts
     then do
#if DBG_NAMES
      runIO $ putStrLn $ "modulespection:names = []"
#endif
      return []
     else do
#if DBG_NAMES
      let ss = map (\ (n,t) -> pprint n ++ " :: " ++ pprint t) nts :: [String]
--    let ss = map pprint ns :: [String]
      let ss' = intercalate "\n  " ss
      runIO $ putStrLn $ "modulespection:names = \n  " ++ ss'
--    reportWarning ss'
#endif
      sidtyp <- [t| SiteID |]
      ([ann]:dss') <- manifestSeqinjDecls sidtyp nts
--    runIO $ putStrLn $ show $ map length dss'
      let dss = dss'
--    let dss = map reverse dss'
#if INJECT_DUMMY_CLASS_AND_INSTANCE_TO_BLOCK_DEAD_CODE_ELIMINATION
      (clsd:instd:[]) <- manifestDummyClassAndInstance sidtyp dss
#endif

--    error "STOP"

      -- Pattern of decls is: Anns, TypeSig, Binding, TypeSig, Binding, ...
#if SEQAIDPP_TYPES
      let ds = sats_pragma_decl : ann : clsd : instd : reverse (concat $ map reverse dss)
#else
#if INJECT_DUMMY_CLASS_AND_INSTANCE_TO_BLOCK_DEAD_CODE_ELIMINATION
      let ds = ann : clsd : instd : reverse (concat $ map reverse dss)
#else
      let ds = ann : reverse (concat $ map reverse dss)
#endif
#endif

      return ds

-------------------------------------------------------------------------------

  instancesToSeqinjDecls :: [InstanceDec] -> Q [[Dec]]
  instancesToSeqinjDecls ids = zipWithM instancesToSeqinjDecl' [0,1..] ids
   where
    instancesToSeqinjDecl' :: Int -> InstanceDec -> Q [Dec]
    instancesToSeqinjDecl' idx (InstanceD ctx t ds) = do
     let (AppT _ t2) = t  -- (AppT (AppT ArrowT t2) t2)
     (seqinj_dec:_) <- [d| seqinj = seqaidDispatch :: () -> () |]  -- inefficient
--   (seqinj_dec:_) <- [d| seqinj = forcep "#" :: () -> () |]  -- inefficient
     let (ValD (VarP vp1) (NormalB (SigE ae1 (AppT (AppT ArrowT _) _))) [])
          = seqinj_dec
#if 0
     let vp1' = vp1
#else
     let vp1s = show vp1
     let (vp1s1,vp1s2) = break (=='_') vp1s
     let vp1s' = "seqinjinst_" ++ show idx
--   let vp1s' = "seqinjinst" ++ vp1s2
     vp1' <- newName vp1s'
--   let vp1' = mkName vp1s'
#endif
-- > runQ [d| seqinj_blah = forcep "#" |]
-- [ValD (VarP seqinj_blah_0) (NormalB (AppE (VarE Control.DeepSeq.Bounded.NFDataP.forcep) (LitE (StringL "#")))) []]
--  vs.
-- > runQ [d| seqinj_blah = forcep "#" :: Float -> Float |]
-- [ValD (VarP seqinj_blah_1) (NormalB (SigE (AppE (VarE Control.DeepSeq.Bounded.NFDataP.forcep) (LitE (StringL "#"))) (AppT (AppT ArrowT (ConT GHC.Types.Float)) (ConT GHC.Types.Float)))) []]
     let free_tvars_t2 = getFreeTVars t2
     let bind_tvars_t2 = map bindTVars free_tvars_t2
     ctx2 <- make_ctx Nothing free_tvars_t2 ctx
--   ForallT [TyVarBndr] Cxt Type
     let seqinj_tdec
          = SigD vp1' (ForallT bind_tvars_t2 ctx2 (AppT (AppT ArrowT t2) t2))
--        = SigD vp1' (ForallT free_tvars_t2 [] (AppT (AppT ArrowT t2) t2))
--        = SigD vp1' (AppT (AppT ArrowT t2) t2)
--   seqinj_tdec <- [d| $(vp1') :: $(t2) -> $(t2) |]
     let seqinj_fdec
          = (ValD (VarP vp1') (NormalB       ae1                            ) [])
--        = (ValD (VarP vp1') (NormalB       ae1                            ) [])
--        = (ValD (VarP vp1') (NormalB (SigE ae1 (AppT (AppT ArrowT t2) t2))) [])
--   return [seqinj_tdec]
     return [seqinj_tdec, seqinj_fdec]
--   return (seqinj_tdec, seqinj_fdec)

-------------------------------------------------------------------------------

  -- (Cloned from [ancient] instancesToSeqinjDecls. Probably doesn't
  -- need the zipping with unique Int's.)
  instancesToTypeStrings :: [InstanceDec] -> Q [String]
  instancesToTypeStrings ids = zipWithM instancesToTypeString' [0,1..] ids
   where
    instancesToTypeString' :: Int -> InstanceDec -> Q String
    instancesToTypeString' idx (InstanceD ctx t ds) = do
     let (AppT _ t2) = t  -- (AppT (AppT ArrowT t2) t2)
     return $ pprint t
---  -- (AppT (AppT ArrowT SiteID) (AppT (AppT ArrowT t2) t2))
---  let (AppT _ (AppT _ t2)) = t

-------------------------------------------------------------------------------

  bindTVars :: Type -> TyVarBndr  -- specifically, VarT ->
  bindTVars (VarT name) = PlainTV name
  bindTVars _ = error "bindTVars: unexpected!"

-------------------------------------------------------------------------------

  getFreeTVars :: Type -> [Type]  -- specifically, -> [VarT]
  getFreeTVars t = listify f t
   where
    f :: Type -> Bool
    f x@(VarT name) = True
    f x = False

-------------------------------------------------------------------------------

  -- Thanks to John L. in http://stackoverflow.com/a/5398910
  -- This was my introduction to TH.

  -- get a list of instances
  getInstances :: Name -> Q [InstanceDec]
--getInstances :: Name -> Q [ClassInstance]
  getInstances typ = do
    ClassI _ instances <- reify typ
    return instances

#if 0
  -- convert the list of instances into an Exp so they can be displayed in GHCi
  showInstances :: Name -> Q Exp
  showInstances typ = do
    ins <- getInstances typ
    return . LitE . stringL $ show ins
#endif

-------------------------------------------------------------------------------

-- XXX Okay, I've run into trouble already here.
-- Between Type and Name (and String).
-- We want the **Type** Types.TA (from the String "Types.TA").
-- But if reify . loopupTypeName, you get (which is cool and nice;
-- and is due to the fact that the types are imported) the full
-- declarations:
--   data Types.TA
--    = Types.A1 GHC.Types.Int | Types.A2 Types.TB GHC.Types.Int Types.TC
--   type Types.State = Types.TA
-- So I guess I could pattern-match on these and other type constructors,
-- and extract the types from it directly.  But can we not just avoid it???
--   lookupTypeName :: String -> Maybe Name
--   reify :: Name -> Decl
--   Name -> Q Info with VarI (eg. a function binding), TyConI (what
-- we are getting at present), ...
--   But can't we just say the equivalent of "typeOf Name"?
-------
-- XXX Decided to clone this (and decruft the copy, as usual),
-- since the type case has more than a few differences...

--makeNameType :: Either Name Name -> Q (Either Name (Maybe Name,Type))
  makeNameType :: Either Name Type -> Q (Either Name (Maybe Name,Type))
--makeNameType :: Either Name Type -> Q (Either Name (Name,Type))
--makeNameType :: Either Name Type -> Q (Maybe (Name,Type))

  makeNameType (Right _) = error "makeNameType: Right unexpected!"
  makeNameType enm@(Left nm) = do
   modname <- fmap loc_module qLocation
#if 0
   !_ <- trace ("nm="++pprint nm++" modname="++modname) $ return ()
   if False && takeQuals (pprint nm) /= modname
   then do
     !_ <- trace "HERE-1" $ return ()
     return $ Left nm
-- then return $ Left nm
   else do
#else
   do
#endif
--  Just nm <- lookupValueName nm_string
{-
data TH.Info =
 ClassI Dec [InstanceDec]
 ClassOpI Name Type ParentName Fixity
 TyConI Dec
 FamilyI Dec [InstanceDec]
 PrimTyConI Name Arity Unlifted
 DataConI Name Type ParentName Fixity
 VarI Name Type (Maybe Dec) Fixity
-}
#if 1
--- !_ <- trace "HERE-2" $ return ()
    rnm <- reify nm
--- !_ <- trace ("<><><><> " ++ pprint rnm) $ return ()
    let mnmt
         = case rnm of
-- XXX You might think it would be better to just OMIT all but VarI
-- types (gracefully) -- which I should have tried already, and will
-- certainly try in a moment ... But, probably, you'll NEED to deal
-- with things like DataConI's, since the associated types will
-- have to be in scope ... or ... no?...
              VarI nm_ t_ mdec_ fxty_ ->
                if takeQuals (show nm_) /= modname
                then Nothing
                else Just (nm_,t_)
#if 0
              -- XXX can probably scrounge a Name up in the 3 unimplemented ones
--            ClassI dec_ ideclst_ -> error "makeNameType: ClassI unimplemented"
              ClassOpI nm_ t_ parentnm_ fxty_ ->
                if takeQuals (show nm_) /= modname
                then Nothing else Just (nm_,t_)
--            TyConI dec_ -> error "makeNameType: TyConI unimplemented"
--            FamilyI dec_ ideclst_ -> error "makeNameType: FamilyI unimplemented"
--            PrimTyConI nm_ arty_ unlifted_ -> error "makeNameType: PrimTyConI unimplemented"
              DataConI nm_ t_ parentnm_ fxty_ ->
                if takeQuals (show nm_) /= modname
                then Nothing else Just (nm_,t_)
              _ -> Nothing
#else
              _ -> Nothing
#endif
#else
    rnm@(VarI nm_ t_ mdec_ fxty_) <- reify nm
#endif
    if isNothing mnmt
    then do
      return $ Left nm
--    return $ Nothing
    else do
      let Just (nm_,t_) = mnmt
#if 0
      let snm_ = pprint nm_
      let st_ = pprint t_
      runIO $ putStrLn $ " ********* " ++ snm_ ++ "  " ++ st_
#endif
      return $ Right (Just nm_,t_)
--    return $ Just (nm_,t_)

-------------------------------------------------------------------------------

--makeNameType_types :: Either Name Name -> Q (Either Name (Maybe Name,Type))
  makeNameType_types :: Either Name Type -> Q (Either Name (Maybe Name,Type))
--makeNameType_types :: Either Name Type -> Q (Either Name (Name,Type))
--makeNameType_types :: Either Name Type -> Q (Maybe (Name,Type))

  makeNameType_types (Left _) = error "makeNameType_types: Left unexpected!"

#if 1

  makeNameType_types enm@(Right typaboo) = do
    return $ Right (Nothing,typaboo)

#else

  makeNameType_types enm@(Right nm) = do
    modname <- fmap loc_module qLocation
--- !_ <- trace "HERE-2" $ return ()
    rnm <- reify nm
    !_ <- trace ("<><><><> " ++ pprint rnm) $ return ()
    let mnmt
         = case rnm of
              VarI nm_ t_ mdec_ fxty_ -> error "makeNameType_types: VarI unexpected!"
--            ClassI dec_ ideclst_ -> error "makeNameType_types: ClassI unimplemented"
---           TyConI dec_ -> error "makeNameType_types: TyConI unimplemented"
--            FamilyI dec_ ideclst_ -> error "makeNameType_types: FamilyI unimplemented"
--            PrimTyConI nm_ arty_ unlifted_ -> error "makeNameType_types: PrimTyConI unimplemented"
--            DataConI nm_ t_ parentnm_ fxty_ -> error "makeNameType_types: DataConI unimplemented"
-- DataD Cxt Name [TyVarBndr] [Con] [Name]
-- NewtypeD Cxt Name [TyVarBndr] Con [Name]
-- TySynD Name [TyVarBndr] Type

... unfinished ...

              TyConI dec_ -> case dec_ of
                DataD cxt name tyvarbndrs cons name ->
                NewtypeD Cxt Name [TyVarBndr] Con [Name]
                TySynD Name [TyVarBndr] Type
              _ -> Nothing
    if isNothing mnmt
    then do
      return $ Left nm
--    return $ Nothing
    else do
      let Just (nm_,t_) = mnmt
#if 0
      let snm_ = pprint nm_
      let st_ = pprint t_
      runIO $ putStrLn $ " ********* " ++ snm_ ++ "  " ++ st_
#endif
      return $ Right (Just nm_,t_)
--    return $ Just (nm_,t_)

#endif

-------------------------------------------------------------------------------

  -- This is identical to produceSeqinjDecls, minus cruft,
  -- since wrote makeNameType instead...
  manifestSeqinjDecls :: Type -> [(Maybe Name,Type)] -> Q [[Dec]]
  manifestSeqinjDecls sidtyp nts = do

    mod <- thisModule
    modname <- fmap loc_module qLocation  -- really?? can't get this from mod?!

    reiannbad <- reifyAnnotations (AnnLookupModule mod) :: Q [SeqaidAnnIncludeList]
    if not $ null reiannbad
    then do
     error $ "seqaid: illegal SeqaidAnnIncludeList annotation"
    else do

     reiann <- reifyAnnotations (AnnLookupModule mod) :: Q [SeqaidAnnExclude]
--   runIO $ putStrLn $ "--------------\n" ++ show reiann ++ "---------\n"
--ValueAnnotation
     -- Collect existing ANN 's to initialise the lists of excluded name strings:
     let zlst' = map (\ (SeqaidAnnExclude s) -> s) reiann
     -- Make sure all the name strings fully-qualified:
     let zlst'' = map (\ s -> if elem '.' s then s else modname ++ ('.':s)) zlst'
     let zlst = zlst''

-- XXX I do confess I'm having a hard time following this...

     (injdecls,excorincnms) <- liftM fst $
       ( foldM
           ( \ y@((injdecls_, excorincnms_), yidx) x ->
               do
                  eideclst <- manifestSeqinjDecl' zlst yidx x
                  case eideclst of
                   Left nm -> return ((injdecls_, excorincnms_), yidx)
#if 1
                   Right d -> case fst x of
                               Nothing -> return ((d:injdecls_, excorincnms_), 1+yidx)
                               Just nm -> return ((d:injdecls_, nm:excorincnms_), 1+yidx)
#else
                   Right d -> let nm = fst x in
                              return ((d:injdecls_, nm:excorincnms_), 1+yidx)
#endif
           )
           (([],[]),0)
         :: [(Maybe Name,Type)] -> Q (([[Dec]],[Name]),Int) )  -- type up to here
---      :: [(Name,Type)] -> Q (([[Dec]],[Name]),Int) )  -- type up to here
           nts

--   runIO $ putStrLn $ " :|: injdecls =\n" ++ pprint injdecls
--   runIO $ putStrLn $ " :|: excorincnms =\n" ++ pprint excorincnms
--   error "STOP"

-- Get all the binds explicitly excluded by ANN pragmas already
-- present in the input source module:

-- Get all the binds excluded based on as-yet unsupported types:
--   let exp = LitE (StringL "Boo!")
     let ss = map pprint excorincnms

     let ss' = ss
--   let ss' = ss ++ zlst
--   runIO $ putStrLn $ " :|: ss' = " ++ intercalate "\n" ss'
--   runIO $ putStrLn $ " :|: zlst = " ++ intercalate "\n" zlst
--   runIO $ putStrLn $ " :|: ss = " ++ intercalate "\n" ss

     exp <- [e| SeqaidAnnIncludeList ss' |]
--   runIO $ putStrLn $ " :|: exp =\n" ++ pprint exp

     let pragma_decl = PragmaD (AnnP ModuleAnnotation exp)

     return ([pragma_decl] : injdecls)
--   manifestSeqinjDecls nts = liftM catMaybes $ zipWithM manifestSeqinjDecl' [0,1..] nts
   where
    manifestSeqinjDecl' :: [String] -> Int -> (Maybe Name,Type) -> Q (Either Name [Dec])
--  manifestSeqinjDecl' :: [String] -> Int -> (Name,Type) -> Q (Either Name [Dec])
    manifestSeqinjDecl' zlst idx (mnm,t) = do
#if 0
data Type = ForallT [TyVarBndr] Cxt Type --- <long comment elided>
          | AppT Type Type               --- @T a b@
          | SigT Type Kind               --- @t :: k@
          | VarT Name                    --- @a@
          | ConT Name                    --- @T@
          | PromotedT Name               --- @'T@
          -- See Note [Representing concrete syntax in types]
          | TupleT Int                   --- @(,), (,,), etc.@
          | UnboxedTupleT Int            --- @(#,#), (#,,#), etc.@
          | ArrowT                       --- @->@
          | ListT                        --- @[]@
          | PromotedTupleT Int           --- @'(), '(,), '(,,), etc.@
          | PromotedNilT                 --- @'[]@
          | PromotedConsT                --- @(':)@
          | StarT                        --- @*@
          | ConstraintT                  --- @Constraint@
          | LitT TyLit                   --- @0,1,2, etc.@
#endif
     let st = pprint t
     let nm = fromJust mnm
     let snm = pprint nm
#if 0
     runIO $ putStrLn $ "mnm = " ++ show mnm ++ "\nst = " ++ st
--   runIO $ putStrLn $ "snm = " ++ snm ++ "\nst = " ++ st
     runIO $ putStrLn $ "zlst = " ++ show zlst
--   error "HERE!"
#endif
     if isJust mnm && elem snm zlst
--   if elem snm zlst
     then do {- runIO (putStrLn "YAY!!!!!") >> -} return (Left nm)
     else do
#if ! EXCLUDE_POLYMORPHIC_TYPES
--- #if ! EXCLUDE_MULTIPARAM_TYPES
      let t2 = t
      do
#else
      mt2 <- case t of
              (ForallT _ _ _) -> do
               if isNothing mnm
               then return $ Just t
               else do
                firstwarnpassed <- runIO $ readIORef firstWarningPassed
                if firstwarnpassed
                then reportWarning $ "seqaid: omitting declaration from auto-harness:\n      " ++ snm ++ " :: " ++ beautify st ++ "\n    Polymorphic types not yet supported by the plugin."
                else reportWarning $ "seqaid: omitting declaration from auto-harness:\n      " ++ snm ++ " :: " ++ beautify st ++ "\n    Sorry, polymorphic types not yet supported by the plugin.\n    If you need this declaration to be included in the harness,\n    please manually instrument with \" seqaid $ \" at the front of the\n    RHS of its binding.  (For technical reasons, this warning will\n    still appear unless you also put\n      {-# ANN module (SeqaidAnnExclude \"" ++ dropQuals snm ++ "\") #-}\n    someplace in the same module; or just ignore the warning.)"
#if 0
                      ++ "  Pass seqaidDispatch a distinct, negative Int as\n    first argument.  (Negative keys are reserved for this eventuality.)"
#endif
                runIO $ modifyIORef' firstWarningPassed (const True)
                return Nothing
              _ -> return $ Just t
      if isNothing mt2 then return $ Left nm
      else do
       -- or is "resultType :: Type -> Type" in TH API?...
       let t2 = followArrows $ fromJust mt2  -- use result type
#endif
       (seqinj_dec:_) <- [d| seqinj = seqaidDispatch :: SiteID -> () -> () |]  -- inefficient
#if 0
       let (ValD (VarP vp1) (NormalB (SigE ae1 ( AppT (AppT ArrowT sidtyp) ( AppT (AppT ArrowT _) _))))) [])
            = seqinj_dec
#else
       let (ValD (VarP vp1) (NormalB (SigE ae1 (
                                                 AppT
                                                  (AppT ArrowT sidtyp)
                                                  ( AppT
                                                      (AppT ArrowT _)
                                                      _
                                                  )
                                               )
                                     )) [])
            = seqinj_dec
#endif
#if 0
                      ( AppT
                         (AppT ArrowT sidtyp)
                         ( AppT
                             (AppT ArrowT typ_)
                             typ_
                         )
                      )
#endif
       let vp1s = show vp1
       let (vp1s1,vp1s2) = break (=='_') vp1s
       let vp1s' = "seqinj_" ++ show idx
       vp1' <- newName vp1s'
#if 1
       t' <- {-trace "Boo-hoo" $-} makeSeqinjType Nothing sidtyp t2
       let seqinj_tdec = SigD vp1' t'
--     let seqinj_tdec = trace ("YES: "++show t') $ SigD vp1' t'
#else
       let seqinj_tdec
            = SigD vp1' (AppT (AppT ArrowT t2) t2)
#endif
-- > runQ [t| forall a. a -> a |]
-- ForallT [PlainTV a_2] [] (AppT (AppT ArrowT (VarT a_2)) (VarT a_2))
-- > runQ [t| forall a b. (a,b) -> (a,b) |]
-- ForallT [PlainTV a_0,PlainTV b_1] [] (AppT (AppT ArrowT (AppT (AppT (TupleT 2) (VarT a_0)) (VarT b_1))) (AppT (AppT (TupleT 2) (VarT a_0)) (VarT b_1)))
       let seqinj_fdec
            = ValD (VarP vp1') (NormalB ae1) []
       return $ Right [seqinj_tdec, seqinj_fdec]

-------------------------------------------------------------------------------

  -- Added this to correctly build a seqinj_[0-9]* type from
  -- a given forall type, also adding the necessary qualifiers:
  -- This is like the 3rd function cloned off of instancesToSeqinjDecl
  -- already, isn't it??  But this is the first to preserve (let alone
  -- focus on) the forall stuff...
  -- XXX This should be called from any places which are creating t -> t...
  -- XXX XXX XXX Forgot! Still need to make newName's -- the only
  -- type var Name that can be reused in here is Maybe Name argument...
  -- XXX Also, we are getting duplicate vars in our synthesised forall binders,
  -- simply need to nub.
-- XXX This is wretched at the moment:
--  When isNothing mn, assume tt is NOT yet in t -> t form, and do it.
--  When isJust mn, tt IS in t -> t form (but we have some other stuff to do).
  makeSeqinjType :: Maybe Name -> Type -> Type -> Q Type
  makeSeqinjType mn sidtyp tt@(ForallT tyvarbndr_lst ctx typ) = do
--   runIO $ putStrLn $ " >FORALL> " ++ pprint tt
     let n = fromJust mn
     let free_tvars_typ' = getFreeTVars typ
-- Next day: I don't like this approach; just send the class typevar
-- if there is one (Just vs. Nothing), but assure the rest of the
-- Type does not start with that var. The type arg should also
-- not yet have the duplication, obviously (i.e. t ==> (t -> t)) -- this
-- is part of the job of THIS function!
#if 1
     let free_tvars_typ = free_tvars_typ'
#else
     let free_tvars_typ
          | isNothing mb       = free_tvars_typ'
          | PlainTV n <- b     = filter (==(VarT n)) free_tvars_typ'
          | KindedTV n k <- b  = filter (==(VarT n)) free_tvars_typ'
#endif
#if 1
     let blah1 = map (\ (VarT n) -> show n) (free_tvars_typ :: [Type]) :: [String]
     let blah2 = nub blah1 :: [String]
     blah3 <- mapM newName (map (take 1) blah2) :: Q [Name]
--   blah3 <- mapM newName (take (length blah2) (repeat "a")) :: Q [Name]
--   blah3 <- mapM newName blah2 :: Q [Name]
     blah4 <- mapM (\ n -> return $ VarT n) blah3 :: Q [Type]
     let bind_tvars_typ = blah4
--   let new_tvars_typ = blah4
     let orig_tyvars = nub free_tvars_typ
     let fresh_tyvars = bind_tvars_typ
#else
     new_tvars_typ
      <- fmap (\ n -> return $ VarT n) $
         (( mapM newName $
         (( nub $
           (( map (\ (VarT n) -> show n) (free_tvars_typ :: [Type])) :: [String] ) ) :: [String] )
         :: Q [Name]))
--   new_tvars_typ <- mapM (\ n -> return $ VarT n) $ mapM newName $ nub $ map (\ (VarT n) -> show n) free_tvars_typ
#endif
     let binding = map bindTVars bind_tvars_typ
-- ForallT [TyVarBndr] Cxt Type
     let (AppT _ typ_) = typ  -- you can do this, but it does feel sloppy
                  -- to have pattern bindings which you know are inapplicable
                  -- except when those patterns arise (i.e. they should
                  -- be localised to branches of conditionals, except
                  -- conditional branching is such a pain with monads...
--   let (AppT (AppT ArrowT typ_) typ_) = typ
     let typ_' = substTyVars (orig_tyvars,fresh_tyvars) typ_
     ctx2 <- make_ctx (Just (orig_tyvars,fresh_tyvars)) bind_tvars_typ ctx
     let typ' | isNothing mn  -- it's not a class method signature
                  = let typ_ = substTyVars (orig_tyvars,fresh_tyvars) typ
                    in
                    ForallT
                      binding
                      ctx2
                      ( {-trace "BOO-1" $-}
                      ( AppT
                         (AppT ArrowT sidtyp)
                         ( AppT
                             (AppT ArrowT typ_)
                             typ_
                         )
                      )
                      )
---                   (AppT (AppT ArrowT (AppT ArrowT sidtyp) typ_) typ_)
              | otherwise  -- then it's a class method signature
                  = let (AppT _ typ_) = typ
                        typ_' = substTyVars (orig_tyvars,fresh_tyvars) typ_
                    in
                    ForallT
                      binding
                      ctx2
#if 0
                      (AppT (AppT ArrowT (VarT n))
                        (AppT (AppT ArrowT typ_') typ_')
                        )
#else
#if 0
                      ( trace "BOO-2" $
                      ( AppT
                         (AppT ArrowT (VarT n))
                         ( AppT
                             (AppT ArrowT sidtyp)
                             typ_'
                         )
                      )
                      )
#else
                      (AppT (AppT ArrowT (VarT n)) typ_')
#endif
#endif
     return typ'
  makeSeqinjType mn sidtyp tt@typ = do
     let n = fromJust mn
     let free_tvars_typ = getFreeTVars typ
     if null free_tvars_typ
     then do
--    runIO $ putStrLn $ " >OTHER:null> " ++ pprint tt
      let typ' | isNothing mn
               = ( AppT
                    (AppT ArrowT sidtyp)
                    ( AppT
                       (AppT ArrowT typ)
                       typ
                    )
                 )
               | otherwise
#if 0
               = AppT
                   (AppT ArrowT (VarT n))
                   (AppT (AppT ArrowT typ) typ)
#else
#if 0
-- > runQ [t| Int -> Bool -> Float |]
-- AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (AppT ArrowT (ConT GHC.Types.Bool)) (ConT GHC.Types.Float))
-- > runQ [t| t1 -> t2 -> t3 |]  -- pseudo
-- AppT (AppT ArrowT t1) (AppT (AppT ArrowT t2) t3)
               = trace "BOO-3" $ ( AppT
                    (AppT ArrowT (VarT n))
                    ( AppT
                        (AppT ArrowT sidtyp)
                        typ
                    )
                 )
#else
               = (AppT (AppT ArrowT (VarT n)) typ)
#endif
#endif
      return typ'
     else do
#if 1
      error $ "makeSeqinjType: Nothing and free vars"
#else
      runIO $ putStrLn $ " >OTHER:non-null> " ++ pprint tt
      let bind_tvars_typ = map bindTVars free_tvars_typ
      ctx <- make_ctx (Just ([],[])) free_tvars_typ []
      let typ' = ForallT
                   bind_tvars_typ
                   ctx
                   (AppT (AppT ArrowT typ) typ)
      let typ' = AppT (AppT ArrowT typ) typ
      return typ'
#endif
--makeSeqinjType (Just n) tt@typ = do
--  error $ "makeSeqinjType: Just " ++ pprint n ++ " and typ not forall"

-------------------------------------------------------------------------------

  -- Humph, looks like I need SYB? I don't see utility functions
  -- for doing subsitutions inside general Type's.
  substTyVars :: ([Type],[Type]) -> Type -> Type
  substTyVars (orig,fresh) t = everywhere (mkT fg) t
   where
    fg :: Type -> Type
    fg (VarT n)
     | isNothing mfn  = error "substTyVars: lookup failed!"
     | otherwise      = VarT $ fromJust mfn
     where
      mfn = h n orig fresh
      h :: Name -> [Type] -> [Type] -> Maybe Name
      h n [] _ = Nothing
      h n ((VarT o):os) ((VarT f):fs)
-- XXX why am I never seeing this traceline?
       | n == o     = trace (show o ++ " ----->> " ++ show f) $ Just f
       | otherwise  = h n os fs
    fg x = {-trace (" %%% " ++ show x)-} x

-------------------------------------------------------------------------------

  make_ctx :: Maybe ([Type],[Type]) -> [Type] -> [Pred] -> Q [Pred]
  make_ctx mof free_tvars_typ ctx = do
    let class_name_lst
         =    []
#if SEQABLE_ONLY
           ++ [name_SOP_Generic]
#else
#if NFDATAN_ONLY
           ++ [name_NFDataN]
#else
           ++ [name_Typeable]
           ++ [name_NFDataN]
           ++ [name_NFData]
           ++ [name_NFDataP]
#endif
#endif
#if SHOW_CONSTRAINT
           ++ [name_Show]
#endif

    ctx2 <- mapM varT class_name_lst
    return ctx2
     where
      -- This is to avoid duplication (perhaps ironically...):
#if 1
#warning At 2429376459103765 in TH_710.hs, this is a kludge!
      ctx_ | isNothing mof  = ctx
           | otherwise      = ctx
--         | otherwise      = map j ctx
#else
#if __GLASGOW_HASKELL__ < 710
#if 0
      j p | ClassP n ts <- p   = ClassP n $ map (substTyVars (orig,fresh)) ts
          | EqualP t1 t2 <- p  = EqualP ((substTyVars (orig,fresh)) t1) ((substTyVars (orig,fresh)) t1)
--        | EqualP t1 t2 <- p  = error "make_ctx: not ready for EqualP"
#endif
#else
      j p | ForallT ts ((VarT n):_) _ <- p   = forallT (map (substTyVars (orig,fresh)) ts) (varT n)
 --       | EqualP t1 t2 <- p  = EqualP ((substTyVars (orig,fresh)) t1) ((substTyVars (orig,fresh)) t1)
--        | EqualP t1 t2 <- p  = error "make_ctx: not ready for EqualP"
#endif
      ctx_ | isNothing mof  = ctx
           | otherwise      = map j ctx
#endif
      Just (orig,fresh) = mof  -- antipattern...
      !_ = trace (show ctx_) $ ()
      ctx' = (
               id
#if SEQABLE_ONLY
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "Generic")
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "Generics.SOP.Universe.Generic")
#if SHOW_TYPE
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "Typeable")
#endif
#else
#if NFDATAN_ONLY
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "NFDataN")
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "Control.DeepSeq.Bounded.NFDataN.NFDataN")
#if SHOW_TYPE
-- (recently added; and untested; but I think is supposed to be here?)
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "Typeable")
#endif
#else
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "Typeable")
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "NFDataN")
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "NFData")
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "NFDataP")
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "Data.Typeable.Internal.Typeable")
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "Control.DeepSeq.Bounded.NFDataN.NFDataN")
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "Control.DeepSeq.NFData")
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "Control.DeepSeq.Bounded.NFDataP.NFDataP")
#endif
#endif
#if SHOW_CONSTRAINT
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "Show")
             . filter (\ (ForallT _ ((VarT name):_) _) -> show name /= "GHC.Show.Show")
#endif
             )
             ctx_
      name_NFDataP = mkName "NFDataP"
      name_NFDataN = mkName "NFDataN"
      name_NFData = mkName "NFData"
      name_Typeable = mkName "Typeable"
      name_Show = mkName "Show"
      name_SOP_Generic = mkName "Generics.SOP.Universe.Generic"

-------------------------------------------------------------------------------

  showNTs :: [(Name,Type)] -> String
  showNTs nts = ss'
   where
    ss = map (\ (n,t) -> pprint n ++ " :: " ++ pprint t) nts
    ss' = intercalate "\n" ss

-------------------------------------------------------------------------------

  followArrows :: Type -> Type
  followArrows (AppT (AppT ArrowT t1) t2) = followArrows t2
---followArrows (ArrowT t1 t2) = followArrows t2
  followArrows t = t

-------------------------------------------------------------------------------

#if INJECT_DUMMY_CLASS_AND_INSTANCE_TO_BLOCK_DEAD_CODE_ELIMINATION

-- We have already run "dss <- manifestSeqinjDecls nts" so we
-- can assume full benefit of that information here.
--
-- What we need to build:
--
--  - a single class declaration
--     - which contains one method signature declaration per seqinj_[0-9]*
--       signature that was prepared by manifestSeqinjDecls
--
--          class SeqinjDummyClass a where
--            seqinj_class_0 :: a -> Int -> Int
--            ...
--
--  - and a single instance declaration (instancing this class)
--     - which contains one method definition declaration to correspond
--       with each signature in the class declaration
--
--          instance SeqinjDummyClass () where
--            seqinj_class_0 _ x = seqinj_0 x
--            ...
--
-- > runQ [d| class Blah a where blah :: a -> Int -> Int |]
-- [ClassD [] Blah_0 [PlainTV a_2] [] [SigD blah_1 (AppT (AppT ArrowT (VarT a_2)) (AppT (AppT ArrowT (ConT GHC.Types.Int)) (ConT GHC.Types.Int)))]]
--
-- > runQ [d| instance Blah () where blah _ x = seqinj_0 x |]
-- [InstanceD [] (AppT (ConT Ghci1.Blah) (TupleT 0)) [FunD Ghci1.blah [Clause [WildP,VarP x_0] (NormalB (AppE (VarE seqinj_0_1627392795) (VarE x_0))) []]]]
--
-- Notes:
--  - I don't think we want to try to use the (Name,Type) pairs.
--  - rather, use the [[Dec]] -- not all Name's end up producing
--    Decl, remember, so these lists are not parallel! (We could
--    return an elided nts which IS parallel, upstream, but we didn't.)

-- XXX I think I need nested forall in a case like:
--   class SeqinjDummyClass a where
--     seqinj_meth_0 :: a -> Blob b -> Blob b
-- This will become more explicit in TH land; Blob b will actually be
-- (forall b. <ctx> => Blob b). And we do NOT want
--     seqinj_meth_0 :: a -> (forall b. <ctx> => Blob b) -> (forall b. <ctx> => Blob b)
-- we want
--     seqinj_meth_0 :: forall b. <ctx> => a -> Blob b -> Blob b
-- Note that the "a" variable takes its ctx from the class declaration,
-- and in fact it does not get a forall at all [?? right?]
--   So we DON'T need nested forall here (although we would if had
-- multiple variables with different contexts)...

-- XXX Also remember, we probably don't need the top-level seqinj_[0-9]*'s
-- at all now, with the class/instance -- but when build the Map in plugin,
-- will have to make a change to read the signatures in the class rather
-- than as now from the top-level seqinj_[0-9]*'s...
-- LATER: This turned out to be not the case: The top-level seqinj_[0-9]*'s
-- seem to need to be retained in the plugin output.

  manifestDummyClassAndInstance :: Type -> [[Dec]] -> Q [Dec]
  manifestDummyClassAndInstance sidtyp dss = do
--manifestDummyClassAndInstance :: [(Name,Type)] -> [[Dec]] -> Q [Dec]
--manifestDummyClassAndInstance (nt:nts) (ds:dss) = do
    modname <- fmap loc_module qLocation
    let modname' = map (\x -> if x == '.' then '_' else x) modname
    an <- newName "a"
    (sigs,defs) <- liftM fst $
      ( foldM
          ( \ y@((sigs_, defs_), i) dec ->
              do
                 -- Prise apart the Dec, tagging values for later reference
                 let [tdec, fdec] = dec
                 let (SigD vp_s t_s) = tdec
                 let (ValD (VarP vp_v) (NormalB ae_v) []) = fdec
                 mn <- newName $ "seqinj_meth_" ++ show i
#if 1
#if 1
                 t_s' <- makeSeqinjType (Just an) sidtyp t_s
#else
-- XXX no.
                 let t_s'' = AppT (AppT ArrowT (VarT an)) t_s
                 t_s' <- makeSeqinjType (Just an) t_s''
#endif
#else
                 let t_s' = AppT (AppT ArrowT (VarT tv)) t_s
#endif
                 let s = SigD mn t_s'
                 vv <- newName "x"  -- no matter if we [shadow/whatever]
                 sid <- newName "sid"  -- no matter if we [shadow/whatever]
                 let d = FunD
                           mn
                           [ Clause
                               [WildP, VarP sid, VarP vv]
                               (NormalB
                                 (AppE
-- To really never even build them, there's some more changes
-- needed in this modules...
#if NO_TOP_LEVEL_SEQINJ_DUMMIES
                                   (AppE (VarE $ mkName "Seqaid.Runtime.seqaidDispatch") (VarE sid))
#else
                                   (AppE (VarE vp_v) (VarE sid))
#endif
                                   (VarE vv)
                                 )
                               )
                               []
                           ]
--- sigs = [SigD mn typ]
--- typ = (AppT (AppT ArrowT (VarT tv)) (AppT (AppT ArrowT (ConT GHC.Types.Int)) (ConT GHC.Types.Int)))
-- defs = [FunD mn [Clause [WildP,VarP vv] (NormalB (AppE (VarE seqinj_0_1627392795) (VarE vv))) []]]
                 return ((s:sigs_, d:defs_), -1+i)
          )
          (([],[]),-1+length dss)
        :: [[Dec]] -> Q (([Dec],[Dec]),Int) )  -- type up to here
          dss
    cn <- newName $ "SeqinjDummyClass_" ++ modname'
--  cn <- newName "SeqinjDummyClass"
    let cdec = ClassD [] cn [PlainTV an] [] sigs
    let idec = InstanceD [] (AppT (ConT cn) (TupleT 0)) defs
    return [cdec, idec]

#endif

-------------------------------------------------------------------------------

  -- These are only for cosmetic purposes in warning messages.

  -- Cloned from Core.hs.
  -- This will work fine on module names, but don't try to
  -- use it on (String-ified) types!... (See caveat in Core.hs.)
  -- XXX how about nameBase and nameModule? (Oh, but that's Name not String...)
  dropQuals :: String -> String
  dropQuals = reverse . takeWhile (/= '.') . reverse

  takeQuals :: String -> String
  takeQuals = reverse . drop 1 . dropWhile (/= '.') . reverse

  -- XXX VERY BAD INDEED!!!
  beautify :: String -> String
  beautify s = s_
   where
    marr1 = (s =~ "^forall ") :: MatchArray
    (a1,b1) = (marr1!0)
    marr2 = (s =~ "=> ") :: MatchArray
    (a2,b2) = (marr2!0)
    s_ | null $ indices marr1  = s
       | null $ indices marr2  = s
       | otherwise
          =   dropWhile (\x -> x==' '||x=='\t'||x=='\n')
            $ drop (a2+b2) s

-------------------------------------------------------------------------------

#if TRY_INJECT_NOINLINE_ON_REQUESTED_BINDS
  noinlineTH :: [String] -> Q [Dec]
--noinlineTH :: [Name] -> Q [Dec]
  noinlineTH nms = do
    mapM
      (\x -> do Just nm <- lookupValueName x
                return $ PragmaD $ InlineP nm NoInline FunLike AllPhases)
      nms
--  mapM (\x -> do { (Just nm) <- lookupValueName x ; return $ PragmaD $ InlineP nm NoInline FunLike AllPhases }) nms
--  mapM (\x -> (liftM fromJust) lookupValueName x >>= \nm -> PragmaD $ InlineP nm NoInline FunLike AllPhases) nms  -- oh whatever!!
--  return $ map (\x -> PragmaD $ InlineP x NoInline FunLike AllPhases) nms
#endif

-------------------------------------------------------------------------------

#endif

