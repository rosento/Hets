{- |
Module      :  $Header$
Description :  static analysis of CASL specification libraries
Copyright   :  (c) Till Mossakowski, Uni Bremen 2002-2006
License     :  GPLv2 or higher, see LICENSE.txt
Maintainer  :  till@informatik.uni-bremen.de
Stability   :  provisional
Portability :  non-portable(Logic)

Static analysis of CASL specification libraries
   Follows the verification semantics sketched in Chap. IV:6
   of the CASL Reference Manual.
-}

module Static.AnalysisLibrary
    ( anaLibFileOrGetEnv
    , anaLibDefn
    , anaSourceFile
    , anaLibItem
    , anaViewDefn
    , LNS
    ) where

import Logic.Logic
import Logic.Grothendieck

import Syntax.AS_Structured
import Syntax.AS_Library

import Static.GTheory
import Static.DevGraph
import Static.DgUtils
import Static.ComputeTheory
import Static.AnalysisStructured
import Static.AnalysisArchitecture
import Static.ArchDiagram (emptyExtStUnitCtx)

import Common.AS_Annotation hiding (isAxiom, isDef)
import Common.DocUtils
import Common.GlobalAnnotations
import Common.ConvertGlobalAnnos
import Common.AnalyseAnnos
import Common.Result
import Common.ResultT
import Common.LibName
import Common.Id
import Common.IO
import qualified Common.Unlit as Unlit

import Driver.Options
import Driver.ReadFn
import Driver.WriteLibDefn

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

import Control.Monad
import Control.Monad.Trans

import Data.Maybe

import System.Directory
import System.FilePath

import LF.Twelf2DG

import Framework.Analysis

-- a set of library names to check for cyclic imports
type LNS = Set.Set LibName

{- | parsing and static analysis for files
Parameters: logic graph, default logic, file name -}
anaSourceFile :: LogicGraph -> HetcatsOpts -> LNS -> LibEnv -> DGraph
  -> FilePath -> ResultT IO (LibName, LibEnv)
anaSourceFile = anaSource Nothing

anaSource :: Maybe LibName -- ^ suggested library name
  -> LogicGraph -> HetcatsOpts -> LNS -> LibEnv -> DGraph
  -> FilePath -> ResultT IO (LibName, LibEnv)
anaSource mln lgraph opts topLns libenv initDG fname = ResultT $
  if checkUri fname then do
       putIfVerbose opts 2 $ "Downloading file " ++ fname
       resp <- simpleHTTP (getRequest fname)
       input <- getResponseBody resp
       runResultT $
         anaString mln lgraph opts topLns libenv initDG input fname
  else do
  fname' <- findFileOfLibName opts fname
  case fname' of
    Nothing ->
        return $ fail $ "no file for library '" ++ fname ++ "' found."
    Just file ->
        if any (`isSuffixOf` file) [envSuffix, prfSuffix] then
          return $ fail $ "no matching source file for '" ++ fname ++ "' found."
        else do
        inputLit <- readEncFile (ioEncoding opts) file
        let input = (if unlit opts then Unlit.unlit else id) inputLit
            libStr = if isAbsolute fname
              then convertFileToLibStr fname
              else dropExtensions fname
            nLn = case mln of
              Nothing | useLibPos opts ->
                Just $ emptyLibName libStr
              _ -> mln
        putIfVerbose opts 2 $ "Reading file " ++ file
        if takeExtension file /= ('.' : show TwelfIn)
           then runResultT $
                   anaString nLn lgraph opts topLns libenv initDG input file
           else do
             res <- anaTwelfFile opts file
             case res of
                  Nothing -> fail ""
                  Just (lname, lenv) -> return $ Result [] $
                      Just (lname, Map.union lenv libenv)

{- | parsing and static analysis for string (=contents of file)
Parameters: logic graph, default logic, contents of file, filename -}
anaString :: Maybe LibName -- ^ suggested library name
  -> LogicGraph -> HetcatsOpts -> LNS -> LibEnv -> DGraph -> String
  -> FilePath -> ResultT IO (LibName, LibEnv)
anaString mln lgraph opts topLns libenv initDG input file = do
  curDir <- lift getCurrentDirectory -- get full path for parser positions
  mt <- if checkUri file then return noTime
    else lift $ getModificationTime file
  let realFileName = curDir </> file
      posFileName = case mln of
          Just gLn | useLibPos opts -> show $ getLibId gLn
          _ -> if checkUri file then file else realFileName

  Lib_defn pln is ps ans <- readLibDefnM lgraph opts posFileName input

  let noSuffixFile = rmSuffix file
      spN = convertFileToLibStr file
      noLibName = null $ show $ getLibId pln
      ln = setFilePath posFileName mt
            $ if noLibName then fromMaybe (emptyLibName spN) mln else pln
      nIs = case is of
        [Annoted (Spec_defn spn gn as qs) rs [] []]
            | noLibName && null (tokStr spn)
                -> [Annoted (Spec_defn (mkSimpleId spN) gn as qs) rs [] []]
        _ -> is
      ast = Lib_defn ln nIs ps ans
  case analysis opts of
      Skip -> do
          lift $ putIfVerbose opts 1 $
                  "Skipping static analysis of library " ++ show ln
          ga <- liftR $ addGlobalAnnos emptyGlobalAnnos ans
          lift $ writeLibDefn ga file opts ast
          liftR mzero
      _ -> do
          let libstring = show $ getLibId ln
          unless (isSuffixOf libstring noSuffixFile) $ lift
            $ putIfVerbose opts 1
            $ "### file name '" ++ file ++ "' does not match library name '"
            ++ libstring ++ "'"
          lift $ putIfVerbose opts 1 $ "Analyzing "
               ++ (if noLibName then "file " ++ file ++ " as " else "")
               ++ "library " ++ show ln
          (_, ld, ga, lenv) <- anaLibDefn lgraph opts topLns libenv initDG ast
          case Map.lookup ln lenv of
              Nothing -> error $ "anaString: missing library: " ++ show ln
              Just dg -> lift $ do
                  writeLibDefn ga file opts ld
                  when (hasEnvOut opts)
                        (writeFileInfo opts ln file ld dg)
                  return (ln, lenv)

-- lookup or read a library
anaLibFile :: LogicGraph -> HetcatsOpts -> LNS -> LibEnv -> DGraph -> LibName
           -> ResultT IO (LibName, LibEnv)
anaLibFile lgraph opts topLns libenv initDG ln =
    let lnstr = show ln in case Map.lookup ln libenv of
    Just _ -> do
        analyzing opts $ "from " ++ lnstr
        return (ln, libenv)
    Nothing -> do
            putMessageIORes opts 1 $ "Downloading " ++ lnstr ++ " ..."
            res <- anaLibFileOrGetEnv lgraph
              (if recurse opts then opts else opts
                      { outtypes = []
                      , unlit = False })
              (Set.insert ln topLns) libenv initDG ln $ libNameToFile ln
            putMessageIORes opts 1 $ "... loaded " ++ lnstr
            return res

-- | lookup or read a library
anaLibFileOrGetEnv :: LogicGraph -> HetcatsOpts -> LNS -> LibEnv -> DGraph
                   -> LibName -> FilePath -> ResultT IO (LibName, LibEnv)
anaLibFileOrGetEnv lgraph opts topLns libenv initDG ln file = ResultT $ do
     let envFile = rmSuffix file ++ envSuffix
     recent_envFile <- checkRecentEnv opts envFile file
     if recent_envFile
        then do
             mgc <- readVerbose lgraph opts ln envFile
             case mgc of
                 Nothing -> runResultT $ do
                     lift $ putIfVerbose opts 1 $ "Deleting " ++ envFile
                     lift $ removeFile envFile
                     anaSource (Just ln) lgraph opts topLns libenv initDG file
                 Just (ld, gc) -> do
                     writeLibDefn (globalAnnos gc) file opts ld
                          -- get all DGRefs from DGraph
                     Result ds mEnv <- runResultT $ foldl
                         ( \ ioLibEnv labOfDG -> let node = snd labOfDG in
                               if isDGRef node then do
                                 let ln2 = dgn_libname node
                                 p_libEnv <- ioLibEnv
                                 if Map.member ln2 p_libEnv then
                                    return p_libEnv
                                    else fmap snd $ anaLibFile lgraph
                                         opts topLns p_libEnv initDG ln2
                               else ioLibEnv)
                         (return $ Map.insert ln gc libenv)
                         $ labNodesDG gc
                     return $ Result ds $ fmap
                                ( \ rEnv -> (ln, rEnv)) mEnv
        else runResultT
          $ anaSource (Just ln) lgraph opts topLns libenv initDG file

{- | analyze a LIB_DEFN.
  Parameters: logic graph, default logic, opts, library env, LIB_DEFN.
  Call this function as follows:

>    do Result diags res <- runResultT (anaLibDefn ...)
>       mapM_ (putStrLn . show) diags
-}
anaLibDefn :: LogicGraph -> HetcatsOpts -> LNS -> LibEnv -> DGraph -> LIB_DEFN
  -> ResultT IO (LibName, LIB_DEFN, GlobalAnnos, LibEnv)
anaLibDefn lgraph opts topLns libenv dg (Lib_defn ln alibItems pos ans) = do
  gannos <- showDiags1 opts $ liftR $ addGlobalAnnos emptyGlobalAnnos ans
  (libItems', dg', libenv', _) <- foldM (anaLibItemAux opts topLns ln)
      ([], dg { globalAnnos = gannos }, libenv
      , setCurLogic (defLogic opts) lgraph) (map item alibItems)
  let dg1 = computeDGraphTheories libenv' $ markFree libenv' $
            markHiding libenv' dg'
      newLD = Lib_defn ln
        (zipWith replaceAnnoted (reverse libItems') alibItems) pos ans
      dg2 = dg1 { optLibDefn = Just newLD }
  return (ln, newLD, globalAnnos dg2, Map.insert ln dg2 libenv')

anaLibItemAux :: HetcatsOpts -> LNS -> LibName
  -> ([LIB_ITEM], DGraph, LibEnv, LogicGraph)
  -> LIB_ITEM -> ResultT IO ([LIB_ITEM], DGraph, LibEnv, LogicGraph)
anaLibItemAux opts topLns ln q@(libItems', dg1, libenv1, lg) libItem = let
  currLog = currentLogic lg
  newOpts = if elem currLog ["DMU", "Framework"] then
              opts { defLogic = currLog } else opts
  in ResultT $ do
      res2@(Result diags2 res) <- runResultT
        $ anaLibItem lg newOpts topLns ln libenv1 dg1 libItem
      runResultT $ showDiags1 opts (liftR res2)
      let mRes = case res of
             Just (libItem', dg1', libenv1', newLG) ->
                 Just (libItem' : libItems', dg1', libenv1', newLG)
             Nothing -> Just q
      if outputToStdout opts then
         if hasErrors diags2 then
            fail "Stopped due to errors"
            else runResultT $ liftR $ Result [] mRes
         else runResultT $ liftR $ Result diags2 mRes

putMessageIORes :: HetcatsOpts -> Int -> String -> ResultT IO ()
putMessageIORes opts i msg =
   if outputToStdout opts
   then lift $ putIfVerbose opts i msg
   else liftR $ message () msg

analyzing :: HetcatsOpts -> String -> ResultT IO ()
analyzing opts = putMessageIORes opts 1 . ("Analyzing " ++)

alreadyDefined :: String -> String
alreadyDefined str = "Name " ++ str ++ " already defined"

-- | analyze a GENERICITY
anaGenericity :: LogicGraph -> LibName -> DGraph -> HetcatsOpts -> NodeName
  -> GENERICITY -> Result (GENERICITY, GenSig, DGraph)
anaGenericity lg ln dg opts name
    gen@(Genericity (Params psps) (Imported isps) pos) =
  adjustPos pos $ case psps of
  [] -> do -- no parameter ...
    unless (null isps) $ plain_error ()
      "Parameterless specifications must not have imports" pos
    l <- lookupCurrentLogic "GENERICITY" lg
    return (gen, GenSig (EmptyNode l) [] $ EmptyNode l, dg)
  _ -> do
   l <- lookupCurrentLogic "IMPORTS" lg
   (imps', nsigI, dg') <- case isps of
     [] -> return ([], EmptyNode l, dg)
     _ -> do
      (is', _, nsig', dgI) <- anaUnion False lg ln dg (EmptyNode l)
          (extName "Imports" name) opts isps
      return (is', JustNode nsig', dgI)
   (ps', nsigPs, ns, dg'') <- anaUnion False lg ln dg' nsigI
          (extName "Parameters" name) opts psps
   return (Genericity (Params ps') (Imported imps') pos,
     GenSig nsigI nsigPs $ JustNode ns, dg'')

-- | analyse a LIB_ITEM
anaLibItem :: LogicGraph -> HetcatsOpts -> LNS -> LibName -> LibEnv -> DGraph
  -> LIB_ITEM -> ResultT IO (LIB_ITEM, DGraph, LibEnv, LogicGraph)
anaLibItem lg opts topLns currLn libenv dg itm = case itm of
  Spec_defn spn gen asp pos -> do
    let spstr = tokStr spn
        nName = makeName spn
    analyzing opts $ "spec " ++ spstr
    (gen', gsig@(GenSig _ _args allparams), dg') <-
      liftR $ anaGenericity lg currLn dg opts nName gen
    (sanno1, impliesA) <- liftR $ getSpecAnnos pos asp
    when impliesA $ liftR $ plain_error ()
       "unexpected initial %implies in spec-defn" pos
    (sp', body, dg'') <-
      liftR (anaSpecTop sanno1 True lg currLn dg'
             allparams nName opts (item asp))
    let libItem' = Spec_defn spn gen' (replaceAnnoted sp' asp) pos
        genv = globalEnv dg
    if Map.member spn genv
      then liftR $ plain_error (libItem', dg'', libenv, lg)
               (alreadyDefined spstr) pos
      else
        -- let (_n, dg''') = addSpecNodeRT dg'' (UnitSig args body) $ show spn
        return
         ( libItem'
         , dg'' { globalEnv = Map.insert spn (SpecEntry
                  $ ExtGenSig gsig body) genv }
         , libenv, lg)
  View_defn vn gen vt gsis pos -> do
    analyzing opts $ "view " ++ tokStr vn
    liftR $ anaViewDefn lg currLn libenv dg opts vn gen vt gsis pos
  Arch_spec_defn asn asp pos -> do
    let asstr = tokStr asn
    analyzing opts $ "arch spec " ++ asstr
    (_, _, diag, archSig, dg', asp') <- liftR $ anaArchSpec lg currLn dg
      opts emptyExtStUnitCtx Nothing $ item asp
    let asd' = Arch_spec_defn asn (replaceAnnoted asp' asp) pos
        genv = globalEnv dg'
    if Map.member asn genv
      then liftR $ plain_error (asd', dg', libenv, lg)
               (alreadyDefined asstr) pos
      else do
            let dg'' = updateNodeNameRT dg'
                       (refSource $ getPointerFromRef archSig) $ show asn
                dg3 = dg'' { archSpecDiags =
                           Map.insert (show asn) diag
                           $ archSpecDiags dg''}
            return (asd', dg3
              { globalEnv = Map.insert asn (ArchOrRefEntry True archSig) genv }
              , libenv, lg)
  Unit_spec_defn usn usp pos -> do
    let usstr = tokStr usn
    analyzing opts $ "unit spec " ++ usstr
    l <- lookupCurrentLogic "Unit_spec_defn" lg
    (rSig, dg', usp') <-
      liftR $ anaUnitSpec lg currLn dg opts (EmptyNode l) Nothing usp
    unitSig <- liftR $ getUnitSigFromRef rSig
    let usd' = Unit_spec_defn usn usp' pos
        genv = globalEnv dg'
    if Map.member usn genv
      then liftR $ plain_error (itm, dg', libenv, lg)
               (alreadyDefined usstr) pos
      else return (usd', dg'
             { globalEnv = Map.insert usn (UnitEntry unitSig) genv },
             libenv, lg)
  Ref_spec_defn rn rsp pos -> do
    let rnstr = tokStr rn
    l <- lookupCurrentLogic "Ref_spec_defn" lg
    (_, _, _, rsig, dg', rsp') <- liftR $ anaRefSpec lg currLn dg opts
      (EmptyNode l) rn emptyExtStUnitCtx Nothing rsp
    analyzing opts $ "ref spec " ++ rnstr
    let rsd' = Ref_spec_defn rn rsp' pos
        genv = globalEnv dg'
    if Map.member rn genv
      then liftR $ plain_error (itm, dg', libenv, lg)
               (alreadyDefined rnstr) pos
      else return ( rsd', dg'
        { globalEnv = Map.insert rn (ArchOrRefEntry False rsig) genv }
        , libenv, lg)
  Logic_decl logN pos -> do
    putMessageIORes opts 1 $ showDoc itm ""
    (mth, newLg) <- liftR
      $ adjustPos pos $ anaSublogic opts logN currLn dg libenv lg
    case mth of
      Nothing -> return (itm, dg, libenv, newLg)
      Just (libName, _, (n, _)) -> do
        -- properly compute theory
        let newLibEnv = computeLibEnvTheories (Map.insert currLn dg libenv)
            _lbl = labDG (lookupDGraph libName newLibEnv) n
            newDG = lookupDGraph currLn newLibEnv
            -- store th-lbl in newDG
        return (itm, newDG, newLibEnv, newLg)
  Download_items ln items pos -> if Set.member ln topLns then
    liftR $ mkError "illegal cyclic library import"
      $ Set.map getLibId topLns
    else do
        (ln', libenv') <- anaLibFile lg opts topLns libenv
          (cpIndexMaps dg emptyDG) ln
        unless (ln == ln')
          $ liftR $ warning ()
              (shows ln " does not match internal name " ++ shows ln' "")
              pos
        case Map.lookup ln' libenv' of
          Nothing -> error $ "Internal error: did not find library "
            ++ show ln' ++ " available: " ++ show (Map.keys libenv')
          Just dg' -> do
            let dg0 = cpIndexMaps dg' dg
            dg1 <- liftR $ anaItemNamesOrMaps libenv' ln' dg' dg0 items
            return (itm, dg1, libenv', lg)
  Newlogic_defn ld _ -> ResultT $ do
    dg' <- anaLogicDef ld dg
    return $ Result [] $ Just (itm, dg', libenv, lg)
  Newcomorphism_defn com _ -> ResultT $ do
    dg' <- anaComorphismDef com dg
    return $ Result [] $ Just (itm, dg', libenv, lg)

-- the first DGraph dg' is that of the imported library
anaItemNamesOrMaps :: LibEnv -> LibName -> DGraph -> DGraph
  -> [ITEM_NAME_OR_MAP] -> Result DGraph
anaItemNamesOrMaps libenv' ln refDG dg items = do
  (genv1, dg1) <- foldM
    (anaItemNameOrMap libenv' ln refDG) (globalEnv dg, dg) items
  gannos'' <- globalAnnos dg `mergeGlobalAnnos` globalAnnos refDG
  return dg1
    { globalAnnos = gannos''
    , globalEnv = genv1 }

-- | analyse genericity and view type and construct gmorphism
anaViewDefn :: LogicGraph -> LibName -> LibEnv -> DGraph -> HetcatsOpts
  -> SIMPLE_ID -> GENERICITY -> VIEW_TYPE -> [G_mapping] -> Range
  -> Result (LIB_ITEM, DGraph, LibEnv, LogicGraph)
anaViewDefn lg ln libenv dg opts vn gen vt gsis pos = do
  let vName = makeName vn
  (gen', gsig@(GenSig _ _ allparams), dg') <-
       anaGenericity lg ln dg opts vName gen
  (vt', (src@(NodeSig nodeS gsigmaS)
        , tar@(NodeSig nodeT gsigmaT@(G_sign lidT _ _))), dg'') <-
       anaViewType lg ln dg' allparams opts vName vt
  let genv = globalEnv dg''
  if Map.member vn genv
    then plain_error (View_defn vn gen' vt' gsis pos, dg'', libenv, lg)
                    (alreadyDefined $ tokStr vn) pos
    else do
      let (tsis, hsis) = partitionGmaps gsis
      (gsigmaS', tmor) <- if null tsis then do
          (gsigmaS', imor) <- gSigCoerce lg gsigmaS (Logic lidT)
          tmor <- gEmbedComorphism imor gsigmaS
          return (gsigmaS', tmor)
        else do
          mor <- anaRenaming lg allparams gsigmaS opts (Renaming tsis pos)
          let gsigmaS'' = cod mor
          (gsigmaS', imor) <- gSigCoerce lg gsigmaS'' (Logic lidT)
          tmor <- gEmbedComorphism imor gsigmaS''
          fmor <- comp mor tmor
          return (gsigmaS', fmor)
      emor <- fmap gEmbed $ anaGmaps lg opts pos gsigmaS' gsigmaT hsis
      gmor <- comp tmor emor
      let vsig = ExtViewSig src gmor $ ExtGenSig gsig tar
          voidView = nodeS == nodeT && isInclusion gmor
      when voidView $ warning ()
        ("identity mapping of source to same target for view: " ++ tokStr vn)
        pos
      return (View_defn vn gen' vt' gsis pos,
                (if voidView then dg'' else insLink dg'' gmor globalThm
                 (DGLinkView vn $ Fitted gsis) nodeS nodeT)
                -- 'LeftOpen' for conserv correct?
                { globalEnv = Map.insert vn (ViewOrStructEntry True vsig) genv }
               , libenv, lg)

{- | analyze a VIEW_TYPE
The first three arguments give the global context
The AnyLogic is the current logic
The NodeSig is the signature of the parameter of the view
flag, whether just the structure shall be analysed -}
anaViewType :: LogicGraph -> LibName -> DGraph -> MaybeNode -> HetcatsOpts
  -> NodeName -> VIEW_TYPE -> Result (VIEW_TYPE, (NodeSig, NodeSig), DGraph)
anaViewType lg ln dg parSig opts name (View_type aspSrc aspTar pos) = do
  l <- lookupCurrentLogic "VIEW_TYPE" lg
  (spSrc', srcNsig, dg') <- adjustPos pos $ anaSpec False lg ln dg (EmptyNode l)
    (extName "Source" name) opts (item aspSrc)
  (spTar', tarNsig, dg'') <- adjustPos pos $ anaSpec True lg ln dg' parSig
    (extName "Target" name) opts (item aspTar)
  return (View_type (replaceAnnoted spSrc' aspSrc)
                    (replaceAnnoted spTar' aspTar)
                    pos,
          (srcNsig, tarNsig), dg'')

anaItemNameOrMap :: LibEnv -> LibName -> DGraph -> (GlobalEnv, DGraph)
  -> ITEM_NAME_OR_MAP -> Result (GlobalEnv, DGraph)
anaItemNameOrMap libenv ln refDG res itm =
   anaItemNameOrMap1 libenv ln refDG res $ case itm of
     Item_name name -> (name, name)
     Item_name_map old new _ -> (old, new)

-- | Auxiliary function for not yet implemented features
anaErr :: String -> a
anaErr f = error $ "*** Analysis of " ++ f ++ " is not yet implemented!"

anaItemNameOrMap1 :: LibEnv -> LibName -> DGraph -> (GlobalEnv, DGraph)
  -> (SIMPLE_ID, SIMPLE_ID) -> Result (GlobalEnv, DGraph)
anaItemNameOrMap1 libenv ln refDG (genv, dg) (old, new) = do
  let newName = makeName new
  entry <- maybeToResult (tokPos old)
            (tokStr old ++ " not found") (Map.lookup old $ globalEnv refDG)
  maybeToResult (tokPos new) (tokStr new ++ " already used")
    $ case Map.lookup new genv of
    Nothing -> Just ()
    Just _ -> Nothing
  case entry of
    SpecEntry extsig ->
      let (dg1, extsig1) = refExtsig libenv ln refDG dg newName extsig
          genv1 = Map.insert new (SpecEntry extsig1) genv
       in return (genv1, dg1)
    ViewOrStructEntry b vsig ->
      let (dg1, vsig1) = refViewsig libenv ln refDG dg newName vsig
          genv1 = Map.insert new (ViewOrStructEntry b vsig1) genv
      in return (genv1, dg1)
    UnitEntry _usig -> anaErr "unit spec download"
    ArchOrRefEntry b _rsig -> anaErr $ (if b then "arch" else "ref")
      ++ " spec download"

refNodesig :: LibEnv -> LibName -> DGraph -> DGraph -> (NodeName, NodeSig)
           -> (DGraph, NodeSig)
refNodesig libenv refln refDG dg
  (name, NodeSig refn sigma@(G_sign lid sig ind)) =
  let (ln, _, (n, lbl)) =
         lookupRefNode libenv refln refDG refn
      refInfo = newRefInfo ln n
      new = newInfoNodeLab name refInfo
        $ noSensGTheory lid sig ind
      nodeCont = new { globalTheory = globalTheory lbl }
      node = getNewNodeDG dg
   in case lookupInAllRefNodesDG refInfo dg of
        Just existNode -> (dg, NodeSig existNode sigma)
        Nothing ->
          ( addToRefNodesDG node refInfo $ insNodeDG (node, nodeCont) dg
          , NodeSig node sigma)

refNodesigs :: LibEnv -> LibName -> DGraph -> DGraph -> [(NodeName, NodeSig)]
            -> (DGraph, [NodeSig])
refNodesigs libenv ln = mapAccumR . refNodesig libenv ln

refExtsig :: LibEnv -> LibName -> DGraph -> DGraph -> NodeName -> ExtGenSig
          -> (DGraph, ExtGenSig)
refExtsig libenv ln refDG dg name
  (ExtGenSig (GenSig imps params gsigmaP) body) = let
  pName = extName "Parameters" name
  (dg1, imps1) = case imps of
    EmptyNode _ -> (dg, imps)
    JustNode ns -> let
        (dg0, nns) = refNodesig libenv ln refDG dg (extName "Imports" name, ns)
        in (dg0, JustNode nns)
  (dg2, params1) = refNodesigs libenv ln refDG dg1
      $ snd $ foldr (\ p (n, l) -> let nn = inc n in
              (nn, (nn, p) : l)) (pName, []) params
  (dg3, gsigmaP1) = case gsigmaP of
    EmptyNode _ -> (dg, gsigmaP)
    JustNode ns -> let
        (dg0, nns) = refNodesig libenv ln refDG dg2 (pName, ns)
        in (dg0, JustNode nns)
  (dg4, body1) = refNodesig libenv ln refDG dg3 (name, body)
  in (dg4, ExtGenSig (GenSig imps1 params1 gsigmaP1) body1)

refViewsig :: LibEnv -> LibName -> DGraph -> DGraph -> NodeName -> ExtViewSig
           -> (DGraph, ExtViewSig)
refViewsig libenv ln refDG dg name (ExtViewSig src mor extsig) = let
  (dg1, src1) = refNodesig libenv ln refDG dg (extName "Source" name, src)
  (dg2, extsig1) = refExtsig libenv ln refDG dg1 (extName "Target" name) extsig
  in (dg2, ExtViewSig src1 mor extsig1)
