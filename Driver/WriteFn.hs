{-# OPTIONS -cpp #-}
{- |
Module      :  $Header$
Copyright   :  (c) Klaus L�ttich, C.Maeder, Uni Bremen 2002-2006
License     :  similar to LGPL, see HetCATS/LICENSE.txt or LIZENZ.txt

Maintainer  :  maeder@tzi.de
Stability   :  provisional
Portability :  non-portable(DevGraph)

This module provides functions to write a pretty printed abstract
  syntax and all the other formats.
-}

module Driver.WriteFn where

import Control.Monad
import Data.Maybe
import Data.List

import Text.ParserCombinators.Parsec
import Text.PrettyPrint.HughesPJ(render)
import Common.Utils
import Common.Id
import Common.Doc (pretty)
import Common.Result
import Common.GlobalAnnotations (GlobalAnnos)
import Common.ConvertGlobalAnnos()
import qualified Common.Lib.Map as Map
import Common.SimpPretty (writeFileSDoc)

import Common.ATerm.Lib
import Common.ATerm.ReadWrite

import Logic.Coerce
import Logic.Grothendieck
import Comorphisms.LogicGraph

import Syntax.Print_HetCASL
import Syntax.AS_Library (LIB_DEFN(), LIB_NAME())

import CASL.Logic_CASL
#ifdef UNI_PACKAGE
import CASL.CompositionTable.ComputeTable
import CASL.CompositionTable.CompositionTable
#endif
#ifdef PROGRAMATICA
import Haskell.CreateModules
#endif
import Isabelle.CreateTheories
import Isabelle.IsaParse
import SPASS.CreateDFGDoc

import Logic.Prover
import Static.DevGraph
import Static.DotGraph
import Static.DGToSpec
import qualified Static.PrintDevGraph as DG
import Proofs.StatusUtils

import ATC.DevGraph()
import ATC.GlobalAnnotations()

import Driver.Options

import OMDoc.OMDocOutput

-- | compute the prefix for files to be written out
getFilePrefix :: HetcatsOpts -> FilePath -> (FilePath, FilePath)
getFilePrefix opt file =
    let odir' = outdir opt
        (base, path, _) = fileparse (envSuffix : downloadExtensions) file
        odir = if null odir' then path else odir'
    in (odir, pathAndBase odir base)

{- |
  Write the given LIB_DEFN in every format that HetcatsOpts includes.
  Filenames are determined by the output formats.
-}
write_LIB_DEFN :: GlobalAnnos -> FilePath -> HetcatsOpts -> LIB_DEFN -> IO ()
write_LIB_DEFN ga file opt ld = do
    let (odir, filePrefix) = getFilePrefix opt file
        filename ty = filePrefix ++ "." ++ show ty
        verbMesg ty = putIfVerbose opt 2 $ "Writing file: " ++ filename ty
        printAscii ty = do
          verbMesg ty
          write_casl_asc opt ga (filename ty) ld
        showAst ty = do
          verbMesg ty
          writeFile (filename ty) $ show ld
        write_type :: OutType -> IO ()
        write_type t = case t of
            HetCASLOut OutASTree OutAscii -> showAst t
            PrettyOut PrettyAscii -> printAscii t
            PrettyOut PrettyLatex -> do
                verbMesg t
                write_casl_latex opt ga (filename t) ld
            _ -> return () -- implemented elsewhere
    putIfVerbose opt 3 ("Current OutDir: " ++ odir)
    mapM_ write_type $ outtypes opt

write_casl_asc :: HetcatsOpts -> GlobalAnnos -> FilePath -> LIB_DEFN -> IO ()
write_casl_asc _ ga oup ld = writeFile oup $ printLIB_DEFN_text ga ld

debug_latex_filename :: FilePath -> FilePath
debug_latex_filename = (\(b,p,_) -> p++ b ++ ".debug.tex") .
                       fileparse [".pp.tex"]

write_casl_latex :: HetcatsOpts -> GlobalAnnos -> FilePath -> LIB_DEFN -> IO ()
write_casl_latex opt ga oup ld =
    do writeFile oup $ printLIB_DEFN_latex ga ld
       doIfVerbose opt 5 $
           writeFile (debug_latex_filename oup) $
               printLIB_DEFN_debugLatex ga ld

toShATermString :: (ShATermConvertible a) => a -> IO String
toShATermString atcon = fmap writeSharedATerm $ versionedATermTable atcon

writeShATermFile :: (ShATermConvertible a) => FilePath -> a -> IO ()
writeShATermFile fp atcon = toShATermString atcon >>= writeFile fp

versionedATermTable :: (ShATermConvertible a) => a -> IO ATermTable
versionedATermTable atcon = do
    att0 <- newATermTable
    (att1, versionnr) <- toShATermAux att0 hetsVersion
    (att2, aterm) <- toShATermAux att1 atcon
    return $ fst $ addATerm (ShAAppl "hets" [versionnr,aterm] []) att2

writeShATermFileSDoc :: (ShATermConvertible a) => FilePath -> a -> IO ()
writeShATermFileSDoc fp atcon = do
   att <- versionedATermTable atcon
   writeFileSDoc fp $ writeSharedATermSDoc att

writeFileInfo :: HetcatsOpts -> LIB_NAME -> FilePath -> LIB_DEFN
              -> GlobalContext -> IO ()
writeFileInfo opts ln file ld gctx =
  let envFile = snd (getFilePrefix opts file) ++ ".env" in
  case analysis opts of
  Basic -> do
      putIfVerbose opts 2 ("Writing file: " ++ envFile)
      catch (writeShATermFileSDoc envFile (ln, (ld, gctx))) $ \ err -> do
              putIfVerbose opts 2 (envFile ++ " not written")
              putIfVerbose opts 3 ("see following error description:\n"
                                   ++ shows err "\n")
  _ -> putIfVerbose opts 2 ("Not writing " ++ envFile)

writeVerbFile :: HetcatsOpts -> FilePath -> String -> IO ()
writeVerbFile opt f str = do
    putIfVerbose opt 2 $ "Writing file: " ++ f
    writeFile f str

writeSpecFiles :: HetcatsOpts -> FilePath -> LibEnv -> GlobalAnnos
               -> (LIB_NAME, GlobalEnv) -> IO ()
writeSpecFiles opt file lenv ga (ln, gctx) = do
    let ns = specNames opt
        filePrefix = snd $ getFilePrefix opt file
        outTypes = outtypes opt
        allSpecs = null ns
    mapM_ ( \ ot -> let f = filePrefix ++ "." ++ show ot in
        case ot of
          Prf -> do
              str <- toShATermString (ln, lookupHistory ln lenv)
              writeVerbFile opt f str
          OmdocOut ->
            hetsToOMDoc opt (ln, lenv) f
          GraphOut Dot ->
            writeVerbFile opt f . concat . dot . devGraph $
                          lookupGlobalContext ln lenv
          _ -> return () -- treat others below
          ) outTypes
    mapM_ ( \ i -> case Map.lookup i gctx of
        Just (SpecEntry (_,_,_, NodeSig n _)) ->
         case computeTheory lenv ln n of
          Result ds Nothing -> do
                 putIfVerbose opt 0 $ "could not compute theory of spec "
                                  ++ show i
                 putIfVerbose opt 0 $ unlines $ map show ds
          Result _ (Just raw_gTh0) -> do
            let tr = transNames opt
                resTh = if null tr then return (raw_gTh0, "") else do
                   comor <- lookupCompComorphism (map tokStr tr) logicGraph
                   tTh <- mapG_theory comor raw_gTh0
                   return (tTh, show comor)
            case resTh of
             Result es Nothing -> do
                   putIfVerbose opt 0 "could not translate theory"
                   putIfVerbose opt 0 $ unlines $ map show es
             Result _ (Just (raw_gTh, tStr)) ->
              case theoremsToAxioms raw_gTh of
                gTh@(G_theory lid sign0 sens0) -> do
                  if null tStr then return () else
                     putIfVerbose opt 2 $
                        "Translated using comorphism " ++ tStr
                  putIfVerbose opt 4 $ "Sublogic of " ++ show i ++ ": " ++
                          (show $ sublogicOfTh gTh)
                  mapM_ ( \ ot ->
                     let f = filePrefix ++ "_" ++ show i ++ "." ++ show ot
                     in case ot of
                      ThyFile -> case printTheory (libdir opt) ln i gTh of
                                    Nothing -> putIfVerbose opt 0 $
                                        "could not translate to Isabelle " ++
                                         show i
                                    Just d -> do
                                      let s = shows d "\n"
                                      case parse parseTheory f s of
                                          Left err -> putIfVerbose opt 0 $
                                                      show err
                                          _ -> return ()
                                      writeVerbFile opt f s
                      DfgFile c -> do
                            mDoc <- printTheoryAsDFG ln i
                                       (case c of
                                        ConsistencyCheck -> True
                                        OnlyAxioms  -> False)
                                       gTh
                            maybe (putIfVerbose opt 0 $
                                     "could not translate to DFG: " ++
                                     show i)
                                  (\ d -> writeVerbFile opt f $ shows d "\n")
                                  mDoc

                      TheoryFile d -> if null $ show d then
                          writeVerbFile opt f $
                              shows (DG.printTh ga i raw_gTh) "\n"
                          else putIfVerbose opt 0
                                   "printing theory delta is not implemented"
                      SigFile d -> if null $ show d then
                          writeVerbFile opt f $
                              shows (pretty $ signOf gTh) "\n"
                          else putIfVerbose opt 0
                                 "printing signature delta is not implemented"
#ifdef PROGRAMATICA
                      HaskellOut -> case printModule gTh of
                                    Nothing -> putIfVerbose opt 0 $
                                        "could not translate to Haskell " ++
                                         show i
                                    Just d ->
                                        writeVerbFile opt f $ shows d "\n"
#endif
#ifdef UNI_PACKAGE
                      ComptableXml -> let
                                    th = (sign0, toNamedList sens0)
                                    r1 = coerceBasicTheory lid CASL "" th
                                  in case r1 of
                                     Nothing -> putIfVerbose opt 0 $
                                         "could not translate from CASL " ++
                                         show i
                                     Just th2 ->
                                       let Result d res =
                                               computeCompTable i th2
                                        in do showDiags opt d
                                              when (isJust res) $
                                                 writeVerbFile opt f $
                                                 render $ table_document $
                                                 fromJust res
#endif
                      _ -> return () -- ignore other file types
                             ) outTypes
        _ -> if allSpecs then return () else
                    putIfVerbose opt 0 $ "Unknown spec name: " ++ show i
              ) $ if null outTypes then [] else
                      if allSpecs then Map.keys gctx else ns
