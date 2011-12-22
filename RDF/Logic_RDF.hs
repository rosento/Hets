{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS -w #-}
{- |
Module      :  $Header$
Description :  instance of the class Logic for RDF
Copyright   :  (c) Felix Mance
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  f.mance@jacobs-university.de
Stability   :  provisional
Portability :  non-portable

Here is the place where the class Logic is instantiated for RDF
-}

module RDF.Logic_RDF where

import Common.AS_Annotation
import Common.Doc
import Common.DocUtils
import Common.ProofTree
import Common.DefaultMorphism
import Common.Consistency
import Common.ProverTools

import ATC.ProofTree ()

import Logic.Logic

import RDF.AS
import RDF.ATC_RDF ()
import RDF.Parse
import RDF.Symbols
import RDF.Print
import RDF.Sign
import RDF.Morphism
import RDF.Sublogic
import RDF.StaticAnalysis

data RDF = RDF deriving Show

instance Language RDF where
  language_name _ = "RDF"
  description _ =
    "RDF -- Resource Description Framework http://www.w3.org/RDF/"

instance Category Sign RDFMorphism where
    ide sig = inclRDFMorphism sig sig
    dom = osource
    cod = otarget
    legal_mor = legalMor
    isInclusion = isRDFInclusion
    composeMorphisms = composeMor

instance Syntax RDF RDFGraph SymbItems SymbMapItems where
    parse_basic_spec RDF = Just basicSpec
    parse_symb_items RDF = error "RDF.b" -- Just symbItems
    parse_symb_map_items RDF = error "RDF.c" -- Just symbMapItems

instance Sentences RDF Axiom Sign RDFMorphism RDFEntity where
    map_sen RDF = mapSen
    print_named RDF namedSen = pretty $ sentence namedSen
    sym_of RDF = singletonList . symOf
    symmap_of RDF = symMapOf

instance StaticAnalysis RDF RDFGraph Axiom
               SymbItems SymbMapItems
               Sign
               RDFMorphism
               RDFEntity RawSymb where
      basic_analysis RDF = Just basicRDFAnalysis
      stat_symb_items RDF _ = return . statSymbItems
      stat_symb_map_items RDF _ _ = statSymbMapItems
      empty_signature RDF = emptySign
      signature_union RDF = uniteSign
      signatureDiff RDF s = return . diffSig s
      final_union RDF = signature_union RDF
      is_subsig RDF = isSubSign
      subsig_inclusion RDF s = return . inclRDFMorphism s
      matches RDF = matchesSym
      symbol_to_raw RDF = ASymbol
      induced_from_morphism RDF = inducedFromMor
      cogenerated_sign RDF = cogeneratedSign
      generated_sign RDF = generatedSign

instance Logic RDF RDFSub RDFGraph Axiom SymbItems SymbMapItems
               Sign
               RDFMorphism RDFEntity RawSymb ProofTree where
         empty_proof_tree RDF = emptyProofTree

{-
instance SemiLatticeWithTop ProfSub where
    join = maxS
    top = topS

instance SublogicName ProfSub where
    sublogicName = nameS

instance MinSublogic ProfSub Axiom where
    minSublogic = psAxiom

instance MinSublogic ProfSub OWLMorphism where
    minSublogic = sMorph

instance ProjectSublogic ProfSub OWLMorphism where
    projectSublogic = prMorph

instance MinSublogic ProfSub Sign where
    minSublogic = sSig

instance ProjectSublogic ProfSub Sign where
    projectSublogic = prSign

instance MinSublogic ProfSub SymbItems where
    minSublogic = const topS

instance MinSublogic ProfSub SymbMapItems where
    minSublogic = const topS

instance MinSublogic ProfSub Entity where
    minSublogic = const topS

instance MinSublogic ProfSub OntologyDocument where
    minSublogic = profilesAndSublogic

instance ProjectSublogicM ProfSub SymbItems where
    projectSublogicM = const Just

instance ProjectSublogicM ProfSub SymbMapItems where
    projectSublogicM = const Just

instance ProjectSublogicM ProfSub Entity where
    projectSublogicM = const Just

instance ProjectSublogic ProfSub OntologyDocument where
    projectSublogic = prOntDoc
-}