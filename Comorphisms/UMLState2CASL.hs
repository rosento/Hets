{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{- |
TODO: adjust comments
Module      :  ./Comorphisms/Maude2CASL.hs
Description :  Coding of Maude with preorder semantics into CASL
Copyright   :  (c) Adrian Riesco and Uni Bremen 2007
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  ariesco@fdi.ucm.es
Stability   :  experimental
Portability :  non-portable (imports Logic.Logic)

The translating comorphism from Maude with preorder semantics to CASL.
-}

module Comorphisms.UMLState2CASL
    (
     UMLState2CASL (..)
    )
    where

import Common.ProofTree

import Logic.Logic
import Logic.Comorphism

-- UMLState
import qualified UMLState.Logic_UMLState as ULogic

-- CASL
import qualified CASL.Logic_CASL as CLogic
import qualified CASL.AS_Basic_CASL as CBasic
import qualified CASL.Sublogic as CSL
import qualified CASL.Sign as CSign
import qualified CASL.Morphism as CMor

-- | lid of the morphism
data UMLState2CASL = UMLState2CASL deriving Show

instance Language UMLState2CASL where
  language_name UMLState2CASL = "UMLState2CASL"

mapSig sign = ()
              { CSign.predMap = Set.fold (`MapSet.insert` CSign.PredType [])
                                MapSet.empty $ PSign.items sign }

mapTheory (sig, form) = Result.Result [] $
    Just (mapSig sig, undefined)

instance Comorphism UMLState2CASL
    ULogic.UMLState
    ()
    PBasic.BASIC_SPEC
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    CLogic.CASL
    CSL.CASL_Sublogics
    CLogic.CASLBasicSpec
    CBasic.CASLFORMULA
    CBasic.SYMB_ITEMS
    CBasic.SYMB_MAP_ITEMS
    CSign.CASLSign
    CMor.CASLMor
    CSign.Symbol
    CMor.RawSymbol
    ProofTree
    where
      sourceLogic UMLState2CASL = ULogic.UMLState
      sourceSublogic UMLState2CASL = undefined
      targetLogic UMLState2CASL = CLogic.CASL
      mapSublogic UMLState2CASL = undefined
      map_theory UMLState2CASL = mapTheory
      is_model_transportable UMLState2CASL = undefined
      map_symbol UMLState2CASL _ = undefined
      map_sentence UMLState2CASL = undefined
      map_morphism UMLState2CASL = undefined
      has_model_expansion UMLState2CASL = undefined
      is_weakly_amalgamable UMLState2CASL = undefined
      isInclusionComorphism UMLState2CASL = undefined
