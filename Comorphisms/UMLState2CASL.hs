{-# LANGUAGE MultiParamTypeClasses , RankNTypes , TypeSynonymInstances , FlexibleInstances #-}
module Comorphisms.UMLState2CASL where

import Common.Id
import Common.ProofTree

import Logic.Logic
import Logic.Comorphism

import UMLState.Logic_UMLState

import qualified CASL.Logic_CASL as CL
import qualified CASL.AS_Basic_CASL as CA
import qualified CASL.Sublogic as CSL
import qualified CASL.Formula as CF
import qualified CASL.Morphism as CM
import qualified CASL.Sign as CS


type CSub     = CSL.CASL_Sublogics
type CBasic   = CL.CASLBasicSpec
type CForm    = CA.CASLFORMULA
type CSyms    = CA.SYMB_ITEMS
type CSymMaps = CA.SYMB_MAP_ITEMS
type CSign    = CS.CASLSign
type CMor     = CM.CASLMor
type CSym     = CS.Symbol
type CRawSym  = CM.RawSymbol
type CProof   = ProofTree

data UMLState2CASL = UMLState2CASL deriving Show

instance Language UMLState2CASL where
  language_name UMLState2CASL = "UMLState2CASL"
  description   UMLState2CASL = "TODO"

instance Comorphism UMLState2CASL
  UMLState ()   BASIC_SPEC SEN_ITEMS ()    ()       Sign  ()   Token ()      ()
  CL.CASL  CSub CBasic     CForm     CSyms CSymMaps CSign CMor CSym  CRawSym CProof
  where
    sourceLogic UMLState2CASL = UMLState
    sourceSublogic UMLState2CASL = ()
    map_theory UMLState2CASL (sign, sens) = undefined
