{-# LANGUAGE MultiParamTypeClasses , RankNTypes , TypeSynonymInstances , FlexibleInstances #-}
module Comorphisms.UMLState2CASL where

import Common.Id
import Common.ProofTree
import Common.AS_Annotation (makeNamed)
import Common.Lib.State

import Data.Set

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
  UMLState ()   BASIC_SPEC EDHML ()    ()       Library  Morphism Token ()      ()
  CL.CASL  CSub CBasic     CForm CSyms CSymMaps CSign    CMor     CSym  CRawSym CProof
  where
    sourceLogic UMLState2CASL = UMLState
    sourceSublogic UMLState2CASL = ()
    targetLogic UMLState2CASL = CL.CASL
    mapSublogic UMLState2CASL () = Just top
    map_theory UMLState2CASL (sign, _) = do
      let
          computeSign = do
            -- CS.addSort CA.NonEmptySorts natSort 
            -- CS.addSort CA.NonEmptySorts evtSort
            -- CS.addSort CA.NonEmptySorts ctrlSort
            return undefined
          csign = execState computeSign (CS.emptySign ())
          g = str2Token "g"
          vars = (attrL sign,g,prime g)
          machine = CA.mkForall [CA.mkVarDecl g confSort] (
                      initP (confVar g) `CA.mkImpl` edhml2CASL vars (lib2EDHML sign)
                    )
      return ( csign
             , [ makeNamed "init" $ initCASL sign
               ] ++ [ makeNamed "machine" m
                    | m <- sepCASL $ edhml2CASL vars (lib2EDHML sign)
                    ]
             )

    map_sentence UMLState2CASL sen = undefined
    map_symbol UMLState2CASL lib tok = singleton $ CS.Symbol (token2Id tok) undefined
