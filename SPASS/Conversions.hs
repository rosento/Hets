{- |
Module      :  $Header$
Copyright   :  (c) Rene Wagner, Uni Bremen 2005
Licence     :  similar to LGPL, see HetCATS/LICENCE.txt or LIZENZ.txt

Maintainer  :  rwagner@tzi.de
Stability   :  provisional
Portability :  unknown

   Functions to convert to internal SP* data structures.

-}

module SPASS.Conversions where

import Maybe

import qualified Common.Lib.Map as Map
import qualified Common.Lib.Set as Set
import qualified Common.Lib.Rel as Rel

import SPASS.Sign

signToSPLogicalPart :: Sign -> SPLogicalPart
signToSPLogicalPart s = SPLogicalPart {symbolList = sList,
                                       declarationList = decList,
                                       formulaLists = []}
  where
    sList = if Rel.null (sortRel s) && Map.null (funcMap s) && Map.null (predMap s)
              then Nothing
              else Just emptySymbolList { functions = map (\(f, t) -> SPSignSym {sym = f, arity = length (fst t)}) (Map.toList (funcMap s)),
                                          predicates = map (\(p, t) -> SPSignSym {sym = p, arity = length t}) (Map.toList (predMap s)),
                                          sorts = map SPSimpleSignSym (Set.toList (Rel.nodes (sortRel s)))}

    decList = subsortDecl ++ termDecl ++ predDecl ++ genDecl

    subsortDecl = map (\(a, b) -> SPSubsortDecl {sortSymA = a, sortSymB = b}) (Rel.toList (Rel.transReduce (sortRel s)))

    termDecl = map (\(fsym, (args, ret)) -> SPTermDecl {termDeclTermList = map (\(t, x) -> SPComplexTerm {symbol = SPCustomSymbol t, arguments = [SPSimpleTerm (SPCustomSymbol ('x' : (show x)))]}) (zip args [1..]), termDeclTerm = SPComplexTerm {symbol = SPCustomSymbol ret, arguments = [SPComplexTerm {symbol = SPCustomSymbol fsym, arguments = map (SPSimpleTerm . SPCustomSymbol . ('x':) . show . snd) (zip args [1..])}]}}) (Map.toList (funcMap s))

    predDecl = map (\(p, t) -> SPPredDecl {predSym = p, sortSyms = t}) (Map.toList (predMap s))

    genDecl = map (\(ssym, Just gen) -> SPGenDecl {sortSym = ssym, freelyGenerated = freely gen, funcList = byFunctions gen}) (filter (isJust . snd) (Map.toList (sortMap s)))

