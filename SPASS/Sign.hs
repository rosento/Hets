{- |
Module      :  $Header$
Description :  Data structures representing SPASS signatures.
Copyright   :  (c) Rene Wagner, Uni Bremen 2005
License     :  similar to LGPL, see HetCATS/LICENSE.txt or LIZENZ.txt

Maintainer  :  luettich@tzi.de
Stability   :  provisional
Portability :  unknown

Data structures representing SPASS signatures.
   Refer to <http://spass.mpi-sb.mpg.de/webspass/help/syntax/dfgsyntax.html>
   for the SPASS syntax documentation.

-}

module SPASS.Sign where

import Char

import Common.AS_Annotation
import Common.DefaultMorphism

import qualified Common.Lib.Map as Map
import qualified Common.Lib.Set as Set
import qualified Common.Lib.Rel as Rel

-- * Externally used data structures

type SortMap = Map.Map SPIdentifier (Maybe Generated)

type FuncMap = Map.Map SPIdentifier (Set.Set ([SPIdentifier], SPIdentifier))

type PredMap = Map.Map SPIdentifier (Set.Set [SPIdentifier])


{- |
  This Signature data type will be translated to the SPASS data types
  internally.

  sortRel contains the sorts relation. For each sort we need to know
  if it is a generated sort and if so by which functions it is
  possibly freely generated (sortMap).

  For each function the types of all arguments and the return type
  must be known (funcMap). The same goes for the arguments of a predicate
  (predMap).
-}
data Sign = Sign { sortRel :: Rel.Rel SPIdentifier
                 , sortMap :: SortMap
                 , funcMap :: FuncMap
                 , predMap :: PredMap 
                 } deriving (Eq, Show)

{- |
  Sorts can be (freely) generated by a set of functions.
-}
data Generated = Generated { freely :: Bool
                           , byFunctions :: [SPIdentifier]
                           } deriving (Eq, Ord, Show)

{- |
  Creates an empty Signature.
-}
emptySign :: Sign
emptySign = Sign { sortRel = Rel.empty
                 , sortMap = Map.empty
                 , funcMap = Map.empty
                 , predMap = Map.empty
                 }

{- | 
'checkArities'
checks if the signature has only overloaded symbols with the same arity
-}
checkArities :: Sign -> Bool
checkArities s = 
    checkPredArities (predMap s) && checkFuncArities (funcMap s)

checkPredArities :: PredMap -> Bool
checkPredArities = Map.fold checkSet True 
    where checkSet s bv = bv && not (Set.null s) &&
                  all (\ x -> length x == length hd) tl
                      where hd : tl = Set.toList s

checkFuncArities :: FuncMap -> Bool
checkFuncArities = checkPredArities . mapToPredMap
    where mapToPredMap = Map.map (Set.map fst)
{- |
  A Sentence is a SPASS Term.
-}
type Sentence = SPTerm

{- |
  We use the DefaultMorphism for SPASS.
-}
type SPASSMorphism = DefaultMorphism Sign

{- |
  A SPASS Identifier is a String for now. See also 'checkIdentifier' function
  below. Might need conversion functions as well.
-}
type SPIdentifier = String

{- |
  SPASS Identifiers may contain letters, digits, and underscores only.
-}
checkIdentifier :: String-> Bool
checkIdentifier "" = False
checkIdentifier xs@(x:_) = and ((x /= '_') : map checkSPChar xs)

{- |
Allowed SPASS characters are letters, digits, and underscores.
-}
checkSPChar :: Char -> Bool
checkSPChar c = isAlphaNum c || '_' == c


-- * Internal data structures

-- ** SPASS Problems

{- |
  A SPASS problem consists of a description and a logical part. The optional
  settings part hasn't been implemented yet.
-}
data SPProblem =
        SPProblem { identifier  :: SPIdentifier,
                    description :: SPDescription,
                    logicalPart :: SPLogicalPart,
                    settings    :: [SPSetting]
                    }
      deriving (Eq, Ord, Show)

-- ** SPASS Logical Parts

{- |
  A SPASS logical part consists of a symbol list, a declaration list, and a
  set of formula lists. Support for clause lists and proof lists hasn't
  been implemented yet.
-}
data SPLogicalPart =
        SPLogicalPart { symbolList      :: Maybe SPSymbolList,
                        declarationList :: [SPDeclaration],
                        formulaLists    :: [SPFormulaList] --,
--                        clauseLists :: [SPClauseList],
--                        proofLists :: [SPProofList]
                        }
      deriving (Eq, Ord, Show)

-- *** Symbol Lists

{- |
  All non-predefined signature symbols must be declared as part of a SPASS
  symbol list. 
-}
data SPSymbolList =
        SPSymbolList { functions   :: [SPSignSym],
                       predicates  :: [SPSignSym],
                       sorts       :: [SPSignSym],
                       operators   :: [SPSignSym],
                       quantifiers :: [SPSignSym] }
      deriving (Eq, Ord, Show)

{- |
  Creates an empty SPASS Symbol List.
-}
emptySymbolList :: SPSymbolList
emptySymbolList =
        SPSymbolList { functions   = [],
                       predicates  = [],
                       sorts       = [],
                       operators   = [],
                       quantifiers = [] }

{- |
  A common data type used for all signature symbols.
-}
data SPSignSym =
        SPSignSym { sym    :: SPIdentifier,
                    arity  :: Int }
      | SPSimpleSignSym SPIdentifier
      deriving (Eq, Ord, Show)

-- *** Declarations

{- |
  SPASS Declarations allow the introduction of sorts.
-}
data SPDeclaration =
        SPSubsortDecl { sortSymA :: SPIdentifier,
                        sortSymB :: SPIdentifier }
      | SPTermDecl { termDeclTermList :: [SPTerm],
                     termDeclTerm     :: SPTerm }
      | SPSimpleTermDecl SPTerm
      | SPPredDecl { predSym  :: SPIdentifier,
                     sortSyms :: [SPIdentifier] }
      | SPGenDecl { sortSym         :: SPIdentifier,
                    freelyGenerated :: Bool,
                    funcList        :: [SPIdentifier]}
      deriving (Eq, Ord, Show)

-- *** Formula List

{- |
  SPASS Formula List
-}
data SPFormulaList = 
        SPFormulaList { originType :: SPOriginType,
                        formulae   :: [SPFormula] }
      deriving (Eq, Ord, Show)

{- |
  There are axiom formulae and conjecture formulae.
-}
data SPOriginType =
        SPOriginAxioms
      | SPOriginConjectures
      deriving (Eq, Ord, Show)

-- *** Formulae And Terms

{- |
  A SPASS Formula is modelled as a Named SPTerm for now. This doesn't reflect
  the fact that the SPASS syntax lists both term and label as optional.
-}
type SPFormula = Named SPTerm

{- |
  A SPASS Term.
-}
data SPTerm = 
        SPQuantTerm { quantSym     :: SPQuantSym,
                      termTermList :: [SPTerm],
                      termTerm     :: SPTerm }
      | SPSimpleTerm SPSymbol
      | SPComplexTerm { symbol    :: SPSymbol,
                        arguments :: [SPTerm]}
      deriving (Eq, Ord, Show)

{- |
  SPASS Quantifier Symbols.
-}
data SPQuantSym =
        SPForall
      | SPExists
      | SPCustomQuantSym SPIdentifier
      deriving (Eq, Ord, Show)

{- |
  SPASS Symbols.
-}
data SPSymbol =
        SPEqual
      | SPTrue 
      | SPFalse 
      | SPOr 
      | SPAnd
      | SPNot
      | SPImplies
      | SPImplied
      | SPEquiv
      | SPCustomSymbol SPIdentifier
      deriving (Eq, Ord, Show)

-- ** SPASS Desciptions

{- |
  A description is mandatory for a SPASS problem. It has to specify at least
  a 'name', the name of the 'author', the 'status' (see also 'SPLogState' below),
  and a (verbose) description.
-}
data SPDescription =
        SPDescription { name    :: String,
                        author  :: String,
                        version :: Maybe String,
                        logic   :: Maybe String,
                        status  :: SPLogState,
                        desc    :: String,
                        date    :: Maybe String}
      deriving (Eq, Ord, Show)

{- |
  The state of a SPASS problem can be satisfiable, unsatisfiable, or unknown.
-}
data SPLogState =
        SPStateSatisfiable
      | SPStateUnsatisfiable
      | SPStateUnknown
      deriving (Eq, Ord, Show)

-- ** SPASS Settings

{- |
  We only support one of the three types mentioned here:
  http://spass.mpi-sb.mpg.de/webspass/help/options.html
-}

data SPSetting = SPFlag String String
     deriving (Eq,Ord,Show)