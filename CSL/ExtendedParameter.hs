{- |
Module      :  $Header$
Description :  Handling of extended parameters
Copyright   :  (c) Ewaryst Schulz, DFKI Bremen 2010
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  ewaryst.schulz@dfki.de
Stability   :  experimental
Portability :  portable

This module defines an ordering on extended parameters and other analysis tools.

Extended parameters may be based on one of the following
     relations:

>    =, <=, >=, !=, <, >, -|

     We reduce the relations to one of the Data items in 'NormEP'.  We
     handle @-|@ as @*@ for comparison and range computations, i.e.,
     we remove it, and @< n@ is turned into @<= n-1@
     (or @'LeftOf' n-1@) and accordingly for @>@.

     We could work more generally (and we do it) as in
     CSL.GeneralExtendedParameter .

 -}

module CSL.ExtendedParameter where

import CSL.EPBasic
import CSL.TreePO
import CSL.AS_BASIC_CSL
import Common.Id (tokStr)

-- ----------------------------------------------------------------------
-- * Datatypes for efficient Extended Parameter comparison
-- ----------------------------------------------------------------------

-- | Normalized representation of an extended param relation
data NormEP = LeftOf | RightOf | Equal | Except deriving Eq

instance Show NormEP where
    show LeftOf = "<="
    show RightOf = ">="
    show Equal = "="
    show Except = "/="

type EPExp = (NormEP, APInt)

showEP :: (String, EPExp) -> String
showEP (s, (n, i)) = s ++ show n ++ show i

-- | Conversion function into the more efficient representation.
toEPExp :: EXTPARAM -> Maybe (String, EPExp)
toEPExp (EP t r i) =
    case r of 
      "<=" -> Just (tokStr t, (LeftOf, i))
      "<" -> Just (tokStr t, (LeftOf, i-1))
      ">=" -> Just (tokStr t, (RightOf, i))
      ">" -> Just (tokStr t, (RightOf, i+1))
      "=" -> Just (tokStr t, (Equal, i))
      "!=" -> Just (tokStr t, (Except, i))
      "-|" -> Nothing
      _ -> error $ "toEPExp: unsupported relation: " ++ r


toBoolRep :: String -> EPExp -> BoolRep
toBoolRep s (x, i) = Pred (show x) [s, show i]

-- ----------------------------------------------------------------------
-- * Extended Parameter comparison
-- ----------------------------------------------------------------------


-- | Compares two 'EPExp': They are uncompareable if they overlap or are disjoint.
compareEP :: EPExp -> EPExp -> EPCompare
compareEP ep1@(r1, n1) ep2@(r2, n2)
    | r1 == r2 = compareSameRel r1 n1 n2
    | otherwise =
        case (r1, r2) of
          (Equal, Except) | n1 == n2 -> Incomparable Disjoint
                          | otherwise -> Comparable LT
          (Equal, LeftOf) | n1 > n2 -> Incomparable Disjoint
                          | otherwise -> Comparable LT
          (Equal, RightOf) | n1 < n2 -> Incomparable Disjoint
                           | otherwise -> Comparable LT
          (Except, LeftOf) | n1 > n2 -> Comparable GT
                           | otherwise -> Incomparable Overlap
          (Except, RightOf) | n1 < n2 -> Comparable GT
                            | otherwise -> Incomparable Overlap
          (LeftOf, RightOf) | n1 < n2 -> Incomparable Disjoint
                            | otherwise -> Incomparable Overlap
          _ -> swapCmp $ compareEP ep2 ep1


compareSameRel :: NormEP -> APInt -> APInt -> EPCompare
compareSameRel r n1 n2
    | n1 == n2 = Comparable EQ
    | otherwise =
        case r of
          LeftOf -> Comparable (compare n1 n2)
          RightOf -> Comparable (compare n2 n1)
          Equal -> Incomparable Disjoint
          Except -> Incomparable Overlap

