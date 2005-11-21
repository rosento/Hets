{- |
Module      :  $Header$
Copyright   :  (c) University of Cambridge, Cambridge, England
               adaption (c) Till Mossakowski, Uni Bremen 2002-2005
License     :  similar to LGPL, see HetCATS/LICENSE.txt or LIZENZ.txt

Maintainer  :  maeder@tzi.de
Stability   :  provisional
Portability :  portable

translate 'Id' to Isabelle strings
-}

module Isabelle.Translate
    ( showIsaConstT, showIsaConstIT, showIsaTypeT, transConstStringT
    , mkIsaConstT, mkIsaConstIT, transString, isaPrelude, toAltSyntax
    ) where

import Common.Id
import Common.ProofUtils
import Common.GlobalAnnotations
import Common.AS_Annotation

import qualified Common.Lib.Map as Map
import qualified Common.Lib.Set as Set
import qualified Common.Lib.Rel as Rel
import Data.Char

import Isabelle.IsaSign
import Isabelle.IsaStrings

------------------- Id translation functions -------------------
data IsaPreludes = IsaPreludes
    { preTypes :: Map.Map BaseSig (Set.Set String)
    , preConsts :: Map.Map BaseSig (Set.Set String) }

isaPrelude :: IsaPreludes
isaPrelude = IsaPreludes {
  preTypes = Map.fromList
  [(HsHOLCF_thy, types holcfS), (MainHC_thy, types mainS),
   (Main_thy, types mainS), (HOLCF_thy, types holcfS)],
  preConsts = Map.fromList
  [(HsHOLCF_thy, Set.insert "fliftbin" (consts holcfS)),
   (MainHC_thy, foldr Set.insert  (consts mainS)
                  ["pApp","apt","app","defOp","pair"]),
   (Main_thy,  consts mainS), (HOLCF_thy, consts holcfS)]}

toAltSyntax :: GlobalAnnos -> Int -> Id -> Maybe AltSyntax
toAltSyntax ga n i = let
    (precMap, mx) = Rel.toPrecMap $ prec_annos ga
    minPrec = 42
    adjustPrec p = 2 * p + minPrec
    newPlace = "/ _"
    minL = replicate n minPrec
    minL1 = tail minL
    minL2 = tail minL1
    (hd : tl) = getTokenList newPlace i
    tts = concatMap convert tl
    ht = convert hd
    ts = ht ++ tts
    convert = \ Token { tokStr = s } -> if s == newPlace then s
                         else quote s
    (precList, erg) = if isInfix i then case Map.lookup i precMap of
        Just p -> let 
            q = adjustPrec p  
            (l, r) = case Map.lookup i $ assoc_annos ga of
                 Nothing -> (q, q)
                 Just ALeft -> (q, q + 1)
                 Just ARight -> (q + 1, q)
            in (l : minL2 ++ [r], q)
        Nothing -> let q = adjustPrec $ mx + 1 in (q : minL2 ++ [q], minPrec)
      else if begPlace i then let q = adjustPrec $ mx + 3 in (q : minL1 , q)
      else if endPlace i then let q = adjustPrec $ mx + 2 in (minL1 ++ [q], q)
      else (minL, adjustPrec $ mx + 4)
    in if n == 0 then Just $ AltSyntax ts [] 999
       else if isMixfix i then Just $ AltSyntax
                ('(' : (if begPlace i then "_ " else ht) 
                         ++ tts ++ ")") precList erg
       else Just $ AltSyntax 
            (ts ++ "'(" ++ 
                   concat (replicate (n - 1) "_,/ ")
                   ++ "_')") minL $ adjustPrec $ mx + 4 

quote :: String -> String
quote l = case l of
    [] -> l
    c : r -> (if c `elem` "_/'()" then '\'' : [c] 
              else if c `elem` "\\\"" then '\\' : [c] else [c]) ++ quote r

showIsaT1 :: (String -> String) -> Id -> String
showIsaT1 tr ide = let
    rdru = reverse . dropWhile (== '_')
    str = show ide
    in if isInfix2 ide then "XX" ++ tr (rdru $ rdru str) else tr str

showIsaConstT :: Id -> BaseSig -> String
showIsaConstT ide thy = showIsaT1 (transConstStringT thy) ide

-- also pass number of arguments
mkIsaConstT :: GlobalAnnos -> Int -> Id -> BaseSig -> VName
mkIsaConstT = mkIsaConstVName showIsaConstT

mkIsaConstVName :: (Id -> BaseSig -> String) 
                -> GlobalAnnos -> Int -> Id -> BaseSig -> VName
mkIsaConstVName f ga n ide thy = VName 
 { new = f ide thy
 , altSyn = toAltSyntax ga n ide }

showIsaTypeT :: Id -> BaseSig -> String
showIsaTypeT ide thy = showIsaT1 (transTypeStringT thy) ide

-- | add a number for overloading
showIsaConstIT :: Id -> Int -> BaseSig -> String
showIsaConstIT ide i thy = showIsaConstT ide thy ++ "_" ++ show i

mkIsaConstIT :: GlobalAnnos -> Int -> Id -> Int -> BaseSig -> VName
mkIsaConstIT ga n ide i = mkIsaConstVName
                        ( \ ide' -> showIsaConstIT ide' i) ga n ide

transIsaStringT :: Map.Map BaseSig (Set.Set String) -> BaseSig
                -> String -> String
transIsaStringT m i s = let t = transString s in
  if Set.member t $ maybe (error "Isabelle.transIsaStringT") id
         $ Map.lookup i m
  then t ++ "X" else t

transConstStringT :: BaseSig -> String -> String
transConstStringT = transIsaStringT $ preConsts isaPrelude

transTypeStringT :: BaseSig -> String -> String
transTypeStringT  = transIsaStringT $ preTypes isaPrelude

-- | check for legal alphanumeric isabelle characters
isIsaChar :: Char -> Bool
isIsaChar c = isAlphaNum c && isAscii c || c `elem` "_'"

transString :: String -> String
transString str = let
    x = 'X'
    replaceChar1 d | d == x = "YX"  -- code out existing X!
                   | isIsaChar d = [d]
                   | otherwise = replaceChar d ++ [x]
    in case str of
    "" -> [x]
    c : s -> let l = replaceChar1 c in
             (if isDigit c || c `elem` "_'" then [x, c]
             else l) ++ concatMap replaceChar1 s

-- | injective replacement of special characters
replaceChar :: Char -> String
-- <http://www.htmlhelp.com/reference/charset/>
replaceChar c = if isIsaChar c then [c] else let n = ord c in
    if n <= 32 || n >= 127 && n < 160 || n > 255 then "Slash_" ++ show n
    else maybe (error "Isabelle.replaceChar") id $ Map.lookup c charMap
