{- |
Module      :  $Header$
Copyright   :  (c) University of Cambridge, Cambridge, England
               adaption (c) Till Mossakowski, Uni Bremen 2002-2004
Licence     :  similar to LGPL, see HetCATS/LICENCE.txt or LIZENZ.txt

Maintainer  :  hets@tzi.de
Stability   :  provisional
Portability :  portable

   Printing functions for Isabelle logic.
-}

module Isabelle.IsaPrint where

import Common.Id 
import Common.PrettyPrint
import Common.Lib.Pretty
import qualified Common.Lib.Map as Map

import Isabelle.IsaSign

import Data.Char
import Data.Tree

import Debug.Trace

------------------- Id translation functions -------------------

showIsa :: Id -> String
showIsa = transString . flip showPretty ""

showIsaSid :: SIMPLE_ID -> String
showIsaSid = transString . flip showPretty ""

-- disambiguation of overloaded ids
showIsaI :: Id -> Int -> String
showIsaI ident i = showIsa ident ++ "_" ++ show i

------------------- Printing functions -------------------

instance Show Sentence where
  show s = show (senTerm s)

instance PrintLaTeX Sentence where
    printLatex0 = printText0

instance PrettyPrint Sentence where
    printText0 _ = ptext . show

instance Show Typ where
  show = showTyp 1000

showTyp _ (Type (t,[])) = t
showTyp pri (Type ("fun",[s,t])) = 
  bracketize (pri<=10) (showTyp 10 s ++ " => " ++ showTyp 11 t)
showTyp pri (Type ("*",[s,t])) =
  showTyp pri s ++ " * " ++ showTyp pri t
showTyp pri (Type (t,(arg:args))) = "("++show arg++concat (map ((" "++).show) args)++")"++t
showTyp pri (TFree (v,_)) = v

instance Show TypeSig where
  show tysig =
    if Map.isEmpty (tycons tysig) then ""
     else Map.foldWithKey showTycon "" (tycons tysig) 
     where showTycon t arity rest =
             "typedecl "++
             (if arity>0 then "("++concat (map ((" 'a"++).show) [1..arity])++")"
               else "") 
            ++ show t
            ++"\n"++rest

instance Show Term where
  show = showTerm

showTerm :: Term -> String
showTerm (Const (c,_)) = c
showTerm (Free (v,_)) = v
showTerm (Abs (v,_,t)) = "(% "++showTerm v++" . "++showTerm t++")"
showTerm (Abs (v,_,t)) = "(% "++showTerm v++" . "++showTerm t++")"
showTerm (Const ("All",_) `App` Abs (v,ty,t)) = 
   ("! "++showTerm v++" :: "++show ty++" . "++showTerm t)
showTerm (Const ("Ex",_) `App` Abs (v,_,t)) = 
   ("? "++showTerm v++" . "++showTerm t)
showTerm (Const ("Ex1",_) `App` Abs (v,_,t)) = 
   ("?! "++showTerm v++" . "++showTerm t)
showTerm t = show(toPrecTree t)


-- term annotated with precedence
data PrecTerm = PrecTerm Term Precedence deriving (Show)
type Precedence = Int

pseudoPrec :: Term -> PrecTerm
pseudoPrec t = PrecTerm t 0

appPrec :: Term -> PrecTerm
appPrec t = PrecTerm t 5

eqvPrec :: Term -> PrecTerm
eqvPrec t = PrecTerm t 7

eqPrec :: Term -> PrecTerm
eqPrec t = PrecTerm t 10

andPrec :: Term -> PrecTerm
andPrec t = PrecTerm t 20

orPrec :: Term -> PrecTerm
orPrec t = PrecTerm t 30

implPrec :: Term -> PrecTerm
implPrec t = PrecTerm t 40

noPrec :: Term -> PrecTerm
noPrec t = PrecTerm t (-10)

toPrecTree :: Term -> Tree PrecTerm
toPrecTree t =
--  trace ("[sT] "++st t++"\n") (
  case t of
    (t1 `App` t2) ->
      case t1 of 
        Const ("op <=>",typ) `App` t3 -> Node (eqvPrec (Const ("op =",typ))) [toPrecTree t3, toPrecTree t2] 
        Const ("op =",typ) `App` t3 -> Node (eqPrec (Const ("op =",typ))) [toPrecTree t3, toPrecTree t2] 
        Const ("op &",typ) `App` t3 -> Node (andPrec (Const ("op &",typ))) [toPrecTree t3, toPrecTree t2] 
        Const ("op |",typ) `App` t3 -> Node (orPrec (Const ("op |",typ))) [toPrecTree t3, toPrecTree t2] 
        Const ("op -->",typ) `App` t3 -> Node (implPrec (Const ("op -->",typ))) [toPrecTree t3, toPrecTree t2] 
        Const (c,typ) `App` t3 -> Node (appPrec (Const (c,typ))) [toPrecTree t3, toPrecTree t2] 
        _ ->  Node (pseudoPrec (Const ("dummy",dummyT))) [toPrecTree t1, toPrecTree t2] 
-- t1 `App` t2
    _ -> Node (noPrec t) []
  --)

instance Show (Tree PrecTerm) where
  show = showPTree

showPTree :: Tree PrecTerm -> String
showPTree (Node (PrecTerm term prec) []) = showTerm term
showPTree (Node (PrecTerm term prec) annos) = 
-- trace ("[showPTree] "++st term++"\n") (
  let leftChild = head annos
      rightChild = last annos
  in
    case term of
      Const ("op =",_) -> infixP prec "=" leftChild rightChild
      Const ("op &",_) -> infixP prec "&" leftChild rightChild
      Const ("op |",_) -> infixP prec "|" leftChild rightChild
      Const ("op -->",_) -> infixP prec "-->" leftChild rightChild
      Const ("dummy",_) -> simpleInfix prec leftChild rightChild
      Const ("Pair",_) -> pair leftChild rightChild
      Const (c,_) -> prefixP prec c leftChild rightChild
      _ -> showTerm term
  -- )

infixP :: Precedence -> String -> Tree PrecTerm -> Tree PrecTerm -> String
infixP pAdult stAdult leftChild rightChild 
    | (pAdult < (prec leftChild)) && (pAdult < (prec rightChild)) = 
          "("++showPTree leftChild++") "++stAdult++" ("++showPTree rightChild++")"
    | pAdult < (prec leftChild) = 
          "("++showPTree leftChild++") "++stAdult++showPTree rightChild
    | pAdult < (prec rightChild) = 
          showPTree leftChild++" "++stAdult++" ("++showPTree rightChild++")"
    | otherwise = showPTree leftChild++" "++stAdult++" "++showPTree rightChild

prefixP :: Precedence -> String -> Tree PrecTerm -> Tree PrecTerm -> String
prefixP pAdult stAdult leftChild rightChild 
    | (pAdult < (prec leftChild)) && (pAdult < (prec rightChild)) = 
          stAdult++" ("++showPTree leftChild++") ("++showPTree rightChild++")"
    | pAdult < (prec leftChild) = 
          stAdult++" ("++showPTree leftChild++") "++showPTree rightChild
    | pAdult < (prec rightChild) = 
          stAdult++" "++showPTree leftChild++" ("++showPTree rightChild++")"
    | otherwise = stAdult++" "++showPTree leftChild++" "++showPTree rightChild

simpleInfix :: Precedence -> Tree PrecTerm -> Tree PrecTerm -> String
simpleInfix pAdult leftChild rightChild 
    | (pAdult < (prec leftChild)) && (pAdult < (prec rightChild)) = 
          "(("++showPTree leftChild++") ("++showPTree rightChild++"))"
    | pAdult < (prec leftChild) = 
          "(("++showPTree leftChild++") "++showPTree rightChild++")"
    | pAdult < (prec rightChild) = 
          "("++showPTree leftChild++" ("++showPTree rightChild++"))"
    | otherwise =  "("++showPTree leftChild++" "++showPTree rightChild++")"

pair :: Tree PrecTerm -> Tree PrecTerm -> String
pair leftChild rightChild = "("++showPTree leftChild++", "++showPTree rightChild++")"

prec (Node (PrecTerm _ p) _) = p


-- Not, =, and, or, -->: Absteigende Prio, alle rechtsassoz (ausser Not)

--sT t = trace ("[sT] "++st t++"\n") (showTerm 1000 t)

st (Const (c,_)) =  "Const ("++c++")"
st (Free (v,_)) = "Free ("++v++")"
st (Abs (v,_,t)) = "%"++showTerm v++" . "++showTerm t
st (Abs (v,_,t)) = "%"++showTerm v++" . "++showTerm t
st (Const ("All",_) `App` Abs (v,ty,t)) = 
   ("! "++showTerm v++" :: "++show ty++" . "++showTerm t)
st (Const ("Ex",_) `App` Abs (v,_,t)) = 
   ( "? "++showTerm v++" . "++showTerm t)
st (Const ("Ex1",_) `App` Abs (v,_,t)) = 
   ( "?! "++showTerm v++" . "++showTerm t)
st (t1 `App` t2) = "App(["++st t1++"],["++st t2++"])"


instance Show Sign where
  show sig =
    baseSig sig ++":\n"++
    shows (tsig sig) (showDataTypeDefs (dataTypeTab sig))
      ++ (showsConstTab (constTab sig))
    where
    showsConstTab tab =
     if Map.isEmpty tab then ""
      else "consts\n" ++ Map.foldWithKey showConst "" tab
    showConst c t rest = show c ++ " :: " ++ "\"" ++ show t ++ "\"\n" ++ rest
    showDataTypeDefs dtDefs = concat $ map showDataTypeDef dtDefs
    showDataTypeDef [] = ""
    showDataTypeDef (dt:dts) = 
       "datatype " ++ showDataType dt
       ++ (concat $ map (("and "++) . showDataType) dts)
    showDataType (t,op:ops) =
       show t ++ " = " ++ showOp op 
       ++ (concat $ map ((" | "++) . showOp) ops)
    showOp (opname,args) =
       opname ++ (concat $ map (((" ")++) . show) args)


instance PrettyPrint Sign where
    printText0 _ = ptext . show

instance PrintLaTeX Sign where
    printLatex0 = printText0


bracketize :: Bool -> String -> String
bracketize b s = if b then "("++s++")" else s

isIsaChar :: Char -> Bool
isIsaChar c = (isAlphaNum c && isAscii c) || c `elem` "_'"


replaceChar1 :: Char -> String
replaceChar1 c | isIsaChar c = [c] 
               | otherwise = replaceChar c++"__"

transString :: String -> String
transString "" = "X"
transString (c:s) = 
  --trace ("[transString] "++ show (c:s)++"\n\n") 
   ((if isAlpha c && isAscii c then [c] else 'X':replaceChar1 c)
     ++ (concat $ map replaceChar1 s))

-- Replacement of special characters

replaceChar :: Char -> String
replaceChar '\t' = "_"
replaceChar '\n' = "_"
replaceChar '\r' = "_"
replaceChar ' ' = "_"
replaceChar '!' = "Exclam"
replaceChar '\"' = "_"
replaceChar '#' = "Sharp"
replaceChar '$' = "Dollar"
replaceChar '%' = "Percent"
replaceChar '&' = "Amp"
replaceChar '(' = "OBra"
replaceChar ')' = "CBra"
replaceChar '*' = "x"
replaceChar '+' = "Plus"
replaceChar ',' = "Comma"
replaceChar '-' = "Minus"
replaceChar '.' = "Dot"
replaceChar '/' = "Div"
replaceChar ':' = "Colon"
replaceChar ';' = "Semi"
replaceChar '<' = "Lt"
replaceChar '=' = "Eq"
replaceChar '>' = "Gt"
replaceChar '?' = "Q"
replaceChar '@' = "At"
replaceChar '[' = "_"
replaceChar '\\' = "Back"
replaceChar ']' = "_"
replaceChar '^' = "Hat"
replaceChar '`' = "'"
replaceChar '{' = "Cur"
replaceChar '|' = "Bar"
replaceChar '}' = "Ruc"
replaceChar '~' = "Tilde"
replaceChar '\128' = "A1"
replaceChar '\129' = "A2"
replaceChar '\130' = "A3"
replaceChar '\131' = "A4"
replaceChar '\132' = "A5"
replaceChar '\133' = "A6"
replaceChar '\134' = "AE"
replaceChar '\135' = "C"
replaceChar '\136' = "E1"
replaceChar '\137' = "E2"
replaceChar '\138' = "E3"
replaceChar '\139' = "E4"
replaceChar '\140' = "I1"
replaceChar '\141' = "I2"
replaceChar '\142' = "I3"
replaceChar '\143' = "I4"
replaceChar '\144' = "D1"
replaceChar '\145' = "N1"
replaceChar '\146' = "O1"
replaceChar '\147' = "O2"
replaceChar '\148' = "O3"
replaceChar '\149' = "O4"
replaceChar '\150' = "O5"
replaceChar '\151' = "x"
replaceChar '\152' = "O"
replaceChar '\153' = "U1"
replaceChar '\154' = "U2"
replaceChar '\155' = "U3"
replaceChar '\156' = "U4"
replaceChar '\157' = "Y"
replaceChar '\158' = "F"
replaceChar '\159' = "ss"
replaceChar '\160' = "_"
replaceChar '�' = "SpanishExclam"
replaceChar '�' = "c"
replaceChar '�' = "Lb"
replaceChar '�' = "o"
replaceChar '�' = "Yen"
replaceChar '�' = "Bar1"
replaceChar '�' = "Paragraph"
replaceChar '�' = "\""
replaceChar '�' = "Copyright"
replaceChar '�' = "a1"
replaceChar '�' = "\""
replaceChar '�' = "not"
replaceChar '�' = "Minus1"
replaceChar '�' = "Regmark"
replaceChar '�' = "_"
replaceChar '�' = "Degree"
replaceChar '�' = "Plusminus"
replaceChar '�' = "2"
replaceChar '�' = "3"
replaceChar '�' = "'"
replaceChar '�' = "Mu"
replaceChar '�' = "q"
replaceChar '�' = "Dot"
replaceChar '�' = "'"
replaceChar '�' = "1"
replaceChar '�' = "2"
replaceChar '�' = "\""
replaceChar '�' = "Quarter"
replaceChar '�' = "Half"
replaceChar '�' = "Threequarter"
replaceChar '�' = "Q"
replaceChar '�' = "A7"
replaceChar '�' = "A8"
replaceChar '�' = "A9"
replaceChar '�' = "A10"
replaceChar '�' = "A11"
replaceChar '�' = "A12"
replaceChar '�' = "AE2"
replaceChar '�' = "C2"
replaceChar '�' = "E5"
replaceChar '�' = "E6"
replaceChar '�' = "E7"
replaceChar '�' = "E8"
replaceChar '�' = "I5"
replaceChar '�' = "I6"
replaceChar '�' = "I7"
replaceChar '�' = "I8"
replaceChar '�' = "D2"
replaceChar '�' = "N2"
replaceChar '�' = "O6"
replaceChar '�' = "O7"
replaceChar '�' = "O8"
replaceChar '�' = "O9"
replaceChar '�' = "O10"
replaceChar '�' = "xx"
replaceChar '�' = "011"
replaceChar '�' = "U5"
replaceChar '�' = "U6"
replaceChar '�' = "U7"
replaceChar '�' = "U8"
replaceChar '�' = "Y"
replaceChar '�' = "F"
replaceChar '�' = "ss"
replaceChar '�' = "a2"
replaceChar '�' = "a3"
replaceChar '�' = "a4"
replaceChar '�' = "a5"
replaceChar '�' = "a6"
replaceChar '�' = "a7"
replaceChar '�' = "ae"
replaceChar '�' = "c"
replaceChar '�' = "e1"
replaceChar '�' = "e2"
replaceChar '�' = "e3"
replaceChar '�' = "e4"
replaceChar '�' = "i1"
replaceChar '�' = "i2"
replaceChar '�' = "i3"
replaceChar '�' = "i4"
replaceChar '�' = "d"
replaceChar '�' = "n"
replaceChar '�' = "o1"
replaceChar '�' = "o2"
replaceChar '�' = "o3"
replaceChar '�' = "o4"
replaceChar '�' = "o5"
replaceChar '�' = "Div1"
replaceChar '�' = "o6"
replaceChar '�' = "u1"
replaceChar '�' = "u2"
replaceChar '�' = "u3"
replaceChar '�' = "u4"
replaceChar '�' = "y5"
replaceChar '�' = "f"
replaceChar '�' = "y"
replaceChar  _ = "_"


