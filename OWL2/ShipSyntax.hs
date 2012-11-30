{- |
Module      :  $Header$
Copyright   :  (c) C. Maeder DFKI GmbH
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  portable

different pretty printing for the SHIP tool
-}

module OWL2.ShipSyntax where

import OWL2.AS

import Common.Doc
import Common.DocUtils
import Common.Parsec

import Control.Monad

import Data.Char

import Text.ParserCombinators.Parsec

data Concept
  = CName String
  | NominalC String
  | NotC Concept
  | JoinedC (Maybe JunctionType) [Concept] -- Nothing denotes disjoint union
  | Quant (Either QuantifierType (CardinalityType, Int)) Role Concept
  deriving (Show, Eq, Ord)

topC :: Concept
topC = CName "T"

data NotOrInverse = NotR | InvR deriving (Show, Eq, Ord)

data Role
  = RName String
  | NominalR String String
  | UnOp NotOrInverse Role
  | JoinedR (Maybe JunctionType) [Role] -- Nothing denotes composition!
  deriving (Show, Eq, Ord)

topR :: Role
topR = RName "TxT"

botR :: Role
botR = UnOp NotR topR

ppJunction :: Maybe JunctionType -> Doc
ppJunction mt = text $ case mt of
  Just t -> case t of
    UnionOf -> "+ "
    IntersectionOf -> "& "
  Nothing -> "<+> "

pppNegConcept :: Concept -> Doc
pppNegConcept c = case c of
    JoinedC {} -> parens
    _ -> id
  $ ppConcept c

pppConcept :: Bool -> Maybe JunctionType -> Concept -> Doc
pppConcept notLast mt c = case c of
    Quant {} | notLast -> parens
    JoinedC mti _
      | mti /= mt && maybe True (const $ maybe False (== UnionOf) mt) mti
        -> parens
    _ -> id
  $ ppConcept c

ppConcept :: Concept -> Doc
ppConcept co = case co of
  CName s -> (if co == topC then keyword else text) s
  NominalC s -> braces $ text s
  NotC c -> text "~" <> pppNegConcept c
  JoinedC t l -> case reverse l of
    [] -> ppConcept $ if t == Just IntersectionOf then topC else NotC topC
    f : r -> fsep . prepPunctuate (ppJunction t)
      . reverse $ pppConcept False t f : map (pppConcept True t) r
  Quant q r c -> fsep [(case q of
    Left v -> keyword (showQuant v)
    Right (n, i) -> text (showCard n) <+> text (show i)
    ) <+> ppRole r, dot <+> ppConcept c]

showQuant :: QuantifierType -> String
showQuant v = case v of
      AllValuesFrom -> "all"
      SomeValuesFrom -> "ex"

showCard :: CardinalityType -> String
showCard n = case n of
      MinCardinality -> ">="
      MaxCardinality -> "<="
      ExactCardinality -> "="

showSame :: SameOrDifferent -> String
showSame s = case s of
  Same -> "=="
  Different -> "!="

pppRole :: Maybe JunctionType -> Role -> Doc
pppRole mt r = case r of
    JoinedR (Just ti) _
      | maybe True (\ to -> ti /= to && ti == UnionOf) mt
        -> parens
    _ -> id
  $ ppRole r

ppRole :: Role -> Doc
ppRole ro = case ro of
  RName s -> (if ro == topR then keyword else text) s
  NominalR s t -> braces $ parens $ text s <> comma <+> text t
  UnOp o r -> (case o of
    NotR -> text "~"
    InvR -> keyword "inv") <> (case r of
      JoinedR {} -> parens
      _ | o == InvR -> parens
      _ -> id) (ppRole r)
  JoinedR t l -> case l of
    [] -> ppRole $ if t /= Just UnionOf then topR else botR
    _ -> fsep . prepPunctuate (ppJunction t) $ map (pppRole t) l

skip :: CharParser st ()
skip = forget $ many $ single (satisfy isSpace) <|> nestedComment "/*" "*/"
       <|> (string "%" <|> tryString "//") <++> many (noneOf "\n")

myLetter :: CharParser st Char
myLetter = satisfy $ \ c -> isAlphaNum c || elem c "_"

nominal :: CharParser st String
nominal = reserved ["all", "ex", "inv", "not"] (many1 myLetter) << skip

key :: String -> CharParser st ()
key s = forget $ try $ string s >> notFollowedBy myLetter

quant :: CharParser st QuantifierType
quant = choice $ map (\ a -> key (showQuant a) >> return a)
        [AllValuesFrom, SomeValuesFrom]

card :: CharParser st CardinalityType
card = choice $ map (\ a -> tryString (showCard a) >> return a)
       [MinCardinality, MaxCardinality, ExactCardinality]

quantOrCard :: CharParser st (Either QuantifierType (CardinalityType, Int))
quantOrCard = fmap Left quant
  <|> do
  c <- card << skip
  i <- many1 digit
  return $ Right (c, read i)

skipChar :: Char -> CharParser st ()
skipChar c = char c >> skip

primConcept :: CharParser st Concept
primConcept = do
    q <- quantOrCard << skip
    r <- role
    skipChar '.'
    fmap (Quant q r) concept
  <|> ((key "not" <|> skipChar '~') >> skip >> fmap NotC primConcept)
  <|> braced (fmap NominalC nominal) -- allow more nominals
  <|> parent concept
  <|> fmap CName nominal

braced :: CharParser st a -> CharParser st a
braced p = skipChar '{' >> p << skipChar '}'

parent :: CharParser st a -> CharParser st a
parent p = skipChar '(' >> p << skipChar ')'

binP :: ([a] -> a) -> String -> CharParser st a -> CharParser st a
binP f c p = do
  a <- p
  l <- many $ tryString c >> skip >> p
  return $ if null l then a else f $ a : l

andConcept :: CharParser st Concept
andConcept = binP (JoinedC $ Just IntersectionOf) "&" primConcept

orConcept :: CharParser st Concept
orConcept = binP (JoinedC $ Just UnionOf) "+" andConcept

concept :: CharParser st Concept
concept = binP (JoinedC Nothing) "<+>" orConcept

notOrInv :: CharParser st NotOrInverse
notOrInv = (char '~' >> return NotR)
  <|> (key "inv" >> return InvR)

nomPair :: (String -> String -> a) -> CharParser st a
nomPair f = parent $ liftM2 f nominal $ skipChar ',' >> nominal

primRole :: CharParser st Role
primRole = do
    o <- notOrInv << skip
    fmap (UnOp o) primRole
  <|> braced (nomPair NominalR)
  <|> parent role
  <|> fmap RName nominal


compRole :: CharParser st Role
compRole = binP (JoinedR Nothing) "." primRole

andRole :: CharParser st Role
andRole = binP (JoinedR $ Just IntersectionOf) "&" compRole

role :: CharParser st Role
role = binP (JoinedR $ Just UnionOf) "+" andRole

data EqOrLess = Eq | Less deriving (Show, Eq, Ord)

data RoleType = RoleType Concept Concept deriving (Show, Eq, Ord)

-- a missing rhs may be modelled as "< T" or by no constructors
data ConceptRhs
  = ADTCons [TBoxCons]
  | ConceptRel EqOrLess Concept
  deriving (Show, Eq, Ord)

data TBoxCons = TBoxCons Concept [(Role, Concept)]
  deriving (Show, Eq, Ord)

data TBox
  = ConceptDecl Concept ConceptRhs
  | DisjointCs [Concept]
  deriving (Show, Eq, Ord)

data RBox
  = RoleDecl Role RoleType
  | RoleRel Role EqOrLess Role
  | RoleProp Character Role
  deriving (Show, Eq, Ord)

-- | assertions
data ABox
  = AConcept String Concept
  | ARole String String Role
  | AIndividual String SameOrDifferent String
  deriving (Show, Eq, Ord)

data Box = Box [TBox] [RBox] [ABox]
  deriving (Show, Eq, Ord)

ppEqOrLess :: EqOrLess -> Doc
ppEqOrLess s = case s of
  Eq -> equals
  Less -> less

ppRoleType :: RoleType -> Doc
ppRoleType (RoleType s t) =
  fsep [colon <+> ppConcept s, cross <+> ppConcept t]

ppConceptRhs :: ConceptRhs -> Doc
ppConceptRhs rhs = case rhs of
  ADTCons cs -> vcat $ prepPunctuate (text "| ") $ map ppTBoxCons cs
  ConceptRel o c -> ppEqOrLess o <+> ppConcept c

ppTBoxCons :: TBoxCons -> Doc
ppTBoxCons (TBoxCons c sels) =
  ppConcept c <> if null sels then empty else
    parens . sepByCommas $ map
      (\ (r, a) -> ppRole r <+> colon <+> ppConcept a) sels

ppTBox :: TBox -> Doc
ppTBox b = case b of
  ConceptDecl d m -> ppConcept d <+> ppConceptRhs m
  DisjointCs cs -> keyword "Disjoint" <> parens (sepByCommas $ map ppConcept cs)

ppRBox :: RBox -> Doc
ppRBox b = case b of
  RoleDecl r t -> fsep [ppRole r, ppRoleType t]
  RoleRel r o t -> fsep [ppRole r, ppEqOrLess o <+> ppRole t]
  RoleProp c s -> text (showCharacter c) <> parens (ppRole s)

ppABox :: ABox -> Doc
ppABox b = case b of
  AConcept n c -> text n <+> colon <+> ppConcept c
  ARole s t r -> parens (text s <> comma <+> text t)
    <+> colon <+> ppRole r
  AIndividual s c t -> text s <+> text (showSame c) <+> text t

ppBox :: Box -> Doc
ppBox (Box ts rs as) =
  vcat $ map ppTBox ts ++ text "%RBOX" : map ppRBox rs ++ map ppABox as

showCharacter :: Character -> String
showCharacter c = case c of
    Functional -> "Func"
    InverseFunctional -> "FuncInv"
    Reflexive -> "Ref"
    Irreflexive -> "Irref"
    Symmetric -> "Sym"
    Asymmetric -> "Asym"
    Antisymmetric -> "Dis"
    Transitive -> "Trans"

character :: CharParser st Character
character = choice $ map (\ a -> key (showCharacter a) >> return a)
  [minBound .. maxBound]

eqOrLess :: CharParser st EqOrLess
eqOrLess = (skipChar '=' >> return Eq) <|> (skipChar '<' >> return Less)

tbox :: CharParser st TBox
tbox = (key "Disjoint" >> fmap DisjointCs
        (parent $ concept <:> many (skipChar ',' >> concept)))
  <|> liftM2 ConceptDecl concept
    (liftM2 ConceptRel eqOrLess concept
     <|> fmap ADTCons (sepBy tboxCons $ skipChar '|'))

tboxCons :: CharParser st TBoxCons
tboxCons = liftM2 TBoxCons concept
  . optionL . parent . flip sepBy (skipChar ',')
  . pair role $ skipChar ':' >> concept

rbox :: CharParser st RBox
rbox = liftM2 RoleProp character (parent role)
  <|> try (do
    r <- role
    liftM2 (RoleRel r) eqOrLess role
      <|> fmap (RoleDecl r) (liftM2 RoleType
        (skipChar ':' >> concept)
        $ skipChar '*' >> concept)) -- added try to recognize abox

abox :: CharParser st ABox
abox = liftM2 ($) (nomPair ARole) (skipChar ':' >> role)
  <|> do
    n <- nominal
    fmap (AConcept n) (skipChar ':' >> concept)
      <|> liftM2 (AIndividual n) (pSame << skip) nominal

pSame :: CharParser st SameOrDifferent
pSame = choice $ map (\ a -> tryString (showSame a) >> return a)
        [Same, Different]

box :: CharParser st Box
box = do
  ts <- many tbox
  key "%RBOX"
  rs <- many rbox
  as <- many abox
  return $ Box ts rs as

{-
box :: CharParser st Box
box = do
    f <- nomPair NominalRDecl
    skipChar ':'
    fmap f role
  <|> do
    c <- character
    fmap (RoleKind c) $ parent role
  <|> do
    key "Disjoint"
    fmap DisjointCs $ parent
      $ concept <:> many (skipChar ',' >> concept)
  <|> do
    n <- nominal
    let c0 = CName n
    do
        el <- eqOrLess
        c <- concept
        return $ ConceptDecl c0 $ Just (el, c)
      <|> do
        skipChar ':'
        c1 <- concept
        do
            skipChar '*'
            c2 <- concept
            let t1 = RoleType (RName n) c1 c2
            m <- optionMaybe $ do
              el <- eqOrLess
              r <- role
              skipChar ':'
              c3 <- concept
              skipChar '*'
              c4 <- concept
              return (el, RoleType r c3 c4)
            return $ RoleDecl t1 m
          <|> return (NominalCDecl n c1)
      <|> return (ConceptDecl c0 Nothing)
-}

ppp :: (a -> Doc) -> CharParser () a -> String -> String
ppp pr pa s = case parse (skip >> pa << eof) "" s of
  Right a -> show $ pr a
  Left e -> show e
