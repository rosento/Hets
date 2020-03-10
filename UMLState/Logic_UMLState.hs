{-# LANGUAGE MultiParamTypeClasses , FlexibleContexts #-}
{- |
 TODO adjust comments
Module      :  ./UMLState/Logic_UMLState.hs
Description : Instance of class Logic for the UMLState logic
Copyright   :  (c) Tobias Rosenberger, Swansea University and Universit{'e} Grenoble Alpes 2020
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  tobias.rosenberger@univ-grenoble-alpes.fr
Stability   :  provisional
Portability :  non-portable (imports Logic.Logic)
-}

module UMLState.Logic_UMLState where

import Logic.Logic

import Common.DocUtils
import Common.Id
import ATerm.Conversion
import Common.GlobalAnnotations
import Common.AnnoState
import Common.Lexer
import Common.Parsec

import Text.Parsec
import Text.Parsec.Expr


-- BEGIN AS
data Ident = Ident String deriving Show
type SPEC_NAME = Ident

data NAMED_SPEC = SPEC_NAME := BASIC_SPEC deriving Show
data BASIC_SPEC = Basic [ACT_ITEMS] [BASIC_ITEMS] deriving Show
data BASIC_ITEMS = SigB SIG_ITEMS
                 | TransB TRANS_ITEM
                 | InitB STATE GUARD
                 | FinalB STATE deriving Show
data ACT_ITEMS = Acts [ACT_ITEM] | Events [EVENT_ITEM] deriving Show
data ACT_ITEM = ActI ACT_NAME VAR_NAME TERM deriving Show
data EVENT_ITEM = EVENT deriving Show
data EVENT = TODOe deriving Show
data TERM = VarT VAR_NAME
          | ConstT NAT_LIT
          | TERM :+ TERM
          | TERM :- TERM
          | TERM :* TERM
          | TERM :/ TERM
                deriving Show
data NAT_OP = Plus | Minus | Times | Div deriving Show
data VAR_NAME = M | N deriving Show

data ACT_NAME = AN String  deriving Show -- TODOa | WildcardAct deriving Show
data NAT_LIT = Lit Int deriving Show

data SIG_ITEMS = StateS [STATE_ITEM]
               | ChoiceS [STATE_ITEM] deriving Show
data STATE_ITEM = StateI STATE (Maybe STATE_ATTR) deriving Show

data STATE_ATTR = Open deriving Show

data STATE = TODOs | WildcardS deriving Show

data TRANS_ITEM = TransI STATE STATE TRANS_LABEL (Maybe TRANS_ATTR) deriving Show
data TRANS_LABEL = Label (Maybe TRIGGER) (Maybe GUARD) (Maybe ACTION) deriving Show
data TRIGGER = Trigger EVENT deriving Show
data GUARD = Guard FORMULA deriving Show
type ACTION = ACT_NAME
data TRANS_ATTR = Optional deriving Show

data FORMULA = CompF TERM COMP_OP TERM
             | TrueF | FalseF
             | NotF FORMULA
             | FORMULA :/\ FORMULA
             | FORMULA :\/ FORMULA
             | FORMULA :=> FORMULA
             | FORMULA :<= FORMULA
             | FORMULA :<=> FORMULA
             | ParensF FORMULA deriving Show
data COMP_OP = Less | LessEq | Eq | GreaterEq | Greater deriving Enum
-- END AS

instance Show COMP_OP where
  show Less = "<"
  show LessEq = "<="
  show Eq = "="
  show GreaterEq = ">="
  show Greater = ">"

data UMLState = UMLState deriving Show

instance Category () ()

instance Monoid BASIC_SPEC where

instance Pretty BASIC_SPEC where
instance GetRange BASIC_SPEC where

instance Language UMLState where

-- BEGIN parsing
basicSpec :: [t0] -> PrefixMap -> AParser st BASIC_SPEC
basicSpec bi _ = do
  ais <- actItems `sepBy` semiT
  semiT
  bis <- basicItems `sepBy` semiT
  return $ Basic ais bis
actItems = Acts <$> (actS *> acts)
actS = pluralKeyword "action"
acts = actItem `sepBy` semiT
actItem = do
  actName <- AN <$> scanLetterWord << skipSmart
  oBraceT
  varName <- var << skipSmart
  asSeparator ":="
  t <- term << skipSmart
  cBraceT
  return $ ActI actName varName t
basicItems = do SigB   <$> sigItems
         <|> do TransB <$> transItem
         <|> do InitB  <$> state <*> guard
         <|> do FinalB <$> state
sigItems = undefined
transItem = do s1 <- try state
               try $ asSeparator "-->"
               s2 <- state
               asSeparator ":"
               label <- transLabel
               attr <- optionMaybe transAttr
               return $ TransI s1 s2 label attr
transLabel = do p <- optionMaybe trigger
                g <- optionMaybe guard
                a <- optionMaybe action
                return $ Label p g a
trigger = event
guard = Guard <$> (oBracketT >> formula << cBracketT)
action  = AN <$> (asSeparator "/" >> scanLetterWord << skipSmart)
transAttr = const Optional <$> key "option"
state = undefined

event = undefined

formula = buildExpressionParser ops simpForm where
  ops = [ [preWordOp "not" NotF]
        , [symOpR "/\\" (:/\)]
        , [symOpR "\\/" (:\/)]
        , [symOpR "=>" (:=>), wordOp "if" (:<=), symOpR "<=>" (:<=>)]
        ]
  simpForm = do key "true"  >> return TrueF
         <|> do key "false" >> return FalseF
         <|> do CompF <$> try term <*> try compOp <*> term
         <|> do oParenT >> formula << cParenT

term = buildExpressionParser ops simpTerm where
  ops = [ [symOpL "*" (:*), symOpL "/" (:/)]
        , [symOpL "+" (:+), symOpL "-" (:-)]
        ]
  simpTerm = do VarT <$> var
         <|> do ConstT <$> natLit
         <|> do oParenT >> term << cParenT
    
compOp = showChoice [Less .. Greater]

var = do char 'm' >> return M << skipSmart
  <|> do char 'n' >> return N << skipSmart

natLit = Lit . value 10 <$> getNumber << skipSmart

key s = keyWord (string s) << skipSmart

-- parse helpers
preWordOp w op = Prefix (key w >> return op)
symOpL s op = Infix (asSeparator s >> return op) AssocLeft
symOpR s op = Infix (asSeparator s >> return op) AssocRight
wordOp w op = Infix (key w >> return op) AssocRight

-- TODO find better name
showChoice xs = choice [ const x <$> try (asSeparator $ show x) | x <- xs ]

-- END parsing

instance ShATermConvertible BASIC_SPEC where
instance Syntax UMLState BASIC_SPEC () () () where
  parsersAndPrinters UMLState = makeDefault (basicSpec [], pretty)

instance Sentences UMLState () () () () where
instance StaticAnalysis UMLState BASIC_SPEC () () () () () () () where
instance Logic UMLState () BASIC_SPEC () () () () () () () () where
