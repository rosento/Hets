{-# LANGUAGE MultiParamTypeClasses #-}
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


-- BEGIN AS
data Ident
type SPEC_NAME = Ident

data NAMED_SPEC = SPEC_NAME := BASIC_SPEC
data BASIC_SPEC = Basic [ACT_ITEMS] [BASIC_ITEMS]
data BASIC_ITEMS = SigB SIG_ITEMS
                 | TransB TRANS_ITEM
                 | InitB STATE GUARD
                 | FinalB STATE
data ACT_ITEMS = Acts [ACT_ITEM] | Events [EVENT_ITEM]
data ACT_ITEM = ActI ACT_NAME VAR_NAME TERM
data EVENT_ITEM = EVENT
data EVENT = TODOe
data TERM = VarT VAR_NAME
          | ConstT NAT_LIT
          | NodeT TERM NAT_OP TERM
          | ParensT TERM -- TODO throw away?
data NAT_OP = Plus | Minus | Times | Div
data VAR_NAME = M | N

data ACT_NAME = TODOa | WildcardAct
data NAT_LIT = Lit Int

data SIG_ITEMS = StateS [STATE_ITEM]
               | ChoiceS [STATE_ITEM]
data STATE_ITEM = StateI STATE (Maybe STATE_ATTR)

data STATE_ATTR = Open

data STATE = TODOs | WildcardS

data TRANS_ITEM = TransI STATE STATE TRANS_LABEL (Maybe TRANS_ATTR)
data TRANS_LABEL = Label (Maybe TRIGGER) (Maybe GUARD) (Maybe ACTION)
data TRIGGER = Trigger EVENT
data GUARD = Guard FORMULA
type ACTION = ACT_NAME
data TRANS_ATTR = Optional

data FORMULA = TERM COMP_OP TERM
          | True | False
          | Not FORMULA
          | And [FORMULA]
          | Or [FORMULA]
          | FORMULA :=> FORMULA
          | FORMULA :<= FORMULA
          | FORMULA :<=> FORMULA
          | Parens FORMULA
data COMP_OP = Eq | Less | LessEq | Greater | GreaterEq
-- END AS

data UMLState = UMLState deriving Show

instance Category () ()

instance Monoid BASIC_SPEC where

instance Show BASIC_SPEC where
instance Pretty BASIC_SPEC where
instance GetRange BASIC_SPEC where

instance Language UMLState where

basicSpec = undefined
instance ShATermConvertible BASIC_SPEC where
instance Syntax UMLState BASIC_SPEC () () () where
  parsersAndPrinters UMLState = makeDefault (basicSpec [], pretty)

instance Sentences UMLState () () () () where
instance StaticAnalysis UMLState BASIC_SPEC () () () () () () () where
instance Logic UMLState () BASIC_SPEC () () () () () () () () where
