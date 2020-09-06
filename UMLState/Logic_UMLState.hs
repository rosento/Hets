{-# LANGUAGE MultiParamTypeClasses , FlexibleContexts , TypeSynonymInstances , FlexibleInstances #-}

{- |
 TODO adjust comments
Module      :  ./UMLState/Logic_UMLState.hs
Description : Instance of class Logic for the UMLState logic
Copyright   :  (c) Tobias Rosenberger, Swansea University and Universit{'e} Grenoble Alpes 2020
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  tobias.rosenberger@univ-grenoble-alpes.fr
Stability   :  experimental
Portability :  non-portable (imports Logic.Logic)
-}

module UMLState.Logic_UMLState where

import Logic.Logic

import qualified CASL.AS_Basic_CASL as C
import qualified CASL.Formula as CF

import Common.DocUtils
import Common.Id
import ATerm.Conversion
import Common.GlobalAnnotations
import Common.AnnoState
import Common.Lexer
import Common.Parsec
import Common.Result

import Data.Set as Set
import Data.Map as Map
import Data.List as List

import Control.Monad (when)
import Control.Monad.Trans.State as S

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String

import CASL.ToDoc -- TODO just for ghci debugging


-- type FORMULA = C.FORMULA ()
-- formula = CF.formula []

-- BEGIN AS
type SPEC_NAME = Token

data NAMED_SPEC = SPEC_NAME := BASIC_SPEC deriving Show
data BASIC_SPEC = Basic [BASIC_ITEMS] deriving Show
data BASIC_ITEMS = SigB SIG_ITEMS
                 | VarB VAR_ITEMS
                 | SenB SEN_ITEMS
                 | EvtB EVENT_ITEMS
                 deriving Show

data VAR_ITEMS = VarIs [VAR_NAME] deriving Show

data SEN_ITEMS = TransB TRANS_ITEM
               | InitB STATE GUARD
               -- -- | FinalB STATE
               deriving Show
data EVENT_ITEMS = EvtIs [EVENT_ITEM] deriving Show
data EVENT_ITEM = EvtI EVENT_NAME [VAR_NAME] deriving Show
data TERM = VarT VAR_NAME
          | ConstT NAT_LIT
          | TERM :+ TERM
          | TERM :- TERM
          | TERM :* TERM
          | TERM :/ TERM
                deriving (Show, Eq)
data NAT_OP = Plus | Minus | Times | Div deriving Show
type VAR_NAME = Token

type EVENT_NAME = Token
type NAT_LIT = Int

data SIG_ITEMS = StateS [STATE_ITEM]
               --  -- | ChoiceS [STATE_ITEM]
               deriving Show
data STATE_ITEM = StateI STATE deriving Show


type STATE = Token

data TRANS_ITEM = TransI STATE STATE TRANS_LABEL deriving Show
data TRANS_LABEL = Label TRIGGER (Maybe GUARD) (Maybe ACTIONS) deriving Show
type TRIGGER = EVENT_ITEM
type GUARD = FORMULA

data ACTIONS = Acts [ACTION] deriving Show
data ACTION = Act VAR_NAME TERM deriving Show

data FORMULA = CompF TERM COMP_OP TERM
             | TrueF | FalseF
             | NotF FORMULA
             | FORMULA :/\ FORMULA
             | FORMULA :\/ FORMULA
             | FORMULA :=> FORMULA
             | FORMULA :<= FORMULA
             | FORMULA :<=> FORMULA
             deriving (Show, Eq)
data COMP_OP = Less | LessEq | Eq | GreaterEq | Greater deriving (Enum, Eq)
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



-- namedSpec :: [t0] -> PrefixMap -> GenParser Char st NAMED_SPEC
namedSpec bi fooTODO = do
  key "spec" 
  name <- str2Token <$> scanLetterWord << skipSmart
  asSeparator "="
  contents <- basicSpec bi fooTODO 
  return $ name := contents

-- basicSpec :: [t0] -> PrefixMap -> GenParser Char st BASIC_SPEC
basicSpec bi _ = Basic <$> basicItems `sepBy` semiT

evtItems = EvtIs <$> (evtS *> evts)
evtS = try $ pluralKeyword "event"
evts = many evtItem
evtItem = do
  evtName <- str2Token <$> scanLetterWord << skipSmart
  maybeArgs <- optionMaybe $ do
    oParenT >> (var << skipSmart) `sepBy` oneOf "," << cParenT
  return $ EvtI evtName $ case maybeArgs of
    Nothing -> []
    Just varNames -> varNames
basicItems = do SigB   <$> sigItems
         <|> do SenB . TransB <$> transItem
         <|> do SenB <$> (InitB  <$> stateP <*> guardP)
         <|> do EvtB <$> evtItems
         -- <|> do SenB . FinalB <$> stateP

varItems = pluralKeyword "var" >> ((var << skipSmart) `sepBy` oneOf ",;")
sigItems = StateS <$> (statePS *> statePs)
statePS = pluralKeyword "state"
statePs = statePItem `sepBy` oneOf ",;"
statePItem = StateI <$> stateP << skipSmart
transItem = do s1 <- try stateP
               try $ asSeparator "-->"
               s2 <- stateP
               asSeparator ":"
               label <- transLabel
               return $ TransI s1 s2 label
transLabel = do p <- trigger
                g <- optionMaybe guardP
                a <- optionMaybe actions
                return $ Label p g a
actions = Acts <$> (asSeparator "/" >> action `sepBy` asSeparator ";")
action = Act <$> var <*> (asSeparator ":=" >> term)
trigger = evtItem
guardP = oBracketT >> formula << cBracketT
stateP = str2Token <$> (scanLetterWord << skipSmart)

formula = buildExpressionParser ops simpForm where
  ops = [ [preWordOp "not" NotF]
        , [symOpR "/\\" (:/\)]
        , [symOpR "\\/" (:\/)]
        , [symOpR "=>" (:=>), wordOp "if" (:<=), symOpR "<=>" (:<=>)]
        ]  -- TODO check CASL precedence rules
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

var = str2Token <$> scanLetterWord << skipSmart

natLit = value 10 <$> getNumber << skipSmart

key s = keyWord (string s) << skipSmart

-- parse helpers
preWordOp w op = Prefix (key w >> return op)
symOpL s op = Infix (asSeparator s >> return op) AssocLeft
symOpR s op = Infix (asSeparator s >> return op) AssocRight
wordOp w op = Infix (key w >> return op) AssocRight

-- TODO find better name
showChoice xs = choice
  [ const x <$> try (asSeparator $ show x)
  | x <- xs
  ]

-- END parsing

instance ShATermConvertible BASIC_SPEC where
instance Syntax UMLState BASIC_SPEC () () () where
  parsersAndPrinters UMLState = makeDefault (basicSpec [], pretty)

instance Sentences UMLState () () () () where
instance StaticAnalysis UMLState BASIC_SPEC () () () () () () () where
instance Logic UMLState () BASIC_SPEC () () () () () () () () where





type Sign = Set STATE
type Thy = [SEN_ITEMS]
data Library = Result
  { statesL :: Set STATE
  , varsL   :: Set VAR_NAME
  , actsL   :: Map (EVENT_NAME, Int) [VAR_NAME]
  , initL   :: Maybe (STATE, GUARD)
  , transL  :: [TRANS_ITEM]
  }

type Check = StateT Library Result

-- instance MonadState Library Check where

ana_BASIC_SPEC :: BASIC_SPEC -> Check BASIC_SPEC
ana_BASIC_SPEC (Basic bs) = Basic <$> sequence (ana_BASIC_ITEMS <$> bs)

ana_BASIC_ITEMS :: BASIC_ITEMS -> Check BASIC_ITEMS
ana_BASIC_ITEMS (SigB items) = SigB <$> ana_SIG_ITEMS items
ana_BASIC_ITEMS (SenB items) = SenB <$> ana_SEN_ITEMS items
ana_BASIC_ITEMS (EvtB items) = EvtB <$> ana_EVENT_ITEMS items
ana_BASIC_ITEMS (VarB items) = VarB <$> ana_VAR_ITEMS items

ana_VAR_ITEMS (VarIs vs) = VarIs <$> sequence (ana_VAR_ITEM <$> vs)

ana_VAR_ITEM var = do
  lib <- get
  let vars = varsL lib
  when (var `Set.member` vars) $ error ("variable declared twice: " ++ show var)
  put $ lib {
    varsL = var `Set.insert` vars
  }
  return var
  

ana_SIG_ITEMS (StateS items) = StateS <$> sequence (ana_STATE_ITEM <$> items)

ana_SEN_ITEMS (TransB trans) = TransB <$> ana_TRANS_ITEMS trans
ana_SEN_ITEMS (InitB st g) = do
  lib <- get
  let sts = statesL lib
      lib' = lib {
        initL = Just (st, g)
      }
  if st `elem` sts
  then case initL lib of
    Nothing   -> put lib' >> do return $ InitB st g
    Just init -> error "Defined two initial transitions."
  else error "Initial transition to undefined state."


ana_EVENT_ITEMS :: EVENT_ITEMS -> Check EVENT_ITEMS
ana_EVENT_ITEMS (EvtIs as) = EvtIs <$> sequence (ana_EVENT_ITEM <$> as)

ana_EVENT_ITEM :: EVENT_ITEM -> Check EVENT_ITEM
ana_EVENT_ITEM (EvtI name vars) = do
  lib <- get
  put $ lib {actsL = Map.insert (name, length vars) vars $ actsL lib}
  return $ EvtI name vars
  

ana_TRANS_ITEMS (TransI st1 st2 (Label t@(EvtI tname tvars) g a)) = do
  lib <- get
  let sts = statesL lib
      acts = actsL lib
      tkey = (tname, length tvars)
      insertVars [] set = set
      insertVars (v:vs) set = insertVars vs (v `Set.insert` set) -- TODO check duplicates
      varsWithTvar = insertVars tvars $ varsL lib
      actSkip   = Acts []
      checkWhenJust m res f = maybe (return $ Just res) f m >> return (Just res)
  when (st1 `Set.notMember` sts) $
    error ("transition out of undefined state: " ++ show st1)
  when (st2 `Set.notMember` sts) $
    error ("transition into undefined state: "   ++ show st2)
  when (tkey `Map.notMember` acts) $
    error ("transition with undefined trigger: " ++ show tkey)
  g' <- checkWhenJust g TrueF $ \ gRaw -> do
    undefined -- TODO check FORMULA
  a' <- checkWhenJust a actSkip $ \ (Acts as) -> do
    Just . Acts <$> sequence (ana_ACTION (varsL lib) varsWithTvar <$> as)
  let l' = Label t g' a'
      newTrans = TransI st1 st2 l'
      lib' = lib {
        transL = newTrans : transL lib
      }
  put lib'
  return newTrans

ana_ACTION vars varsWithTvar (Act var tm) = do
  when (var `Set.notMember` vars) $ error ("assignment to undeclared variable")
  ana_TERM varsWithTvar tm
  return $ Act var tm

ana_TERM vars (VarT var) = when (var `Set.notMember` vars) $
  error ("reference to undeclared variable: " ++ show var)
ana_TERM vars (ConstT lit) = return ()
ana_TERM vars (a :+ b) = ana_TERM vars a >> ana_TERM vars b
ana_TERM vars (a :- b) = ana_TERM vars a >> ana_TERM vars b
ana_TERM vars (a :* b) = ana_TERM vars a >> ana_TERM vars b
ana_TERM vars (a :/ b) = ana_TERM vars a >> ana_TERM vars b
  
ana_STATE_ITEM (StateI st) = do
  lib <- get
  put $ lib {
    statesL = st `Set.insert` statesL lib 
  }
  return $ StateI st

data EDHML = DtSen FORMULA
           | St STATE
           | Binding STATE EDHML
           | At STATE EDHML -- add F?
           | Box EDHML      -- add F?
           | DiaEE EVENT_ITEM FORMULA EDHML -- exists valuation and transititon ...
           | DiaAE EVENT_ITEM FORMULA FORMULA EDHML -- for each valuation satisfying phi there exists a transition ...
           | Not EDHML
           | Or EDHML EDHML
           | TrueE
           deriving Show

state2EDHML c ((phi,e,psi,c'):is) vs bs im1 im2 es =
  At c $ DiaAE e phi psi (
    St c `andE` state2EDHML c is vs bs im1 im2 es
  )
state2EDHML c [] (c':vs) bs im1 im2 es = state2EDHML c' (im1 c') vs bs im1 im2 es
state2EDHML c [] []      bs im1 im2 es = fin bs im2 es `andE` pairsDiff bs

pairsDiff bs = foldAndE [Not $ At c1 $ St c2 | c1 <- bs, c2 <- bs]

fin cs im2 es = foldAndE
  [ At c $ foldAndE [ foldAndE [ DiaEE e (
                                   (
                                     foldAndF [ phi :/\ psi
                                              | (phi, psi, c') <- ps
                                              ]
                                   ) :/\ NotF (
                                     foldOrF  [ phi :/\ psi
                                              | (phi, psi, c') <- nps
                                              ]
                                   )
                                 ) $ foldOrE [ St c'
                                             | (phi, psi, c') <- ps
                                             ]
                               | (ps, nps) <- celookup c e im2
                               ]
                    | e <- es
                    ]
  | c <- cs
  ]

celookup c (EvtI ename evars) im2 = case Map.lookup (c, ename, length evars) im2
                                    of Nothing -> []
                                       Just xs -> [ ( ps
                                                    , xs List.\\ ps
                                                    )
                                                  | ps <- subsequences xs -- TODO use Set
                                                                          -- problem: powerSet reported as not exported
                                                  ]


-- CASL logic helpers
a `andC` b = C.Junction C.Con [a, b] nullRange
a `orC` b = C.Junction C.Dis [a, b] nullRange
notC a = C.Negation a nullRange
foldAndC xs = C.Junction C.Con xs nullRange
foldOrC  xs = C.Junction C.Dis xs nullRange

-- state FORMULA logic helpers
foldAndF = List.foldl (:/\) TrueF
foldOrF  = List.foldl (:\/) FalseF

-- EDHML logic helpers
phi `andE` psi = Not (Not phi `Or` Not psi)
foldAndE = List.foldl andE TrueE
foldOrE = List.foldl Or $ Not TrueE

-- TODO handle EDHML signatures

edhml2CASL :: EDHML -> VAR_NAME -> C.FORMULA f
edhml2CASL (DtSen f) g = stForm2CASL g (prime g) f
edhml2CASL (St s) g = C.Mixfix_token s `C.mkStEq` ctrl g
edhml2CASL (Binding s f) g = exCtrl s (
                                      C.Mixfix_token s `C.mkStEq` ctrl g
                               `andC` edhml2CASL f g -- TODO check index "S union {s}"
                             )
edhml2CASL (At s f) g =
  univConf (prime g) (
               (
                        (ctrl (prime g) `C.mkStEq` C.Mixfix_token s)
                 `andC` reach2 {- TODO F? -} (prime g)
               )
    `C.mkImpl` edhml2CASL f (prime g)
  )
edhml2CASL (Box f) g =
  univConf (prime g) (
    reach3 {- F? -} g (prime g) `C.mkImpl` edhml2CASL f (prime g)
  )
edhml2CASL (DiaEE e@(EvtI _ evars) phi f) g =
  let g' = prime g
  in exEvtArgs evars $ exConf g' (
       trans g e g' `andC` stForm2CASL g g' phi `andC` edhml2CASL f g'
     )
edhml2CASL (DiaAE e@(EvtI _ evars) phi psi f) g =
  let g' = prime g
  in univEvtArgs evars $ (
       stForm2CASL g g' phi `C.mkImpl` exConf g' (
         trans g e g' `andC` stForm2CASL g g' phi `andC` edhml2CASL f g'
       )
     )
edhml2CASL (Not f)   g = notC $ edhml2CASL f g
edhml2CASL (Or f f') g = edhml2CASL f g `orC` edhml2CASL f' g


stForm2CASL :: Token -> Token -> FORMULA -> C.FORMULA f
stForm2CASL g g' (CompF x compOp y) = C.Mixfix_formula $ C.Mixfix_term
  [ term2CASLterm g g' x
  , C.Mixfix_qual_pred $ C.Pred_name $ str2Id $ show compOp
  , term2CASLterm g g' y
  ]
stForm2CASL g g' TrueF        = C.trueForm
stForm2CASL g g' FalseF       = C.falseForm
stForm2CASL g g' (NotF f)     = notC $ stForm2CASL g g' f
stForm2CASL g g' (f :/\ f')   = stForm2CASL g g' f `andC`     stForm2CASL g g' f'
stForm2CASL g g' (f :\/ f')   = stForm2CASL g g' f `orC`      stForm2CASL g g' f'
stForm2CASL g g' (f :=> f')   = stForm2CASL g g' f `C.mkImpl` stForm2CASL g g' f'
stForm2CASL g g' (f :<= f')   = stForm2CASL g g' f'`C.mkImpl` stForm2CASL g g' f
stForm2CASL g g' (f :<=> f')  = stForm2CASL g g' f `C.mkEqv`  stForm2CASL g g' f'

term2CASLterm g g' (VarT var)      = attr2CASLterm g g' var
term2CASLterm g g' (ConstT natLit) = C.Mixfix_token $ str2Token $ show natLit
term2CASLterm g g' (x:+y)          = translOp "+" x y g g'
term2CASLterm g g' (x:-y)          = translOp "-" x y g g'
term2CASLterm g g' (x:*y)          = translOp "*" x y g g'
term2CASLterm g g' (x:/y)          = translOp "/" x y g g'

translOp opName x y g g' = C.mkAppl (C.Op_name $ str2Id opName)
                                    (term2CASLterm g g' <$> [x,y])

-- TODO: consider bound variables as opposed to attributes
attr2CASLterm g g' (Token varName _) =
  if primed varName
  then C.mkAppl (C.Op_name $ token2Id g')
                [C.Qual_var (str2Token $ unprime varName) confSort nullRange]
  else C.mkAppl (C.Op_name $ token2Id g )
                [C.Qual_var (str2Token           varName) confSort nullRange]

primed = (=='\'') . last

-- | assumes primed
unprime = reverse . drop 1 . reverse

eventItem2CASLterm (EvtI (Token name _) vars) =
  C.mkAppl (C.Op_name $ str2Id name)
           (flip C.mkVarTerm natSort <$> vars)

str2Id         = token2Id . str2Token
token2Id tok   = Id [tok] [] nullRange
str2Token name = Token name nullRange

exEvtArgs   evars phi = C.mkExist  [C.Var_decl evars natSort nullRange] phi
univEvtArgs evars phi = C.mkForall [C.Var_decl evars natSort nullRange] phi
exCtrl s f = C.mkExist [C.mkVarDecl s ctrlSort] f
exConf   g f = C.mkExist  [C.mkVarDecl g confSort] f
univConf g f = C.mkForall [C.mkVarDecl g confSort] f
prime (Token var _) = str2Token (var++"'")
trans g e g' = C.mkPredication transPred [ confVar g
                                         , eventItem2CASLterm e
                                         , confVar g'
                                         ]
confVar g   = C.mkVarTerm g confSort
ctrl s      = C.mkAppl (C.Op_name ctrlOp) [C.mkVarTerm s ctrlSort]
reach2 g    = C.mkPredication reach2Pred [confVar g]
reach3 g g' = C.mkPredication reach3Pred (confVar <$> [g,g'])

transPred  = C.Pred_name $ str2Id "trans"
reach2Pred = C.Pred_name $ str2Id "reachable2"
reach3Pred = C.Pred_name $ str2Id "reachable3"
ctrlOp   = str2Id "ctrl"
ctrlSort = str2Id "Ctrl"
evtSort  = str2Id "Evt"
natSort  = str2Id "Nat"
confSort = str2Id "Conf"
