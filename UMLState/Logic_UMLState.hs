{-# LANGUAGE MultiParamTypeClasses , FlexibleContexts , TypeSynonymInstances , FlexibleInstances, DeriveDataTypeable #-}

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
import Common.ExtSign
import Common.Id
import ATerm.Conversion
import Common.GlobalAnnotations
import Common.AnnoState
import Common.Lexer
import Common.Parsec
import Common.Result as R

import Data.Set as Set
import Data.Map as Map
import Data.List as List
import Data.Functor.Identity

import qualified Data.Data as D

import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State as S

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char

import CASL.ToDoc -- TODO just for ghci debugging


-- type FORMULA = C.FORMULA ()
-- formula = CF.formula []

-- BEGIN AS
type SPEC_NAME = Token

data NAMED_SPEC = SPEC_NAME := BASIC_SPEC deriving (Eq,Ord,Show,D.Data)
data BASIC_SPEC = Basic [BASIC_ITEMS] deriving (Eq,Ord,Show,D.Data)
data BASIC_ITEMS = SigB SIG_ITEMS
                 | VarB VAR_ITEMS
                 | SenB SEN_ITEMS
                 | EvtB EVENT_ITEMS
                 deriving (Eq,Ord,Show,D.Data)

data VAR_ITEMS = VarIs [VAR_NAME] deriving (Eq,Ord,Show,D.Data)

data SEN_ITEMS = TransB TRANS_ITEM
               | InitB STATE GUARD
               -- -- | FinalB STATE
               deriving (Eq,Ord,Show,D.Data)
data EVENT_ITEMS = EvtIs [EVENT_ITEM] deriving (Eq,Ord,Show,D.Data)
data EVENT_ITEM = EvtI EVENT_NAME [VAR_NAME] deriving (Eq,Ord,Show,D.Data)
data TERM = VarT VAR_NAME
          | ConstT NAT_LIT
          | TERM :+ TERM
          | TERM :- TERM
          | TERM :* TERM
          | TERM :/ TERM
          deriving (Show,Ord,Eq,D.Data)
data NAT_OP = Plus | Minus | Times | Div deriving (Eq,Ord,Show,D.Data)
type VAR_NAME = Token

type EVENT_NAME = Token
type NAT_LIT = Int

data SIG_ITEMS = StateS [STATE_ITEM]
               --  -- | ChoiceS [STATE_ITEM]
               deriving (Eq,Ord,Show,D.Data)
data STATE_ITEM = StateI STATE deriving (Eq,Ord,Show,D.Data)


type STATE = Token

data TRANS_ITEM = TransI STATE STATE TRANS_LABEL
                deriving (Eq,Ord,Show,D.Data)
data TRANS_LABEL = Label TRIGGER (Maybe GUARD) (Maybe ACTIONS)
                 deriving (Eq,Ord,Show,D.Data)
type TRIGGER = EVENT_ITEM
type GUARD = FORMULA

data ACTIONS = Acts [ACTION] deriving (Eq,Ord,Show,D.Data)
data ACTION = Assign VAR_NAME TERM deriving (Eq,Ord,Show,D.Data)

data FORMULA = CompF TERM COMP_OP TERM
             | TrueF | FalseF
             | NotF FORMULA
             | FORMULA :/\ FORMULA
             | FORMULA :\/ FORMULA
             | FORMULA :=> FORMULA
             | FORMULA :<= FORMULA
             | FORMULA :<=> FORMULA
             deriving (Eq,Ord,Show,D.Data)
data COMP_OP = Less | LessEq | Eq | GreaterEq | Greater deriving (Enum,Eq,Ord,D.Data)
-- END AS

instance Show COMP_OP where
  show Less      = "<"
  show LessEq    = "<="
  show Eq        = "="
  show GreaterEq = ">="
  show Greater   = ">"

data UMLState = UMLState deriving (Eq,Ord,Show,D.Data)

type Morphism = Library -- only identity morphisms

instance Category Library Morphism where
  ide sig = sig
  inverse f = return f
  composeMorphisms f g = return f
  dom f = f
  cod f = f
  isInclusion f = True
  legal_mor f = return ()
  

instance Monoid BASIC_SPEC where

instance Pretty BASIC_SPEC where
instance GetRange BASIC_SPEC where

instance GetRange EDHML where

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
basicSpec bi _ = Basic <$> try basicItems `sepBy` semiT << theEnd

theEnd :: Parsec [Char] st [Char]
theEnd = optionMaybe (asSeparator ";") >> key "end"

evtItems :: Parsec [Char] st EVENT_ITEMS
evtItems = EvtIs <$> (evtS *> evts) <?> "event items"

evtS :: Parsec [Char] st Token
evtS = try $ pluralKeyword "event" << skipSmart

evts :: Parsec [Char] st [EVENT_ITEM]
evts = try evtItem `sepBy` asSeparator ","

evtItem :: Parsec [Char] st EVENT_ITEM
evtItem = do
  evtName <- str2Token <$> scanLetterWord << skipSmart
  maybeArgs <- optionMaybe $ do
    oParenT >> (var << skipSmart) `sepBy` asSeparator "," << cParenT
  return $ EvtI evtName $ case maybeArgs of
    Nothing -> []
    Just varNames -> varNames

basicItems :: Parsec [Char] st BASIC_ITEMS
basicItems = do SigB <$> sigItems
         <|> do SenB . TransB <$> transItem
         <|> do SenB <$> (key "init" >> (InitB  <$> (stateP  << asSeparator ":") <*> guardP))
         <|> do EvtB <$> evtItems
         <|> do VarB <$> varItems
         -- <|> do SenB . FinalB <$> stateP

varItems :: Parsec [Char] st VAR_ITEMS
varItems = VarIs <$> (pluralKeyword "var" >> ((var << skipSmart) `sepBy` asSeparator ","))

sigItems :: Parsec [Char] st SIG_ITEMS
sigItems = StateS <$> (statePS *> statePs)

statePS :: CharParser st Token
statePS = pluralKeyword "state" << skipSmart

statePs :: Parsec [Char] st [STATE_ITEM]
statePs = statePItem `sepBy` asSeparator ","

statePItem :: Parsec [Char] st STATE_ITEM
statePItem = StateI <$> stateP << skipSmart

transItem :: Parsec [Char] st TRANS_ITEM
transItem = do key "trans"
               s1 <- try stateP
               try $ asSeparator "-->"
               s2 <- stateP
               asSeparator ":"
               label <- transLabel
               return $ TransI s1 s2 label

transLabel :: Parsec [Char] st TRANS_LABEL
transLabel = do p <- trigger
                g <- optionMaybe guardP
                a <- optionMaybe actions
                return $ Label p g a

actions :: Parsec [Char] st ACTIONS
actions = Acts <$> do
  asSeparator "/"
  asSeparator "{"
  as <- action `sepBy` asSeparator ";"
  asSeparator "}"
  return as

action = Assign <$> var <*> (asSeparator ":=" >> term)

trigger :: Parsec [Char] st EVENT_ITEM
trigger = evtItem

guardP :: Parsec [Char] st FORMULA
guardP = oBracketT >> formula << cBracketT

stateP :: Parsec [Char] st Token
stateP = str2Token <$> (scanLetterWord << skipSmart)

formula :: Parsec [Char] st FORMULA
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

term :: Parsec [Char] st TERM
term = buildExpressionParser ops simpTerm where
  ops = [ [symOpL "*" (:*), symOpL "/" (:/)]
        , [symOpL "+" (:+), symOpL "-" (:-)]
        ]
  simpTerm = do VarT <$> var
         <|> do ConstT <$> natLit
         <|> do oParenT >> term << cParenT
    

compOp :: Parsec [Char] st COMP_OP
compOp = parseAlts [Less .. Greater]


var :: Parsec [Char] st VAR_NAME
var = str2Token <$> scanLetterWord << skipSmart


natLit :: Parsec [Char] st Int
natLit = value 10 <$> getNumber << skipSmart


key :: String -> Parsec [Char] st String
key s = try (keyWord (string s) << skipSmart)

-- parse helpers

preWordOp :: String -> (a -> a) -> Operator [Char] st Data.Functor.Identity.Identity a
preWordOp w op = Prefix (key w >> return op)

symOpL :: String -> (a -> a -> a) -> Operator [Char] st Data.Functor.Identity.Identity a
symOpL s op = Infix (asSeparator s >> return op) AssocLeft

symOpR :: String -> (a -> a -> a) -> Operator [Char] st Data.Functor.Identity.Identity a
symOpR s op = Infix (asSeparator s >> return op) AssocRight

wordOp :: String -> (a -> a -> a) -> Operator [Char] st Data.Functor.Identity.Identity a
wordOp w op = Infix (key w >> return op) AssocRight

-- TODO find better name
parseAlts :: Show a => [a] -> Parsec [Char] st a
parseAlts xs = choice
  [ const x <$> try (asSeparator $ show x)
  | x <- xs
  ]

-- END parsing

instance ShATermConvertible BASIC_SPEC where
instance Syntax UMLState BASIC_SPEC Token () () where
  parsersAndPrinters UMLState = makeDefault (basicSpec [], pretty)

instance Sentences UMLState EDHML Library Morphism Token where
instance StaticAnalysis UMLState BASIC_SPEC EDHML () () Library Morphism Token () where
  basic_analysis _ = Just $ \ (spec,sign,annos) -> do
    (spec',lib) <- runStateT (ana_BASIC_SPEC spec) mempty
    let sign   = lib
        symSet = sign2SymSet $ lib2Sign lib
        extSign = ExtSign sign symSet
        annos  = []
    return (spec', extSign, annos) -- TODO

instance Logic UMLState () BASIC_SPEC EDHML () () Library Morphism Token () () where

instance Pretty EDHML where
instance Pretty Library where

instance ShATermConvertible EDHML where
instance ShATermConvertible Library where

instance ShATermConvertible Token where

data Sign = Sign
  { statesS :: Set Token
  , varsS   :: Set Token
  , actsS   :: Map (Token, Arity) EVENT_ITEM
  } deriving (Eq,Show,Ord)
type Thy = [EDHML]
type Arity = Int
data Library = Library
  { statesL :: Set STATE
  , attrL   :: Set VAR_NAME
  , actsL   :: Map (EVENT_NAME, Arity) EVENT_ITEM
  , initL   :: [(STATE, GUARD)]
  , transL  :: [TRANS_ITEM]
  } deriving (Eq,Ord,Show)

instance Monoid Library where
  mempty = Library mempty mempty mempty mempty mempty
  lib `mappend` lib' = Library { statesL = statesL lib `mappend` statesL lib'
                               , attrL   = attrL   lib `mappend` attrL   lib'
                               , actsL   = actsL   lib `mappend` actsL   lib'
                               , initL   = initL   lib `mappend` initL   lib'
                               , transL  = transL  lib `mappend` transL  lib'
                               }

lib2Sign lib = Sign
  { statesS = statesL lib
  , varsS   = attrL lib
  , actsS   = actsL lib
  }

extractActs :: Map (Token, Arity) EVENT_ITEM -> Set Token
extractActs = Set.map fst . Map.keysSet

sign2SymSet :: Sign -> Set Token
sign2SymSet sign = statesS sign `Set.union` varsS sign
                                `Set.union` extractActs (actsS sign)

type Check = StateT Library Result

-- instance MonadState Library Check where

errC :: String -> Check a
errC s = lift $ fatal_error s nullRange

ana_BASIC_SPEC :: BASIC_SPEC -> Check BASIC_SPEC
ana_BASIC_SPEC (Basic bs) = Basic <$> sequence (ana_BASIC_ITEMS <$> bs)

ana_BASIC_ITEMS :: BASIC_ITEMS -> Check BASIC_ITEMS
ana_BASIC_ITEMS (SigB items) = SigB <$> ana_SIG_ITEMS items
ana_BASIC_ITEMS (SenB items) = SenB <$> ana_SEN_ITEMS items
ana_BASIC_ITEMS (EvtB items) = EvtB <$> ana_EVENT_ITEMS items
ana_BASIC_ITEMS (VarB items) = VarB <$> ana_VAR_ITEMS items

ana_VAR_ITEMS :: VAR_ITEMS -> Check VAR_ITEMS
ana_VAR_ITEMS (VarIs vs) = VarIs <$> sequence (ana_VAR_ITEM <$> vs)

ana_VAR_ITEM :: VAR_NAME -> Check VAR_NAME
ana_VAR_ITEM var = do
  lib <- get
  let vars = attrL lib
  when (var `Set.member` vars) $ errC ("variable declared twice: " ++ show var)
  put $ lib {
    attrL = var `Set.insert` vars
  }
  return var
  
ana_SIG_ITEMS :: SIG_ITEMS -> Check SIG_ITEMS
ana_SIG_ITEMS (StateS items) = StateS <$> sequence (ana_STATE_ITEM <$> items)

ana_SEN_ITEMS :: SEN_ITEMS -> Check SEN_ITEMS
ana_SEN_ITEMS (TransB tr) = TransB <$> ana_TRANS_ITEMS tr
ana_SEN_ITEMS (InitB st g) = do
  lib <- get
  let sts = statesL lib
      lib' = lib {
        initL = (st,g) : initL lib
      }
  if st `elem` sts
  then put lib' >> do return $ InitB st g
  else errC "Initial transition to undefined state."

ana_EVENT_ITEMS :: EVENT_ITEMS -> Check EVENT_ITEMS
ana_EVENT_ITEMS (EvtIs as) = EvtIs <$> sequence (ana_EVENT_ITEM <$> as)

ana_EVENT_ITEM :: EVENT_ITEM -> Check EVENT_ITEM
ana_EVENT_ITEM e@(EvtI name vars) = do
  lib <- get
  put $ lib {actsL = Map.insert (name, length vars) e $ actsL lib}
  return e
  

ana_TRANS_ITEMS :: TRANS_ITEM -> Check TRANS_ITEM
ana_TRANS_ITEMS (TransI st1 st2 (Label t@(EvtI tname tvars) g a)) = do
  lib <- get
  let sts = statesL lib
      acts = actsL lib
      tkey = (tname, length tvars)
      insertVars [] set = set
      insertVars (v:vs) set = insertVars vs (v `Set.insert` set) -- TODO check duplicates
      varsWithTvar = insertVars tvars $ attrL lib
      actSkip   = Acts []
      checkWhenJust m res f = maybe (return $ Just res) f m >> return (Just res)
  when (st1 `Set.notMember` sts) $
    errC ("transition out of undefined state: " ++ show st1)
  when (st2 `Set.notMember` sts) $
    errC ("transition into undefined state: "   ++ show st2)
  when (tkey `Map.notMember` acts) $
    errC ("transition with undefined trigger: " ++ show tkey)
  g' <- checkWhenJust g TrueF $ \ gRaw -> do
    return g -- TODO check FORMULA
  a' <- checkWhenJust a actSkip $ \ (Acts as) -> do
    Just . Acts <$> sequence (ana_ACTION (attrL lib) varsWithTvar <$> as)
  let l' = Label t g' a'
      newTrans = TransI st1 st2 l'
      lib' = lib {
        transL = newTrans : transL lib
      }
  put lib'
  return newTrans

ana_ACTION :: Set VAR_NAME -> Set VAR_NAME -> ACTION -> Check ACTION
ana_ACTION vars varsWithTvar (Assign var tm) = do
  when (var `Set.notMember` vars) $ errC ("assignment to undeclared variable")
  ana_TERM varsWithTvar tm
  return $ Assign var tm

ana_TERM :: Set VAR_NAME -> TERM -> Check ()
ana_TERM vars (VarT var) = when (var `Set.notMember` vars) $
  errC ("reference to undeclared variable: " ++ show var)
ana_TERM vars (ConstT lit) = return ()
ana_TERM vars (a :+ b) = ana_TERM vars a >> ana_TERM vars b
ana_TERM vars (a :- b) = ana_TERM vars a >> ana_TERM vars b
ana_TERM vars (a :* b) = ana_TERM vars a >> ana_TERM vars b
ana_TERM vars (a :/ b) = ana_TERM vars a >> ana_TERM vars b
  
ana_STATE_ITEM :: STATE_ITEM -> Check STATE_ITEM
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
           deriving (Eq,Ord,Show,D.Data)

initP :: C.TERM f -> C.FORMULA f
initP g = C.mkPredication initPred [g]

initCASL :: Library -> C.FORMULA f
initCASL lib = C.mkForall [C.mkVarDecl gTok confSort]  equiv
  where
    equiv = initP gTerm `C.mkEqv` defForm
    gTok  = str2Token "g"
    gTerm = confVar gTok
    defForm = foldOrC [ (ctrl gTerm `C.mkStEq` mkState s0) `andC` stForm2CASL gTok gTok phi0
                      | (s0,phi0) <- initL lib
                      ]
      
      

lib2EDHML :: Library -> EDHML
lib2EDHML lib = assert `seq` edhmlRmNotNot (computeEDHML c0 is vs bs im1 im2 es)
  where
    (c0, states) = Set.deleteFindMin $ statesL lib
    guard2phi = maybe TrueF id

    primeVars (VarT v)     = VarT $ prime v
    primeVars x@(ConstT _) = x
    primeVars (x :+ y)     = primeVars x :+ primeVars y
    primeVars (x :- y)     = primeVars x :- primeVars y
    primeVars (x :* y)     = primeVars x :* primeVars y
    primeVars (x :/ y)     = primeVars x :/ primeVars y

    acts2psi Nothing = TrueF
    acts2psi (Just (Acts as)) = foldAndF [ CompF (VarT v) Eq (primeVars t)
                                         | Assign v t <- as
                                         ]
    im1 c = Map.findWithDefault id c (
              Map.fromListWith (.) -- for equal keys: compose diff lists
                [ ( c
                  , ([(guard2phi guard, e, acts2psi acts, c')] ++) -- difference list
                  )
                | TransI c c' (Label e guard acts) <- transL lib
                ] 
            ) []
    im2 ce = Map.findWithDefault id ce (
               Map.fromListWith (.) -- for equal keys : compose diff lists
                 [ ( (c, ename, length evars)
                   , ([(guard2phi guard, acts2psi acts, c')] ++) -- difference list
                   )
                 | TransI c c' (Label (EvtI ename evars) guard acts) <- transL lib
                 ]
             ) []
    vs = Set.toList states
    bs = c0 : vs
    is = im1 c0
    es = Map.elems $ actsL lib
    assert = if Set.null (statesL lib)
             then error "Can't translate lib to EDHML: need at least one state"
             else ()

computeEDHML :: STATE
             -> [(FORMULA, EVENT_ITEM, FORMULA, STATE)]
             -> [STATE]
             -> [STATE]
             -> (STATE -> [(FORMULA, EVENT_ITEM, FORMULA, STATE)])
             -> ( (STATE, EVENT_NAME, Int) -> [(FORMULA, FORMULA, STATE)]
                )
             -> [EVENT_ITEM]
             -> EDHML
computeEDHML c ((phi,e,psi,c'):is) vs bs im1 im2 es =
  At c (
    DiaAE e phi psi $ St c'
  ) `andE` computeEDHML c is vs bs im1 im2 es
computeEDHML c [] (c':vs) bs im1 im2 es = computeEDHML c' (im1 c') vs bs im1 im2 es
computeEDHML c [] []      bs im1 im2 es = fin bs im2 es `andE` pairsDiff bs

pairsDiff :: [Token] -> EDHML
pairsDiff bs = foldAndE [ Not $ At c1 $ St c2 | c1 <- bs, c2 <- bs ]

fin :: [STATE]
    -> ( (STATE, EVENT_NAME, Int) -> [(FORMULA, FORMULA, STATE)]
       )
    -> [EVENT_ITEM]
    -> EDHML
fin bs im2 es = foldAndE
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
                               | (ps, nps) <- complements c e im2
                               ]
                    | e <- es
                    ]
  | c <- bs
  ]

          -- TODO use Set
          -- problem: powerSet reported as not exported
complements :: -- (Eq a, Ord t) =>
               STATE
            -> EVENT_ITEM
            -> (
                 (STATE, EVENT_NAME, Arity) -> [(FORMULA, FORMULA, STATE)]
               )
            -> [([(FORMULA, FORMULA, STATE)], [(FORMULA, FORMULA, STATE)])]
complements c (EvtI ename evars) im2 =
  let xs = im2 (c, ename, length evars)
  in [ ( ps
       , xs List.\\ ps
       )
     | ps <- subsequences xs 
     ]


-- CASL logic helpers
andC :: C.FORMULA f -> C.FORMULA f -> C.FORMULA f
a `andC` b = C.conjunct [a, b]

orC :: C.FORMULA f -> C.FORMULA f -> C.FORMULA f
a `orC` b = C.disjunct [a, b]

notC :: C.FORMULA f -> C.FORMULA f
notC a = C.Negation a nullRange

foldAndC :: [C.FORMULA f] -> C.FORMULA f
foldAndC = C.conjunct

foldOrC :: [C.FORMULA f] -> C.FORMULA f
foldOrC  = C.disjunct

-- state FORMULA logic helpers

foldAndF :: [FORMULA] -> FORMULA
foldAndF []   = TrueF
foldAndF phis = List.foldl1 (:/\) phis

foldOrF :: [FORMULA] -> FORMULA
foldOrF  []   = FalseF
foldOrF  phis = List.foldl1 (:\/) phis

-- EDHML logic helpers
andE :: EDHML -> EDHML -> EDHML
phi `andE` psi = Not (Not phi `Or` Not psi)

foldAndE :: [EDHML] -> EDHML
foldAndE [] = TrueE
foldAndE fs = List.foldl1 andE fs

foldOrE :: [EDHML] -> EDHML
foldOrE [] = Not TrueE
foldOrE fs = List.foldl1 Or fs

-- TODO handle EDHML signatures

edhmlRmNotNot :: EDHML -> EDHML
edhmlRmNotNot (Not (Not f))       = edhmlRmNotNot f
edhmlRmNotNot (Not f)             = Not             $ edhmlRmNotNot f
edhmlRmNotNot (Binding c f)       = Binding c       $ edhmlRmNotNot f
edhmlRmNotNot (At c f )           = At c            $ edhmlRmNotNot f
edhmlRmNotNot (Box f)             = Box             $ edhmlRmNotNot f
edhmlRmNotNot (DiaEE e phi f)     = DiaEE e phi     $ edhmlRmNotNot f
edhmlRmNotNot (DiaAE e phi psi f) = DiaAE e phi psi $ edhmlRmNotNot f
edhmlRmNotNot (Or f f')           = edhmlRmNotNot f `Or` edhmlRmNotNot f'
edhmlRmNotNot f                   = f

edhml2CASL :: EDHML -> VAR_NAME -> C.FORMULA f
edhml2CASL (DtSen f) g = stForm2CASL g (prime g) f
edhml2CASL (St s) g = mkState s `C.mkStEq` ctrl (confVar g) -- TODO states bound or as ops?
edhml2CASL (Binding s f) g = exCtrl s (
                                      mkState s `C.mkStEq` ctrl (confVar g)
                               `andC` edhml2CASL f g -- TODO check index "S union {s}"
                             )
edhml2CASL (At s f) g =
  let g' = prime g
  in univConf g' (
                  (
                           (ctrl (confVar g') `C.mkStEq` mkState s)
                    `andC` reach2 {- TODO F? -} g'
                  )
       `C.mkImpl` edhml2CASL f g'
     )
edhml2CASL (Box f) g =
  let g' = prime g
  in univConf g' (
       reach3 {- F? -} g g' `C.mkImpl` edhml2CASL f g'
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
edhml2CASL TrueE     g = C.trueForm

compOp2CASL :: COMP_OP -> C.PRED_SYMB
compOp2CASL op = C.mkQualPred (str2Id ("__" ++ show op ++ "__"))
                              (C.Pred_type [natSort,natSort] nullRange)

stForm2CASL :: Token -> Token -> FORMULA -> C.FORMULA f
stForm2CASL g g' (CompF x op y) =
  compOp2CASL op `C.mkPredication` [ term2CASLterm g g' x
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
term2CASLterm g g' (ConstT natLit) = natLit2CASL $ show natLit
term2CASLterm g g' (x:+y)          = translOp g g' "+" x y
term2CASLterm g g' (x:-y)          = translOp g g' "-" x y
term2CASLterm g g' (x:*y)          = translOp g g' "*" x y
term2CASLterm g g' (x:/y)          = translOp g g' "/" x y

natLit2CASL :: String -> C.TERM f
natLit2CASL ds = List.foldl (%%)
                            (constTerm (str2Token "0") natSort)
                            [constTerm (str2Token [d]) natSort | d <- ds]

(%%) :: C.TERM f -> C.TERM f -> C.TERM f
d %% e = C.mkAppl (op (str2Token "__%%__") [natSort,natSort] natSort)
                  [d,e]

translOp :: Token -> Token -> String -> TERM -> TERM -> C.TERM f
translOp g g' opName x y = C.mkAppl (op (str2Token opName) [natSort,natSort] natSort)
                                    (term2CASLterm g g' <$> [x,y])

-- TODO: consider bound variables as opposed to attributes
attr2CASLterm :: Token -> Token -> Token -> C.TERM f
attr2CASLterm g g' var@(Token varName _) =
  if primed varName
  then C.mkAppl (op (str2Token $ unprime varName) [confSort] natSort)
                [C.Qual_var g' confSort nullRange]
  else C.mkAppl (op var [confSort] natSort)
                [C.Qual_var g confSort nullRange]

primed :: String -> Bool
primed = (=='\'') . last

-- | assumes primed
unprime :: String -> String
unprime = reverse . drop 1 . reverse

eventItem2CASLterm :: EVENT_ITEM -> C.TERM f
eventItem2CASLterm (EvtI (Token name _) vars) =
  C.mkAppl ( op (str2Token ("evt_" ++ name))
                (const natSort <$> vars)
                evtSort
           )
           (flip C.mkVarTerm natSort <$> vars)

str2Id :: String -> Id
str2Id = token2Id . str2Token

token2Id :: Token -> Id
token2Id tok   = Id [tok] [] nullRange

str2Token :: String -> Token
str2Token name = Token name nullRange

exEvtArgs :: [C.VAR] -> C.FORMULA f -> C.FORMULA f
exEvtArgs   []    phi = phi
exEvtArgs   evars phi = C.mkExist  [C.Var_decl evars natSort nullRange] phi

univEvtArgs :: [C.VAR] -> C.FORMULA f -> C.FORMULA f
univEvtArgs []    phi = phi
univEvtArgs evars phi = C.mkForall [C.Var_decl evars natSort nullRange] phi

exCtrl :: C.VAR -> C.FORMULA f -> C.FORMULA f
exCtrl s f = C.mkExist [C.mkVarDecl s ctrlSort] f

exConf :: C.VAR -> C.FORMULA f -> C.FORMULA f
exConf   g f = C.mkExist  [C.mkVarDecl g confSort] f

univConf :: C.VAR -> C.FORMULA f -> C.FORMULA f
univConf g f = C.mkForall [C.mkVarDecl g confSort] f

prime :: Token -> Token
prime (Token var _) = str2Token (var++"'")

op :: Token -> [C.SORT] -> C.SORT -> C.OP_SYMB
op name argTys resTy = C.mkQualOp (token2Id name)
                                  (C.Op_type C.Total argTys resTy nullRange)

constTerm :: Token -> C.SORT -> C.TERM f
constTerm opName sort = op opName [] sort `C.mkAppl` []

mkState :: Token -> C.TERM f
mkState name = op name [] ctrlSort `C.mkAppl` []

mkVar :: C.VAR -> C.SORT -> C.TERM f
mkVar name sort = C.toQualVar $ C.mkVarDecl name sort


trans :: C.VAR -> EVENT_ITEM -> C.VAR -> C.FORMULA f
trans g e g' = C.mkPredication transPred [ confVar g
                                         , eventItem2CASLterm e
                                         , confVar g'
                                         ]

confVar :: C.VAR -> C.TERM f
confVar g = C.mkVarTerm g confSort

ctrl :: C.TERM f -> C.TERM f
ctrl g = ctrlOp `C.mkAppl` [g]

reach2 :: C.VAR -> C.FORMULA f
reach2 g = C.mkPredication reach2Pred [confVar g]

reach3 :: C.VAR -> C.VAR -> C.FORMULA f
reach3 g g' = C.mkPredication reach3Pred (confVar <$> [g,g'])

initPred, transPred, reach2Pred, reach3Pred :: C.PRED_SYMB
initPred   = C.mkQualPred (str2Id "init")
                          (C.Pred_type [confSort] nullRange)
transPred  = C.mkQualPred (str2Id "trans")
                          (C.Pred_type [confSort,evtSort,confSort] nullRange)
reach2Pred = C.mkQualPred (str2Id "reachable2")
                          (C.Pred_type [confSort] nullRange)
reach3Pred = C.mkQualPred (str2Id "reachable3")
                          (C.Pred_type [confSort,confSort] nullRange)

ctrlOp :: C.OP_SYMB
ctrlOp = op (str2Token "ctrl") [confSort] ctrlSort

ctrlSort, evtSort, natSort, confSort :: C.SORT
ctrlSort = str2Id "Ctrl"
evtSort  = str2Id "Evt"
natSort  = str2Id "Nat"
confSort = str2Id "Conf"
