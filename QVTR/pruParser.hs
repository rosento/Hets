import System.IO
import System.FilePath (replaceBaseName, replaceExtension, takeBaseName)

import qualified QVTR.As as QVTR
import QVTR.Print
import qualified CSMOF.As as CSMOF

import Text.ParserCombinators.Parsec

import Common.Parsec

-- From the QVTR folder run: ghc -i.. -o main pruParser.hs

main :: IO ()
main = do  
    handle <- openFile "uml2rdbms.qvt" ReadMode  
    input <- hGetContents handle 
    case runParser pTransformation () "uml2rdbms.qvt" input of  -- Either ParseError String
      Left err -> print err
      Right result -> print result


-- Parse a QVTR model transformation
-- <transformation> ::= <header> 
--                      '{' <keyDecl>* <relation>* '}'

pTransformation :: CharParser st QVTR.Transformation
pTransformation = do
  skip
  (name,souMeta,tarMeta) <- pTransfHeader
  skip
  char '{'
  skip
  keys <- many (try pKeyDecl)
  skip
  relations <- many pRelation
  skip
  char '}'
  skip
  eof
  return (QVTR.Transformation 
            name 
            souMeta
            tarMeta
            keys
            [] -- relations
         )


-- Parse a transformation header without source and target CSMOF.Metamodel (just names)
-- <header> ::= 'transformation' <identifier>
--              '(' <modelDecl> ',' <modelDecl> ')'
-- <modelDecl> ::= <modelId> ':' <metaModelId>
-- <modelId> ::= <identifier>
-- <metaModelId> ::= <identifier>

pTransfHeader :: CharParser st (String, (String,String,CSMOF.Metamodel),(String,String,CSMOF.Metamodel))
pTransfHeader = do
  pKey "transformation"
  skip
  name <- pIdentifier
  skip
  list <- pBetParent $ pCommaSep $ pColonSep $ (skip >> pIdentifier << skip)
  return (  name,
            (head $ head list, head $ tail $ head list, emptyMetamodel),
            (head $ head $ tail list, head $ tail $ head $ tail list, emptyMetamodel)
         )


emptyMetamodel :: CSMOF.Metamodel
emptyMetamodel = CSMOF.Metamodel "" [] []


-- Parse keys of the transfromation
-- <keyDecl> ::= 'key' <classId> '{' <keyProperty> (, <keyProperty>)* '}' ';'
pKeyDecl :: CharParser st QVTR.Key
pKeyDecl = do
  skip
  pKey "key"
  skip
  classId <- pClassId
  skip
  list <- pBetBraces $ pCommaSep $ pKeyProperty
  skip
  char ';'
  return (QVTR.Key (fst classId) (snd classId) list) -- ToDo


-- <classId> ::= <identifier> '::' <identifier>
pClassId :: CharParser st (String,String)
pClassId = do
  met <- pIdentifier
  char ':'
  char ':'
  typ <- pIdentifier
  return ((met,typ))


-- <keyProperty> ::= <identifier>
--                 | 'opposite' '(' <identifier> '.' <identifier> ')'
pKeyProperty :: CharParser st QVTR.PropKey
pKeyProperty = do
    pKey "opposite"
    skip
    oppo <- pBetParent $ pFullName
    return (QVTR.OppositeProp (fst oppo) (snd oppo))
  <|>
    do
    skip
    ident <- pIdentifier
    return (QVTR.SimpleProp ident)


-- <identifier> '.' <identifier>
pFullName :: CharParser st (String,String)
pFullName = do
  cla <- pIdentifier
  char '.'
  typ <- pIdentifier
  return ((cla,typ))


-- Parse transformation rules
-- <relation> ::= ['top'] 'relation' <identifier>
--                '{'
--                <varDeclaration>*
--                <primitiveTypeDomain>*
--                <domain> <domain>
--                [<when>] [<where>]
--                '}'

pRelation :: CharParser st QVTR.Relation
pRelation = do
  skip
  top <- pIsTop
  skip
  iden <- pIdentifier
  skip
  char '{'
  skip
  varSet <- many pVarDeclaration
  skip
  primDom <- many pPrimitiveTypeDomain
  skip
  sourceDom <- pDomain
  skip
  targetDom <- pDomain
  skip
  whenCon <- option Nothing pWhen
  skip
  whereCon <- option Nothing pWhere
  skip
  char '}'
  return ( QVTR.Relation top iden (concat varSet) primDom 
                     sourceDom targetDom whenCon whereCon )

-- Parse if a relation is top or not
pIsTop :: CharParser st Bool
pIsTop = do 
  skip
  pKey "top" 
  skip
  pKey "relation" 
  return (True)
  <|>
  do skip
     pKey "relation"
     return (False)


-- Parse var declaration
-- <varDeclaration> ::= <identifier> (, <identifier>)* ':' <TypeCS> ';'

pVarDeclaration :: CharParser st [QVTR.RelVar]
pVarDeclaration = do
  skip
  vars <- pCommaSep pIdentifier
  skip
  char ':'
  skip
  typ <- pTypeCS
  skip
  char ';'
  return ( map (\nam -> (QVTR.RelVar typ nam)) vars )


-- <TypeCS> ::= <identifier> '::' <identifier>
--            | <identifier>

pTypeCS :: CharParser st String
pTypeCS = do
  met <- pIdentifier
  try (do char ':'
          char ':'
          typ <- pIdentifier
          return (typ)
      )
  return (met)



-- Parse primitive domain
-- <primitiveTypeDomain> ::= 'primitive' 'domain' <identifier> ':' <TypeCS> ';'

pPrimitiveTypeDomain :: CharParser st QVTR.PrimitiveDomain
pPrimitiveTypeDomain = do
  skip
  pKey "primitive" 
  skip
  pKey "domain" 
  skip
  nam <- pIdentifier
  skip
  char ':'
  skip
  typ <- pTypeCS
  skip
  char ';'
  return ( QVTR.PrimitiveDomain nam typ ) 


-- <domain> ::= 'domain' <modelId> <template> ';'

pDomain :: CharParser st QVTR.Domain
pDomain = do
  skip
  (pKey "checkonly" <|> pKey "enforce")
  skip
  pKey "domain" 
  skip
  modelId <- pIdentifier
  skip
  (var,typ,pat) <- pTemplate
  skip
  char ';'
  return ( QVTR.Domain modelId var typ pat )


-- <template> ::= <objectTemplate>
-- <objectTemplate> ::= <identifier> ':' <pathNameCS>
--                     '{' [<propertyTemplateList>] '}'

pTemplate :: CharParser st (String,String,QVTR.Pattern)
pTemplate = do
  skip
  id <- pIdentifier
  skip
  char ':'
  skip
  typ <- pClassId
  skip
  char '{'
  skip
  tempList <- option (QVTR.Pattern [] [] "") pPropertyTemplateList
  skip
  char '}'
  return ( (id,(snd typ),tempList) )


-- <propertyTemplateList> ::= <propertyTemplate> (',' <propertyTemplate>)*

pPropertyTemplateList :: CharParser st QVTR.Pattern
pPropertyTemplateList = do
  tempList <- pCommaSep pPropertyTemplate
  return (QVTR.Pattern [] [] "") --ToDo: procesar la salida para armar el Pattern

-- <propertyTemplate> ::= <identifier> '=' <OclExpressionCS>
--                     | <identifier> '=' <objectTemplate>

pPropertyTemplate :: CharParser st String
pPropertyTemplate = do
  skip
  ident <- pIdentifier
  skip
  char '='  
  skip
  (do try pTemplate 
      return ("") --ToDo
   <|> 
   do pOCLExpressionNoEnd
      return ("")) --ToDo
  

-- <when> ::= 'when' '{' (<OclExpressionCS> ';')* '}'
pWhen :: CharParser st (Maybe QVTR.WhenWhere)
pWhen = do
  skip
  pKey "where" 
  skip
  char '{'
  skip
  relInvok <- many (try pRelInvocation)
  oclExpre <- many pOCLExpression
  skip
  char '}'
  return ( Just (QVTR.When relInvok oclExpre) )


-- <where> ::= 'where' '{' (<RelInvocation> ';')* (<OclExpressionCS> ';')* '}'
pWhere :: CharParser st (Maybe QVTR.WhenWhere)
pWhere = do
  skip
  pKey "where" 
  skip
  char '{'
  skip
  relInvok <- many (try pRelInvocation)
  oclExpre <- many pOCLExpression
  skip
  char '}'
  return ( Just (QVTR.Where relInvok oclExpre) )


-- <RelInvocation> ::= <identifier> '(' (<identifier> ',')* ')' ';'
pRelInvocation :: CharParser st QVTR.RelInvok
pRelInvocation = do
  skip
  name <- pIdentifier
  skip
  char '('
  skip
  params <- pCommaSep pIdentifier
  skip
  char ')'
  skip
  char ';'
  return ( QVTR.RelInvok name params )


pOCLExpression :: CharParser st String
pOCLExpression = do
  res <- many (noneOf ",;")
  return (res)


pOCLExpressionNoEnd :: CharParser st String
pOCLExpressionNoEnd = do
  res <- many anyChar
  return (res)


-- Auxiliary definitions

lineComment :: CharParser st String
lineComment = tryString "--" <++> many (noneOf "\n")

skip :: CharParser st ()
skip = skipMany $ forget space
  <|> forget (nestedComment "/*" "*/" <|> lineComment)

pChar :: CharParser st Char
pChar = alphaNum <|> oneOf "_'"

pKeyS :: String -> CharParser st String
pKeyS s = try (string s << notFollowedBy pChar) << skip

pKey :: String -> CharParser st ()
pKey = forget . pKeyS

pColon :: CharParser st ()
pColon = pSym ":"

pSymS :: String -> CharParser st String
pSymS s = tryString s << skip

pSym :: String -> CharParser st ()
pSym = forget . pSymS

pComma :: CharParser st ()
pComma = pSym ","

pEqual :: CharParser st ()
pEqual = pSym "="

pBetParent :: CharParser st a -> CharParser st a
pBetParent = between (char '(') (char ')')

pBetBraces :: CharParser st a -> CharParser st a
pBetBraces = between (char '{') (char '}')

pCommaSep :: CharParser st a -> CharParser st [a]
pCommaSep p  = p `sepBy` (char ',')

pColonSep :: CharParser st a -> CharParser st [a]
pColonSep p  = p `sepBy` (char ':')

pIdentifier :: CharParser st String
pIdentifier = do
  c <- letter
  rest <- many (alphaNum <|> oneOf "_")
  return (c : rest)
