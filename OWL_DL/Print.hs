{- |
Module      :  $Header$
Copyright   :  (c) Heng Jiang, Uni Bremen 2005
License     :  similar to LGPL, see HetCATS/LICENSE.txt or LIZENZ.txt

Maintainer  :  jiang@tzi.de
Stability   :  provisional
Portability :  unknown

Pretty printing for OWL DL theories.

-}

{-
    todo:
     - invent ASCII display syntax for OWL_DL theories (Klaus)
     - implement printing of a theory
-}

module OWL_DL.Print where

-- import Common.AS_Annotation
import Common.GlobalAnnotations
import Common.Lib.Pretty
import Common.PrettyPrint

import Text.XML.HXT.DOM.XmlTreeTypes
import OWL_DL.Sign
import OWL_DL.AS

import qualified Common.Lib.Set as Set
import qualified Common.Lib.Map as Map


instance PrintLaTeX Sign where
  printLatex0 = printText0

instance PrettyPrint Sign where
  -- printText0 _ = text . show
  printText0 ga (Sign _ p2 p3 p4 p5 p6 _ p8 p9 p10) =
    (text "namespaces ") $+$ (printText0 ga p10) $+$
    -- (printText0 ga p1) $+$
    ((text "concepts ") <> (foldSetToDoc ga p2)) $+$
    ((text "primaryConcepts ") <> (foldSetToDoc ga p3)) $+$
    ((text "datatypes ") <> (foldSetToDoc ga p4)) $+$
    ((text "indvidual_valued_roles ") 
                         <> (foldSetToDoc ga p5)) $+$
    ((text "data_valued_roles ") <> (foldSetToDoc ga p6)) $+$
    ((text "individuals ") <> (foldSetToDoc ga p8)) $+$
    ((text "sign_axioms") $+$ (foldSetToDoc2 ga p9)) $+$ 
    (text "") $+$ (text "Sentence:")  

instance PrettyPrint URIreference where
    printText0 _ (QN prefix localpart uri)
	| localpart == "_" = text $ show "_"
	| null prefix = text $ show (uri ++ ":" ++ localpart)
	| otherwise =	text $ show ( prefix ++ ":" ++ localpart) 

instance PrettyPrint Namespace where
    printText0 _ nsMap = 
	text $ pp (Map.toList nsMap) "" 
       where pp :: [(String, String)] -> String ->  String
	     pp [] str = str ++ "\n"
	     pp ((pre, uri):rest) str 
		 | null str = pp rest (pre ++ " ::= " ++ uri)
		 | otherwise =	       
		     pp rest (str ++ "\n" ++ pre ++ " ::= " ++ uri) 
	   
instance PrintLaTeX Sentence where
  printLatex0 = printText0

instance PrettyPrint Sentence where
    printText0 ga sent = 
	case sent of
	  OWLAxiom axiom -> printText0 ga axiom
	  OWLFact fact   -> printText0 ga fact

instance PrettyPrint Ontology where
    printText0 ga (Ontology maybeID directives ns) =
	(text "Ontology") <+> 
	    (case maybeID of
	     Just oid -> printText0 ga oid
	     _        -> text "anomie"
	    ) $+$
	(text $ show $ map (printText0 ga) directives) $+$
	(printText0 ga ns)

instance PrintLaTeX Ontology where
 printLatex0 = printText0

instance PrettyPrint Directive where
    printText0 _ = text . show

instance PrettyPrint Axiom where
    printText0 ga axiom =
	case axiom of
	DisjointClasses desc1 desc2 _ ->     -- description list is ignored
	    (text "(forall ((x owl:Thing)) (not (and (") <+> 
	          (printDescription ga emptyQN desc1) <+>
                  (text "x) (") <+> 
	          (printDescription ga emptyQN desc2) <+>
	          (text "x))))")
	EquivalentClasses desc1 (desc2:_) ->  -- description list is ignored
          case desc2 of
	  DC cid ->
	    (text "(forall ((x owl:Thing)) (iff (") <+> 
	         (printDescription ga emptyQN desc1) <+>
		 (text "x) (") <+> 
	         (printText0 ga cid) <+>
	         (text "x)))")	
	  _      -> 
	      case desc1 of
	        DC cid1 -> printDescription ga cid1 desc2
	        _       -> error ("EquivalentClasses Error:" ++ (show axiom))
	Datatype dID _ _ -> 
	    printText0 ga dID
	DEquivalentProperties dpID1 dpID2 _ ->
	    (text "(forall ((x owl:Thing)) (y owl:Thing)) (iff (") <+> 
	                (printText0 ga dpID1) <+>
			(text "x y) (") <+> (printText0 ga dpID2) <+>
			(text "x y)))")	
	IEquivalentProperties ipID1 ipID2 _ ->
	    (text "(forall ((x owl:Thing)) (y owl:Thing)) (iff (") <+> 
                        (printText0 ga ipID1) <+>
			(text "x y) (") <+> (printText0 ga ipID2) <+>
			(text "x y)))")	
	DSubPropertyOf dpID1 dpID2 ->
	    (text "(forall ((u rdfs:Resource) (y rdfs:Resource)) (implies (") 
	            <+> (printText0 ga dpID1) <+> (text "u y) (") 
	            <+> (printText0 ga dpID2) <+> (text "u y)))")
	ISubPropertyOf ipID1 ipID2 ->
	    (text "(forall ((u rdfs:Resource) (y rdfs:Resource)) (implies (") 
                    <+> (printText0 ga ipID1) <+> (text "u y) (") 
	            <+> (printText0 ga ipID2) <+> (text "u y)))")
	ObjectProperty iid p2 p3 p4 maybeInverse isSymmetric maybeFunc p8 p9 ->
	    case maybeInverse of
	    Just pid -> (text "(forall ((x owl:Thing) (y owl:Thing)) (iff (")<>
			(printText0 ga iid) <+> (text "x y) (") <>
			(printText0 ga pid) <+> (text "y x) (") $+$
			(printText0 ga (ObjectProperty iid p2 p3 p4 
				Prelude.Nothing isSymmetric maybeFunc p8 p9))
	    _ -> if isSymmetric then
		    (text "(forall ((x owl:Thing) (y owl:Thing)) (implies (")<>
		    (printText0 ga iid) <+> (text "x y) (") <>
		    (printText0 ga iid) <+> (text "y x) (") $+$
		    (printText0 ga (ObjectProperty iid p2 p3 p4 maybeInverse 
				                       False maybeFunc p8 p9))
		    else 
		      case maybeFunc of
		         Just InverseFunctional ->
			    (text "(forall ((x owl:Thing) (y owl:Thing)") <+>
			    (text "(z owl:Thing)) (implies") $+$
			    (nest 2 $ text "(and (") <> (printText0 ga iid) <+>
			    (text "y x) (") <> (printText0 ga iid) <+> 
			    (text "z x))") $+$
			    (nest 2 $ text "(= y z)") $+$
			    (text "))") 
                         Just Transitive ->  
			    (text "(forall ((x owl:Thing) (y owl:Thing)") <+> 
			    (text "(z owl:Thing)) (implies") $+$
			    (nest 2 $ text "(and (") <> (printText0 ga iid) <+>
			    (text "x y) (") <> (printText0 ga iid) <+> 
			    (text "y Z))") $+$
			    (nest 2 $ text "(") <> (printText0 ga iid) <+>
			    (text "x z)") $+$
			    (text "))")
			 _  -> text "" 
	u -> text $ show u              -- annotation is not yet instanced

instance PrettyPrint SignAxiom where
    printText0 ga signAxiom =
	case signAxiom of
	Subconcept cid1 cid2 -> 
	    (text "(forall ((u rdfs:Resource)) (implies (" <> 
	             (printText0 ga cid1) <> 
	             (text " u) (") <> (printText0 ga cid2) <> (text " u)))"))
	RoleDomain rid rdomains ->
	    foldListToDocH ga  
	    (\x y -> (text "(forall ((u rdfs:Resource) (y rdfs:Resource))") $+$
		     (nest 2 $ text "(implies (") <> (printText0 ga x) <+> 
	             (text "u y) (") <> (printText0 ga y) <> (text " u)") $+$
	             (text "))"))
	    rid rdomains
	RoleRange rid rranges ->
	    foldListToDocH ga  
	    (\x y -> (text "(forall ((u rdfs:Resource) (y rdfs:Resource))") $+$
	             (nest 2 $ text "(implies (") <> (printText0 ga x) <+> 
	             (text "u y) (") <> (printText0 ga y) <> (text " y)") $+$
	             (text "))"))
	    rid rranges
	FuncRole rid ->
          (text "(and") $+$
	  (nest 2 $ text "(forall ((x owl:Thing) (y owl:Thing) (z owl:Thing))")
	  $+$ (nest 4 $ text "(implies") $+$
	    ((nest 6 $ text "(and (") <> (printText0 ga rid) <+> 
	     (text "x y) (") <+> (printText0 ga rid) <+> (text "x z))")) $+$
	    (nest 6 $ text "(= y z)") $+$
	    (nest 2 $ text "))") $+$
	    (nest 2 $ 
	     text "(forall ((x owl:Thing) (y rdfs:Literal) (z rdfs:Literal))") 
	      $+$ (nest 4 $ text "(implies") $+$
	    ((nest 6 $ text "(and (") <> (printText0 ga rid) <+> 
	     (text "x y) (") <+> (printText0 ga rid) <+> (text "x z))")) $+$
	    (nest 6 $ text "(= y z)") $+$
	    (nest 2 $ text "))") $+$ (text ")")
	Conceptmembership iID desc ->
	    (text "(") <> (printDescription ga iID desc) 
		     <+> (printText0 ga iID) <> (text ")")

instance PrettyPrint RDomain where
    printText0 ga (RDomain desc) =
	printDescription ga emptyQN desc

instance PrettyPrint RRange where
    printText0 ga rRange =
	case rRange of
	RIRange desc -> printDescription ga emptyQN desc
	RDRange dataRange -> printText0 ga dataRange 

instance PrettyPrint DataRange where
    printText0 ga dr =
      case dr of
	DID dtID -> printText0 ga dtID
	OneOfData dls ->
	    (text "(forall ((x rdfs:Literal))") $+$
	      (nest 2 $ text "(iff (") <> (text "class") <+> (text "x)") $+$
	      (nest 3 $ text "(or (") <> 
	      (foldListToDocH ga (form4 ga) emptyQN dls) <> (char ')') $+$
	      (text "))")
	RLit rdfLit   -> (text "rdf_literal") <+> (text rdfLit)

instance PrettyPrint DataLiteral where
    printText0 ga dl =
	case dl of
	TypedL (lf, uri) -> (char '(') <> (printText0 ga uri) <+>
			    (text ("'" ++ lf ++ "')"))
	PlainL (lf, lt)  -> text ("(stringInLang '" ++ lf ++ "' '" ++ lt ++
				  "')")
	Plain lf         -> text ("'" ++ lf ++ "'")
	RDFSL rdfLit     -> -- (text "rdf_literal") <+> (text rdfLit)
			    text ("'" ++ rdfLit ++ "'")

instance PrettyPrint Restriction where
    printText0 ga restr =
	case restr of
	DataRestriction dpID drcom drcoms ->
	    printRestriction1 ga dpID (drcom:drcoms)
	IndivRestriction ipID ircom ircoms ->
	    printRestriction2 ga ipID (ircom:ircoms)

instance PrettyPrint Description where
    printText0 ga desc =
	printDescription ga emptyQN desc

instance PrettyPrint Fact where
    printText0 ga fact =
	case fact of
	SameIndividual iid1 iid2 iids -> 
	    foldListToDocV ga (\x y -> (text "(=") <+> (printText0 ga x)
			       <+> (printText0 ga y) <> (char ')'))
	                   iid1 (iid2:iids)
	DifferentIndividuals iid1 iid2 iids -> 
	    (text "(forall ((x owl:Thing)) (iff") $+$
	      (nest 4 $ text "(") <> (text "class") <+> (text "x)") $+$
	      (nest 3 $ text "(or (") <> 
	      (foldListToDocH ga (form2 ga) emptyQN (iid1:iid2:iids)) <> 
	      (char ')') $+$
	      (text "))") $+$
	      (text "[ALLDIFFERENT") <+> 
	      (foldListToDocH ga (form3 ga) emptyQN (iid1:iid2:iids)) <> 
	      (char ']')
	u -> text $ show u

printRestriction1 :: GlobalAnnos -> URIreference -> [Drcomponent] -> Doc
printRestriction1 _ _ [] = empty
printRestriction1 ga dpID (h:r) =
    case h of
    DRCAllValuesFrom dataRange -> 
	(text "(forall ((x owl:Thing)) (iff") $+$
	(nest 2 $ text "(restriction x)") $+$
	(nest 2 $ text "(forall (y) (implies (") <> (printText0 ga dpID) <+>
	   (text "x y) (") <> (printText0 ga dataRange) <+>
	   (text "y)))") $+$
	(text "))") $+$
	(printRestriction1 ga dpID r)
    DRCSomeValuesFrom dataRange -> 
	(text "(forall ((x owl:Thing)) (iff") $+$
	(nest 2 $ text "(restriction x)") $+$
	(nest 2 $ text "(exists (y) (and (") <> (printText0 ga dpID) <+>
	   (text "x y) (") <> (printText0 ga dataRange) <+>
	   (text "y)))") $+$
	(text "))") $+$
	(printRestriction1 ga dpID r)
    DRCValue dl -> 
	(text "(forall ((x owl:Thing)) (iff (restriction x) (") <>
	  (printText0 ga dpID) <+> (text "x") <+>
	  (printText0 ga dl) <> (text ")))") $+$
	(printRestriction1 ga dpID r)
    DRCCardinality cardinality -> 
	(printCard ga dpID cardinality) $+$
	(printRestriction1 ga dpID r)

printRestriction2 :: GlobalAnnos -> URIreference -> [Ircomponent] -> Doc
printRestriction2 _ _ [] = empty
printRestriction2 ga ipID (h:r) =
    case h of
    IRCAllValuesFrom desc -> 
	(text "(forall ((x owl:Thing)) (iff") $+$
	(nest 2 $ text "(restriction x)") $+$
	(nest 2 $ text "(forall (y) (implies (") <> (printText0 ga ipID) <+>
	   (text "x y) (") <> (printDescription ga ipID desc) <+>
	   (text "y)))") $+$
	(text "))") $+$
	(printRestriction2 ga ipID r)
    IRCSomeValuesFrom desc -> 
	(text "(forall ((x owl:Thing)) (iff") $+$
	(nest 2 $ text "(restriction x)") $+$
	(nest 2 $ text "(exists (y) (and (") <> (printText0 ga ipID) <+>
	   (text "x y) (") <> (printDescription ga ipID desc) <+>
	   (text "y)))") $+$
	(text "))") $+$
	(printRestriction2 ga ipID r)
    IRCValue iid -> 
	(text "(forall ((x owl:Thing)) (iff (restriction x) (") <>
	  (printText0 ga ipID) <+> (text "x") <+>
	  (printText0 ga iid) <> (text ")))") $+$
	(printRestriction2 ga ipID r)
    IRCCardinality cardinality -> 
	(printCard ga ipID cardinality)  $+$
	(printRestriction2 ga ipID r)

printCard :: GlobalAnnos -> URIreference -> Cardinality -> Doc
printCard ga pid card =
    case card of
    MinCardinality n -> 
      (text "(forall ((x owl:Thing)) (implies") $+$
	(nest 2 $ text "(restriction x)") $+$
	(nest 2 $ text "(exists ((x1 owl:Thing) ... (x") <> (int n) <+>
                                  (text "owl:Thing)) (and ") $+$
	(nest 4 $ text "[ALLDIFFERENT x1 ... x") <> (int n) <>
                                  (text "]") $+$
	(nest 4 $ text "(") <> (printText0 ga pid) <+> (text "x x1) ...") <+>
	   (text "(") <> (printText0 ga pid) <+> (text "x x") <> (int n) <>
			          (text ")") $+$
	(nest 2 $ text "))") $+$
      (text "))")
    MaxCardinality n -> 
      (text "(forall ((x owl:Thing) (x1 owl:Thing) ... (x") <> (int (n+1)) <+>
                  (text "owl:Thing)) (implies") $+$
	(nest 2 $ text "(and (restriction x)") $+$
	(nest 4 $ text "(") <> (printText0 ga pid) <+> (text "x x1) ...")<+>
	     (text "(") <> (printText0 ga pid) <+> (text "x x") <> 
		(int (n+1)) <> (text "))") $+$
	(nest 2 $ (text "not [ALLDIFFERENT x1 ... x")) <> (int (n+1)) <>
                                  (text "])") $+$
	(text "))") 
    Cardinality n ->
      (text "(forall ((x owl:Thing)) (implies") $+$
	(nest 2 $ text "(restriction x)") $+$
	(nest 2 $ text "(exists ((x1 owl:Thing) ... (x") <> (int n) <+>
                                  (text "owl:Thing)) (and ") $+$
	(nest 4 $ text "[ALLDIFFERENT x1 ... x") <> (int n) <>
                                  (text "]") $+$
	(nest 4 $ text "(") <> (printText0 ga pid) <+> (text "x x1) ...") <+>
	   (text "(") <> (printText0 ga pid) <+> (text "x x") <> (int n) <>
			          (text ")") $+$
	(nest 4 $ text "(forall ((z owl:Thing)) (implies") $+$
	(nest 6 $ text "(") <> (printText0 ga pid) <+> (text "x z)") $+$
	(nest 6 $ text "(or (= z x1) ... (= z x") <> (int n) <> 
	            (text "))") $+$
	(nest 4 $ text "))") $+$
	(nest 2 $ text "))") $+$
	(text "))")

printDescription :: GlobalAnnos -> URIreference -> Description -> Doc
printDescription ga iD desc =
    case desc of
    	DC cid -> printText0 ga cid
	DR restr -> printText0 ga restr
	UnionOf descs -> 
	    (text "(forall ((x owl:Thing))") $+$
	    (nest 2 $ text "(iff (") <> (printText0 ga iD) <+> (text "x)") $+$
	    (nest 3 $ text "(or (") <> (foldListToDocH ga (form1 ga) iD descs)
					<> (char ')') $+$
	    (text "))")	
	IntersectionOf descs -> 
	    (text "(forall ((x owl:Thing))") $+$
	      (nest 2 $ text "(iff (") <> (printText0 ga iD) <+> 
	      (text "x)") $+$ (nest 3 $ text "(and (") <> 
	      (foldListToDocH ga (form1 ga) iD descs) <> (char ')')  $+$
	      (text "))")
	ComplementOf desc1 ->
	    (text "(forall ((x owl:Thing)) (iff (") <> (printText0 ga iD) <+>
	            (text "x) (not (") <> (printDescription ga iD desc1) <+>
		    (text "x))))")
	OneOfDes inds -> 
	    (text "(forall ((x owl:Thing)) ") $+$
	    (nest 2 $ text "(iff (") <> (printText0 ga iD) <+> (text "x)") $+$
	    (nest 3 $ text "(or (") <> (foldListToDocH ga (form2 ga) iD inds) 
					<> (char ')') $+$
	    (text "))")	

foldSetToDoc :: (PrettyPrint a) => GlobalAnnos -> Set.Set a -> Doc
foldSetToDoc ga idSet =
    foldr addDoc empty $ map (printText0 ga) (Set.toList idSet)
   where addDoc :: Doc -> Doc -> Doc
	 addDoc d1 d2
	     | isEmpty d2 = d1
	     | otherwise = d1 <> (text ", ") <> d2

foldSetToDoc2 :: (PrettyPrint a) => GlobalAnnos -> Set.Set a -> Doc
foldSetToDoc2 ga idSet =
    foldr addDoc empty $ map (printText0 ga) (Set.toList idSet)
   where addDoc :: Doc -> Doc -> Doc
	 addDoc d1 d2
	     | (isEmpty d1) && (isEmpty d2) = empty	
	     | isEmpty d1 = d2	
	     | isEmpty d2 = d1
	     | otherwise = d1 <> (char ',') $+$ d2

foldListToDocV :: (PrettyPrint a, PrettyPrint b) 
		 => GlobalAnnos -> (a -> b -> Doc) -> a -> [b] -> Doc
foldListToDocV _ _ _ [] = empty
foldListToDocV ga printForm iD (h:r) =
    (printForm iD h) $+$ (foldListToDocV ga printForm iD r) 

foldListToDocH :: (PrettyPrint a, PrettyPrint b) 
		 => GlobalAnnos -> (a -> b -> Doc) -> a -> [b] -> Doc
foldListToDocH _ _ _ [] = empty
foldListToDocH ga printForm iD (h:r) =
    (printForm iD h) <+> (foldListToDocH ga printForm iD r)


form1 :: GlobalAnnos -> URIreference -> Description -> Doc
form1 ga _ des = 
    (text "(") <> (printDescription ga emptyQN  des) <+> 
		   (text "x)")

form2 :: GlobalAnnos -> URIreference -> IndividualID -> Doc
form2 ga _ ind = 
    (text "(=") <+> (printText0 ga ind) <+> 
		    (text "x)")

form3 :: GlobalAnnos -> URIreference -> IndividualID -> Doc
form3 ga _ iid =  printText0 ga iid

form4 :: GlobalAnnos -> URIreference -> DataLiteral -> Doc
form4 ga _ dl = 
    (text "(=") <+> (printText0 ga dl) <+> 
		    (text "x)")

emptyQN :: QName
emptyQN = QN "" "" ""