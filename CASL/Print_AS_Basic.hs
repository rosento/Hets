
{- HetCATS/CASL/Print_AS_Basic.hs
   $Id$
   Authors: Christian Maeder
            Klaus L�ttich
   Year:    2002
   
   printing AS_BASIC data types
-}

module Print_AS_Basic where

import AS_Basic_CASL
import AS_Annotation
import GlobalAnnotations

import Print_AS_Annotation

import Keywords
import Pretty
import PrettyPrint

--debugging 
-- import IOExts (trace)

instance PrettyPrint BASIC_SPEC where
    printText0 ga (Basic_spec l) = vcat (map (printText0 ga) l) 

semiT ga l = cat(punctuate semi (map (printText0 ga) l) )

instance PrettyPrint BASIC_ITEMS where
    printText0 ga (Sig_items s) = printText0 ga s
    printText0 ga (Free_datatype l _) = text freeS <+> text typeS
				 <+> vcat(map (\x -> printText0 ga x <> semi) l)
    printText0 ga (Sort_gen l _) = text generatedS 
			       <+> braces (vcat (map (printText0 ga) l))
    printText0 ga (Var_items l _) = text varS 
				<+> semiT ga l
    printText0 ga (Local_var_axioms l f p) = text forallS 
				 <+> semiT ga l
				 $$ printText0 ga (Axiom_items f p)
    printText0 ga (Axiom_items f _) = vcat (map (\x -> text cDot  <+> printText0 ga x)
					f)

semiAnno :: (PrettyPrint a) => GlobalAnnos -> [Annoted a] -> Doc
semiAnno ga l = vcat(map (\x -> printText0 ga (l_annos x) 
			        $$ printText0 ga (item x) <> semi 
			        <> printText0 ga (r_annos x)) l) 

instance PrettyPrint SIG_ITEMS where
    printText0 ga (Sort_items l _) =  text sortS <+> semiAnno ga l 
    printText0 ga (Op_items l _) =  text opS <+> semiAnno ga l 
    printText0 ga (Pred_items l _) =  text predS <+> semiAnno ga l 
    printText0 ga (Datatype_items l _) = text typeS <+> semiAnno ga l 

commaT ga l = cat(punctuate comma (map (printText0 ga) l))

instance PrettyPrint SORT_ITEM where
    printText0 ga (Sort_decl l _) = commaT ga l
    printText0 ga (Subsort_decl l t _) = commaT ga l <> text lessS 
			       <> printText0 ga t
    printText0 ga (Subsort_defn s v t f _) = printText0 ga s 
			       <> text equalS 
			       <> braces(printText0 ga v
					  <> colon
					  <> printText0 ga t
					  <> (text cDot
					  <+> printText0 ga f))
    printText0 ga (Iso_decl l _) = 
	hcat(punctuate  (text equalS) (map (printText0 ga) l))

instance PrettyPrint OP_ITEM where
    printText0 ga (Op_decl l t a _) = commaT ga l 
				  <+> (colon
				       <> printText0 ga t
				       <> if null a then empty else comma)
				  <+> commaT ga a
    printText0 ga (Op_defn n h t _) = printText0 ga n 
				  <+> printText0 ga h
                                  <+> text equalS
				  <+> printText0 ga t

crossT ga l = hcat(punctuate (text timesS) (map (printText0 ga) l))

instance PrettyPrint OP_TYPE where
    printText0 ga (Total_op_type l s _) = (if null l then empty 
					   else crossT ga l 
				                <> text funS)
				           <> printText0 ga s
    printText0 ga (Partial_op_type l s _) = (if null l then text quMark 
					     else crossT ga l 
					          <> text (funS ++ quMark))
					    <> printText0 ga s

instance PrettyPrint OP_HEAD where
    printText0 ga (Total_op_head l s _) = 
	(if null l then empty 
	 else parens(semiT ga l))
	<> colon
	<> printText0 ga s
    printText0 ga (Partial_op_head l s _) = 
	(if null l then empty 
	 else parens(semiT ga l))
	<> text (colonS ++ quMark)
        <> printText0 ga s

instance PrettyPrint ARG_DECL where
    printText0 ga (Arg_decl l s _) = commaT ga l 
			      <> colon
			      <> printText0 ga s

instance PrettyPrint OP_ATTR where
    printText0 _ (Assoc_op_attr) = text assocS
    printText0 _ (Comm_op_attr) = text commS 
    printText0 _ (Idem_op_attr) = text idemS
    printText0 ga (Unit_op_attr t) = text unitS <+> printText0 ga t

instance PrettyPrint PRED_ITEM where
    printText0 ga (Pred_decl l t _) = commaT ga l 
				  <+> (colon
				       <> printText0 ga t)
    printText0 ga (Pred_defn n h f _) = printText0 ga n 
				        <+> printText0 ga h
                                        <+> text equivS
				        <+> printText0 ga f

instance PrettyPrint PRED_TYPE where
    printText0 ga (Pred_type [] _) = parens (empty)
    printText0 ga (Pred_type l _) = crossT ga l

instance PrettyPrint PRED_HEAD where
    printText0 ga (Pred_head l _) = parens(semiT ga l)

barT = space <> text barS <> space

instance PrettyPrint DATATYPE_DECL where
    printText0 ga (Datatype_decl s a _) = printText0 ga s 
				          <+> text defnS
				          <+> vcat(punctuate barT 
						     (map (printText0 ga) a))

instance PrettyPrint ALTERNATIVE where
    printText0 ga (Total_construct n l _) = printText0 ga n 
				 <> if null l then empty 
				    else parens(semiT ga l)
    printText0 ga (Partial_construct n l _) = printText0 ga n 
				 <> parens(semiT ga l)
				 <> text quMark
    printText0 ga (Subsorts l _) = text sortS <+> commaT ga l 

instance PrettyPrint COMPONENTS where
    printText0 ga (Total_select l s _) = commaT ga l 
				<> colon 
				<> printText0 ga s 
    printText0 ga (Partial_select l s _) = commaT ga l 
				<> text (colonS ++ quMark) 
				<> printText0 ga s 
    printText0 ga (Sort s) = printText0 ga s 	  

instance PrettyPrint VAR_DECL where
    printText0 ga (Var_decl l s _) = commaT ga l 
				<> colon 
				<> printText0 ga s 

instance PrettyPrint FORMULA where
    printText0 ga (Quantification q l f _) = printText0 ga q
			     <+> (semiT ga l
				  <> text cDot)
			     <+> printText0 ga f
    printText0 ga (Conjunction l _) = 
	parens(cat(punctuate (space 
			      <> text lAnd 
			      <> space) (map (printText0 ga) l)))
    printText0 ga (Disjunction  l _) = 
	parens(cat(punctuate (space 
			      <> text lOr 
			      <> space) (map (printText0 ga) l)))
    printText0 ga (Implication f g _) = parens(printText0 ga f
			     <+> text implS
			     <+> printText0 ga g)
    printText0 ga (Equivalence  f g _) = parens(printText0 ga f
			     <+> text equivS
			     <+> printText0 ga g)
    printText0 ga (Negation f _) = text negS <+> printText0 ga f
    printText0 ga (True_atom _) = text trueS
    printText0 ga (False_atom _) = text falseS
    printText0 ga (Predication p l _) = 
	printText0 ga p <> 
         (if null l then empty else parens(commaT ga l))
    printText0 ga (Definedness f _) = text defS <+> printText0 ga f
    printText0 ga (Existl_equation f g _) = printText0 ga f
			     <+> text exEqual
			     <+> printText0 ga g
    printText0 ga (Strong_equation f g _) = printText0 ga f
			     <+> text equalS
			     <+> printText0 ga g 
    printText0 ga (Membership f g _) = printText0 ga f
			     <+> text inS
			     <+> printText0 ga g
    printText0 ga (Mixfix_formula t) = printText0 ga t
    printText0 ga (Unparsed_formula s _) = text s 

instance PrettyPrint QUANTIFIER where
    printText0 ga (Universal) = text forallS
    printText0 ga (Existential) = text existsS
    printText0 ga (Unique_existential) = text (existsS ++ exMark)

instance PrettyPrint PRED_SYMB where
    printText0 ga (Pred_name n) = printText0 ga n
    printText0 ga (Qual_pred_name n t _) = parens (text predS
					      <+> printText0 ga n
					      <+> (colon
						   <> printText0 ga t))
				       
instance PrettyPrint TERM where
    printText0 ga (Simple_id i) = printText0 ga i
    printText0 ga (Qual_var n t _) = parens(text varS
					<+> (printText0 ga n
					     <> colon 
					     <> printText0 ga t))
    printText0 ga (Application o l _) = printText0 ga o <> 
					(if null l then empty 
					 else parens(commaT ga l))
    printText0 ga (Sorted_term t s _) = printText0 ga t
					<+> (colon
					     <> printText0 ga s)
    printText0 ga (Cast  t s _) = printText0 ga t
				  <+> text asS
				  <+> printText0 ga s
    printText0 ga(Conditional u f v _) = printText0 ga u
					 <+> text whenS
					 <+> printText0 ga f
					 <+> text elseS
					 <+> printText0 ga v
    printText0 _ (Unparsed_term s _) = text s
    printText0 ga (Mixfix_qual_pred p) = printText0 ga p
    printText0 ga (Mixfix_term l) = 
	cat(punctuate space (map (printText0 ga) l))
    printText0 ga (Mixfix_token t) = printText0 ga t
    printText0 ga (Mixfix_sorted_term s _) = colon
					     <> printText0 ga s
    printText0 ga (Mixfix_cast s _) = text asS
				     <+> printText0 ga s
    printText0 ga (Mixfix_parenthesized l _) = parens(commaT ga l)
    printText0 ga (Mixfix_bracketed l _) = brackets(commaT ga l)
    printText0 ga (Mixfix_braced l _) = braces(commaT ga l)

instance PrettyPrint OP_SYMB where
    printText0 ga (Op_name o) = printText0 ga o
    printText0 ga (Qual_op_name o t _) = parens(text opS
						<+> printText0 ga o
						<+> (colon
						     <> printText0 ga t))

