{- |
Module      :  $Header$
Copyright   :  (c) Klaus L�ttich, Christian Maeder and Uni Bremen 2002-2003 
Licence     :  similar to LGPL, see HetCATS/LICENCE.txt or LIZENZ.txt

Maintainer  :  hets@tzi.de
Stability   :  experimental
Portability :  portable 

make mixfix analysis checkable by RunParsers

-}

module CASL.RunMixfixParser where

import Common.AnnoState
import CASL.MixfixParser
import CASL.AS_Basic_CASL
import Common.GlobalAnnotations
import Common.Lib.Set
import Common.Id
import Common.Result
import Common.Lexer
import Common.PPUtils

import Common.Token
import CASL.Formula
import CASL.ShowMixfix -- (showTerm, showFormula)

-- start testing
stdOpsL, stdPredsL :: [String]

stdOpsL = ["__^__", "__*__", "__+__", "[__]","__div__","__mod__", "__rem__", 
        "__-__", "+__", "__E__", "__@@__", "[]", "__::__", "__:::__",
	"-__", "__!"] ++ 
          [ "____p", "q____","____x____", "{____}",
          "repeat__until__", "while__do__od", 
	    "__where__but__", "__where__done",
           "__ --> __", "__{__}--__-->{__}__", 
           "Pl7","folge_dem_Gang","nicht_wenden","Pl3","RS3", "RS6"] ++
        map (:[]) 
        "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
         ++ ["A[a[c,d],b]", "B[a[c,d],b]", "__B[a[c,d],b]__", 
	     "a[c,d]", "__a[c,d]__", "A[a]", "A__B", 
	     "A__", "__[a]", "__p", 
	     "__[__]__", "[__]__", "__[__]"] 

stdPredsL = ["__<__", "__<=__", "__>__", "__>=__", "__!=__", "__<>__",
	     "__/=__", "even__", "odd__", "__isEmpty",
	    "__<=__<=__"] ++ map (:[]) "abcdpqrstuvwxyzPQRSTUVWXYZ" 

mkIds :: [String] -> Set Id
mkIds = fromList . map (parseString $ parseId [])

stdOps, stdPreds :: Set Id
stdOps = mkIds stdOpsL
stdPreds = mkIds stdPredsL 

resolveForm :: GlobalAnnos -> AParser (Result (FORMULA ()))
resolveForm ga = 
      resolveFormula ga stdOps stdPreds `fmap` formula []

resolveTerm :: GlobalAnnos -> AParser (Result (TERM ()))
resolveTerm ga = 
      resolveMixfix ga stdOps stdPreds `fmap` term []

testTerm ::  AParser WrapString
testTerm = do t <- term [] :: AParser (TERM ())
	      return $ WrapString $ showTerm t ""

testTermMix :: GlobalAnnos -> AParser WrapString
testTermMix ga = do Result ds mt <- resolveTerm ga
		    return $ WrapString $ 
			case mt of Just t -> showTerm t ""
				   _ -> show ds

testFormula :: AParser WrapString
testFormula = do f <- formula [] :: AParser (FORMULA ())
		 return $ WrapString $ showFormula f ""

testFormulaMix :: GlobalAnnos -> AParser WrapString
testFormulaMix ga = do Result ds m <- resolveForm ga
		       return $ WrapString $ 
			   case m of Just f -> showFormula f ""
				     _ -> show ds
