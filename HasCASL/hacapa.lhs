#!/home/maeder/bin/runhugs

HetCATS/HasCASL/hacapa.lhs
$Id$
Authors: Christian Maeder
Year:    2002
   
test some parsers (and printers)

\begin{code}
module Main where

import HasCASL.ParseItem
import HasCASL.ParseTerm
import HasCASL.PrintAs
import HasCASL.PrintLe
import HasCASL.HToken
import CASL.RunParsers
import HasCASL.RunStaticAna
import Common.Lib.Parsec
import Common.AnnoState
import Common.PrettyPrint
import Common.GlobalAnnotations

main :: IO ()
main = exec lineParser fileParser

parseA :: AParser a -> String -> a
parseA p s = case runParser p emptyAnnos "" s of 
	     Right a -> a
	     Left err -> error $ show err

lineParser, fileParser :: [(String, HetParser)]
lineParser = [
 ("MixIds", HetParser uninstOpId),
 ("Typenames", HetParser typeId),
 ("Kinds", HetParser kind),
 ("Types", HetParser parseType),
 ("Terms", HetParser term),
 ("Typepattern", HetParser typePattern),
 ("Pattern", HetParser pattern),
 ("BasicItems", HetParser basicItems),
 ("Items", HetParser basicSpec)]

fileParser = [ ("BasicSpec", HetParser basicSpec)
	     , ("analysis", HetParser anaParser)
	     ]
\end{code}
