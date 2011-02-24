module FreeCAD.Parser 
    where

import System.IO
import Translator


--the IO part of the program:--
processFile = do
  xmlInput <-readFile "input.xml"
  let parsed = parseXMLDoc xmlInput
  let out = translate (fromJust parsed)
  putStrLn (show out)
------------------------

