{- | 
   
   Module      :  $Header$
   Copyright   :  (c)  Till Mossakowski and Uni Bremen 2003
   Licence     :  similar to LGPL, see HetCATS/LICENCE.txt or LIZENZ.txt

   Maintainer  :  hets@tzi.de
   Stability   :  provisional
   Portability :  non-portable
   
   Assembles all the logics and comorphisms into a graph.
   The modules for the Grothendieck logic are logic graph indepdenent,
   and here is the logic graph that is used to instantiate these.
   Since the logic graph depends on a large number of modules for the
   individual logics, this separation of concerns (and possibility for
   separate compilation) is quite useful.

   References:

   The FLIRTS home page: <http://www.tzi.de/flirts>

   T. Mossakowski:
   Relating CASL with Other Specification Languages:
        the Institution Level
   Theoretical Computer Science 286, p. 367-475, 2002.

   Todo:
   Add many many logics and comorphisms.

-}

module Comorphisms.LogicGraph (defaultLogic, logicList, logicGraph, 
                               lookupLogic_in_LG, lookupComorphism_in_LG)
where

import Common.Result
import Logic.Logic 
import Logic.Comorphism
import Logic.Grothendieck
import CASL.Logic_CASL  -- also serves as default logic
import HasCASL.Logic_HasCASL
import Haskell.Logic_Haskell
import CspCASL.Logic_CspCASL
import Isabelle.Logic_Isabelle
import Comorphisms.CASL2PCFOL
import Comorphisms.CASL2HasCASL
import Comorphisms.HasCASL2Haskell
import Comorphisms.CASL2IsabelleHOL
import Comorphisms.CASL2Modal
import qualified Common.Lib.Map as Map
import CASL.ATC_CASL
import Modal.Logic_Modal

-- This needs to be seperated for utils/InlineAxioms/InlineAxioms.hs
import Comorphisms.LogicList

addComorphismName :: AnyComorphism -> (String,AnyComorphism)
addComorphismName c@(Comorphism cid) = (language_name cid, c)
addInclusionNames :: AnyComorphism -> ((String,String),AnyComorphism)
addInclusionNames c@(Comorphism cid) =
  ((language_name $ sourceLogic cid,
    language_name $ targetLogic cid),
   c)

inclusionList :: [AnyComorphism]
inclusionList = [Comorphism CASL2HasCASL, Comorphism HasCASL2Haskell,
                 Comorphism CASL2IsabelleHOL, Comorphism CASL2Modal]

comorphismList :: [AnyComorphism]
comorphismList = inclusionList ++ [Comorphism CASL2PCFOL]

logicGraph :: LogicGraph
logicGraph = 
  LogicGraph {
    logics =      Map.fromList $ map addLogicName logicList,
    comorphisms = Map.fromList $ map addComorphismName comorphismList,
    inclusions =  Map.fromList $ map addInclusionNames inclusionList
             }

lookupComorphism_in_LG :: String -> Result AnyComorphism
lookupComorphism_in_LG coname =
    lookupComorphism coname logicGraph
