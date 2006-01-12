{- | 
Module      :  $Header$
Copyright   :  (c) Jorina F. Gerken, Till Mossakowski, Uni Bremen 2002-2006
License     :  similar to LGPL, see HetCATS/LICENSE.txt or LIZENZ.txt

Maintainer  :  jfgerken@tzi.de
Stability   :  provisional
Portability :  non-portable(Logic)

the proof status with manipulating functions
-}

module Proofs.StatusUtils where

import Static.DevGraph
import Data.Graph.Inductive.Graph
import Common.PrettyPrint
import qualified Common.Lib.Map as Map
import Syntax.AS_Library

{-
   proof status = (DG0,[(R1,DG1),...,(Rn,DGn)])
   DG0 is the development graph resulting from the static analysis
   Ri is a list of rules that transforms DGi-1 to DGi
   With the list of intermediate proof states, one can easily implement
    an undo operation
-}

type ProofHistory = [([DGRule], [DGChange])]
type ProofStatus = (LIB_NAME, LibEnv, Map.Map LIB_NAME ProofHistory)

emptyProofHistory :: ProofHistory
emptyProofHistory = [([], [])]

emptyProofStatus :: LIB_NAME -> LibEnv -> ProofStatus
emptyProofStatus ln le = (ln, le, Map.map (const emptyProofHistory) le)

-- -------------------------------
-- methods used in several proofs
-- -------------------------------
lookupDGraphError :: LIB_NAME -> a
lookupDGraphError libname = error ("Could not find lib with name <" 
                                   ++(show libname)++ "> in the given LibEnv")

mkResultProofStatus :: ProofStatus -> DGraph -> ([DGRule],[DGChange]) -> ProofStatus
mkResultProofStatus (libname,libEnv,proofHistory) dgraph (dgrules,dgchanges) =
  case Map.lookup libname libEnv of
    Nothing -> lookupDGraphError libname
    Just (globalContext,globalAnnos,_) ->
      (libname,
       Map.insert libname (globalContext,globalAnnos,dgraph) libEnv,
       Map.insert libname (historyElem:history) 
                  (prepareResultProofHistory proofHistory))
      
    where
      historyElem = (dgrules,removeContraryChanges dgchanges)
      history = case Map.lookup libname proofHistory of
                  Nothing -> []
                  Just h -> h

prepareResultProofHistory :: Map.Map LIB_NAME ProofHistory
                          -> Map.Map LIB_NAME ProofHistory
prepareResultProofHistory proofHistory = Map.map (([],[]):) proofHistory

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- prepare, revise, lookup, update on proofstatus and its components
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{- prepares the all histories of the proof history of the given proofstatus -}
prepareProofStatus :: ProofStatus -> ProofStatus
prepareProofStatus (ln,libEnv,history) = 
  (ln,libEnv,Map.map prepareHistory history)


{- prepares the given history for the rule application by appending
   an empty list tuple to the front of it, if there is not already one
   with an empty change list-}
prepareHistory :: [([DGRule],[DGChange])] -> [([DGRule],[DGChange])]
prepareHistory [] = [([],[])]
prepareHistory history@((_,[]):_) = history
prepareHistory history = ([],[]):history


{- revises the history of the given proofstatus -}
reviseProofStatus :: ProofStatus -> ProofStatus
reviseProofStatus (ln,libEnv,historyMap) =
  (ln, libEnv, Map.map reviseHistory historyMap)


{- removes the contrary changes form the given history and adds the name
   of the proof method (TheoremHideShift) -}
reviseHistory :: ProofHistory -> ProofHistory
reviseHistory [] = []
reviseHistory ((_,changes):history) =
  ([TheoremHideShift],(removeContraryChanges changes)):history


{- returns the global context that belongs to the given library name-}
lookupGlobalContext :: LIB_NAME -> ProofStatus -> GlobalContext
lookupGlobalContext ln (_,libEnv,_) =
  case Map.lookup ln libEnv of
    Nothing -> lookupDGraphError ln
    Just globalContext -> globalContext

{- returns the development graph that belongs to the given library name-}
lookupDGraph :: LIB_NAME -> ProofStatus -> DGraph
lookupDGraph ln proofstatus = dgraph
  where
    (_,_,dgraph) = lookupGlobalContext ln proofstatus

{- returns the history that belongs to the given library name-}
lookupHistory :: LIB_NAME -> ProofStatus -> ProofHistory
lookupHistory ln (_,_,historyMap) =
  case Map.lookup ln historyMap of
    Nothing -> []
    Just history -> history

{- updates the history belonging to the given library name,
   inserting the given changes-}
updateHistory :: LIB_NAME -> [DGChange] -> ProofStatus -> ProofStatus
updateHistory ln changes proofstatus@(l,libEnv,historyMap) =
  (l, libEnv,
  Map.insert ln (addChanges changes (lookupHistory ln proofstatus)) historyMap)

{- replaces the development graph belonging to the given library name
   with the given graph-}
updateLibEnv :: LIB_NAME -> DGraph -> ProofStatus -> ProofStatus
updateLibEnv ln dgraph proofstatus@(l,libEnv,historyMap) =
  (l,
   Map.insert ln 
   (updateDGraphInGlobalContext dgraph (lookupGlobalContext ln proofstatus))
   libEnv,
   historyMap)

{- updates the library environment and the proof history of the given
   proofstatus for the given library name-}
updateProofStatus :: LIB_NAME -> DGraph -> [DGChange] -> ProofStatus 
                  -> ProofStatus
updateProofStatus ln dgraph changes proofstatus =
  updateHistory ln changes proofstatusAux
  where
    proofstatusAux = updateLibEnv ln dgraph proofstatus

{- replaces the development graph of the given global context with
   the given graph-}
updateDGraphInGlobalContext :: DGraph -> GlobalContext -> GlobalContext
updateDGraphInGlobalContext dgraph (gAnnos,gEnv,_) = (gAnnos,gEnv,dgraph)


{- changes the library name of the given proofstatus to the given name -}
changeCurrentLibName :: LIB_NAME -> ProofStatus -> ProofStatus
changeCurrentLibName ln (_,libEnv,historyMap) = (ln,libEnv,historyMap)


{- adds the given changes to the given history -}
addChanges :: [DGChange] -> [([DGRule],[DGChange])] -> [([DGRule],[DGChange])]
addChanges changes [] = [([],changes)]
addChanges changes (hElem:history) = (fst hElem, (snd hElem)++changes):history

-- - - - - - - - - - - - - - - - - - - - - -
-- debug methods to print a list of changes
-- - - - - - - - - - - - - - - - - - - - - -

showChanges :: [DGChange] -> String
showChanges [] = ""
showChanges (change:changes) = 
  case change of
    InsertEdge edge -> "InsertEdge " ++ (showEdgeChange edge)
                       ++ (showChanges changes)
    DeleteEdge edge -> "DeleteEdge " ++ (showEdgeChange edge)
                       ++ (showChanges changes)
    InsertNode node -> "InsertNode " ++ (showNodeChange node)
                       ++ (showChanges changes)
    DeleteNode node -> "DeleteNode " ++ (showNodeChange node)
                       ++ (showChanges changes)

showEdgeChange :: LEdge DGLinkLab -> String
showEdgeChange (src,tgt,edgelab) =
  " from " ++ (show src) ++ " to " ++ (show tgt)
  ++ " and of type " ++ showPretty (dgl_type edgelab) "\n\n"

showNodeChange :: LNode DGNodeLab -> String
showNodeChange (descr, nodelab) =
  (show descr) ++ " with name " ++ (show (dgn_name nodelab)) ++ "\n\n"


-- ----------------------------------------------
-- methods that keep the change list clean
-- ----------------------------------------------

removeContraryChanges :: [DGChange] -> [DGChange]
removeContraryChanges [] = []
removeContraryChanges (change:changes) =
  case contraryChange of
    Just c -> removeContraryChanges (removeChange c changes)
    Nothing -> change:(removeContraryChanges changes)
  where
    contraryChange = 
      case getContraryChange change of
        Just c -> if c  `elem` changes then Just c else Nothing
        Nothing -> Nothing

getContraryChange :: DGChange -> Maybe DGChange
getContraryChange change =
  case change of
    InsertEdge edge -> Just $ DeleteEdge edge
    -- re-insertion of deleted edge may be useful if node has changed
    DeleteEdge edge -> Nothing 
    InsertNode node -> Just $ DeleteNode node
    -- re-insertion of deleted node may be useful if node has changed
    -- ... although this should be recognized ... a bit strange ...
    DeleteNode node -> Nothing -- Just $ InsertNode node


removeChange :: DGChange -> [DGChange] -> [DGChange]
removeChange _ [] = []
removeChange c1 (c2:rest) | c1==c2 = rest
-- when a node is removed afterwards, throw away all edge operations
-- refering to that node that are encountered on the way
removeChange c1@(DeleteNode (n,_)) (c2:rest) =
  if case c2 of
     InsertEdge (n1,n2,_) -> n==n1 || n==n2 
     DeleteEdge (n1,n2,_) -> n==n1 || n==n2 
     _ -> False
   then removeChange c1 rest
   else c2:removeChange c1 rest
removeChange c1 (c2:rest) = c2:removeChange c1 rest

