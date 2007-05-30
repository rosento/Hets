{- |
Module      :  $Header$
Description :  hide theorem shift proof rule for development graphs
Copyright   :  (c) Jorina F. Gerken, Till Mossakowski, Uni Bremen 2002-2006
License     :  similar to LGPL, see HetCATS/LICENSE.txt or LIZENZ.txt

Maintainer  :  ken@tzi.de
Stability   :  provisional
Portability :  non-portable(Logic)

hide theorem shift proof rule for development graphs
   Follows Sect. IV:4.4 of the CASL Reference Manual.
-}

{-
   References:

   T. Mossakowski, S. Autexier and D. Hutter:
   Extending Development Graphs With Hiding.
   H. Hussmann (ed.): Fundamental Approaches to Software Engineering 2001,
   Lecture Notes in Computer Science 2029, p. 269-283,
   Springer-Verlag 2001.
-}

module Proofs.HideTheoremShift
    ( interactiveHideTheoremShift
    , automaticHideTheoremShift
    , automaticHideTheoremShiftFromList
    ) where

import Logic.Grothendieck
import Syntax.AS_Library
import Static.DevGraph
import Static.DGToSpec
import Common.Result
import Data.Graph.Inductive.Graph
import Proofs.EdgeUtils
import Proofs.StatusUtils
import GUI.Utils

import Control.Monad.Identity
--import Debug.Trace

-- ----------------------------------------------
-- hide theorem shift
-- ----------------------------------------------

type ListSelector m a = [a] -> m (Maybe a)
type PathTuple = ([LEdge DGLinkLab], [LEdge DGLinkLab])
type ProofBaseSelector m = DGraph -> ListSelector m PathTuple

interactiveHideTheoremShift :: LIB_NAME -> LibEnv -> IO LibEnv
interactiveHideTheoremShift =
    hideTheoremShift hideTheoremShift_selectProofBase

automaticHideTheoremShift :: LIB_NAME -> LibEnv -> LibEnv
automaticHideTheoremShift ln libEnv =
    let dgraph = lookupDGraph ln libEnv
        ls = filter (liftE isUnprovenHidingThm) $ labEdgesDG dgraph
    in automaticHideTheoremShiftFromList ln ls libEnv



automaticHideTheoremShiftFromList :: LIB_NAME -> [LEdge DGLinkLab]-> LibEnv -> LibEnv
automaticHideTheoremShiftFromList ln ls = runIdentity. hideTheoremShiftFromList
      (const $ \ l -> return $ case l of
                      [a] -> Just a -- maybe take the first one always ?
                      _   -> Nothing) ln ls

hideTheoremShiftFromList :: Monad m => ProofBaseSelector m -> LIB_NAME
                     -> [LEdge DGLinkLab] -> LibEnv  -> m LibEnv
hideTheoremShiftFromList proofBaseSel ln hidingThmEdges proofStatus = do
    let dgraph = lookupDGraph ln proofStatus
        finalHidingThmEdges = filter (liftE isUnprovenHidingThm) hidingThmEdges
    result <- hideTheoremShiftAux dgraph ([],[])
              finalHidingThmEdges proofBaseSel
    let (nextDGraph, nextHistoryElem) = result
        newProofStatus
               = mkResultProofStatus ln proofStatus nextDGraph nextHistoryElem
    return newProofStatus


hideTheoremShift :: Monad m => ProofBaseSelector m -> LIB_NAME
                 -> LibEnv -> m LibEnv
hideTheoremShift proofBaseSel ln proofStatus =
    let dgraph = lookupDGraph ln proofStatus
        hidingThmEdges = filter (liftE isUnprovenHidingThm) $ labEdgesDG dgraph
    in hideTheoremShiftFromList proofBaseSel ln hidingThmEdges proofStatus

--hideTheoremShift :: Monad m => ProofBaseSelector m -> LIB_NAME
--                 -> LibEnv -> m LibEnv
--hideTheoremShift proofBaseSel ln proofStatus = do
--  let dgraph = lookupDGraph ln proofStatus
--      hidingThmEdges = filter isUnprovenHidingThm (labEdges dgraph)
--  result <- hideTheoremShiftAux dgraph ([],[]) hidingThmEdges proofBaseSel
--  let nextDGraph = fst result
--      nextHistoryElem = snd result
--      newProofStatus
--          = mkResultProofStatus ln proofStatus nextDGraph nextHistoryElem
--  return newProofStatus

{- auxiliary method for hideTheoremShift -}
hideTheoremShiftAux :: Monad m => DGraph -> ([DGRule],[DGChange])
                    -> [LEdge DGLinkLab] ->  ProofBaseSelector m
                    -> m (DGraph,([DGRule],[DGChange]))
hideTheoremShiftAux dgraph historyElement [] _ =
  return (dgraph, historyElement)
hideTheoremShiftAux dgraph (rules,changes) (ledge:list) proofBaseSel =
  do proofBasis <- findProofBaseForHideTheoremShift dgraph ledge proofBaseSel
     if (null proofBasis)
      then hideTheoremShiftAux dgraph (rules,changes) list proofBaseSel
       else do
         let ((auxDGraph,auxChanges), finalProofBasis) =
		   insertNewEdges (dgraph, changes) proofBasis []
	     newEdge = makeProvenHidingThmEdge finalProofBasis ledge
	     (newDGraph2, newChanges2) = updateWithOneChange (DeleteEdge ledge) 
							  auxDGraph 
							  auxChanges
	     (newDGraph, newChanges) = 
	           insertDGLEdge newEdge newDGraph2 newChanges2
             -- auxDGraph = insEdge newEdge (deLLEdge ledge dgraph)
             -- auxChanges = (DeleteEdge ledge):((InsertEdge newEdge):changes)
             newRules = (HideTheoremShift ledge):rules
         hideTheoremShiftAux newDGraph (newRules,newChanges) list proofBaseSel

{- inserts the given edges into the development graph and adds a corresponding
   entry to the changes, while getting the proofbasis -}
insertNewEdges :: (DGraph, [DGChange]) -> [LEdge DGLinkLab] ->
		  [EdgeID] -> ((DGraph,[DGChange]), [EdgeID]) 
		  --[LEdge DGLinkLab] -> ((DGraph,[DGChange]), [LEdge DGLinkLab])
insertNewEdges res [] proofbasis = (res, proofbasis)
insertNewEdges (dgraph, changes) (edge:list) proofbasis =
  case (tryToGetEdge edge dgraph changes) of
       Just e -> insertNewEdges (dgraph, changes) list (getEdgeID e:proofbasis)
       Nothing -> let
		  (tempDGraph, tempChanges) =
		       (updateWithOneChange (InsertEdge edge) dgraph changes)
		  tempProofBasis = case (head tempChanges) of
				     (InsertEdge tempE) -> (getEdgeID tempE:proofbasis)
				     _ -> error ("Proofs"++
						".HideTheoremShift"++
						".insertNewEdges")
		  in
		  insertNewEdges (tempDGraph, tempChanges)
				 list
				 tempProofBasis
{-
  if isDuplicate edge dgraph changes then insertNewEdges (dgraph, changes) list
   else insertNewEdges (updateWithOneChange (InsertEdge edge) dgraph changes) 
		       list
-}
	--insertNewEdges (insEdge edge dgraph) ((InsertEdge edge):changes) list


{- creates a new proven HidingThm edge from the given HidingThm edge using the edge list as the proofBasis -}
--makeProvenHidingThmEdge :: [LEdge DGLinkLab] -> LEdge DGLinkLab -> LEdge DGLinkLab
makeProvenHidingThmEdge :: [EdgeID] -> LEdge DGLinkLab -> LEdge DGLinkLab
makeProvenHidingThmEdge proofBasisEdges ledge@(src,tgt,edgeLab) =
  (src,
   tgt,
   DGLink {dgl_morphism = morphism,
           dgl_type = (HidingThm hidingMorphism
                       (Proven (HideTheoremShift ledge) proofBasisEdges)),
           dgl_origin = DGProof,
	   dgl_id = dgl_id edgeLab}
  )
  where
    morphism = dgl_morphism edgeLab
    (HidingThm hidingMorphism _) = (dgl_type edgeLab)


{- selects a proof basis for 'hide theorem shift' if there is one-}
findProofBaseForHideTheoremShift
    :: Monad m => DGraph -> LEdge DGLinkLab
    -> ProofBaseSelector m -> m [LEdge DGLinkLab]
findProofBaseForHideTheoremShift dgraph (ledge@(src,tgt,edgelab))
                                  proofBaseSel =
     do pb <- proofBaseSel dgraph pathPairsFilteredByProveStatus
        case pb of
          Nothing -> return []
          Just proofBasis -> do let fstPath = fst proofBasis
				    sndPath = snd proofBasis
                                return [createEdgeForPath fstPath,
                                        createEdgeForPath sndPath]

   where
    pathsFromSrc = getAllPathsOfTypeFrom dgraph src (ledge /=)
    pathsFromTgt = getAllPathsOfTypeFrom dgraph tgt (ledge /=)
    possiblePathPairs = selectPathPairs pathsFromSrc pathsFromTgt
    (HidingThm hidingMorphism _) = (dgl_type edgelab)
    morphism = dgl_morphism edgelab
    pathPairsFilteredByMorphism
        = filterPairsByResultingMorphisms possiblePathPairs
          hidingMorphism morphism
    pathPairsFilteredByConservativity
        = filterPairsByConservativityOfSecondPath pathPairsFilteredByMorphism
    -- advoiding duplicate to be selected proofbasis. 
    pathPairsFilteredByProveStatus
        = filterPairsByGlobalProveStatus pathPairsFilteredByConservativity

-- advoiding duplicate to be selected proofbasis.
filterPairsByGlobalProveStatus :: [([LEdge DGLinkLab], [LEdge DGLinkLab])] -> [([LEdge DGLinkLab], [LEdge DGLinkLab])]
filterPairsByGlobalProveStatus = filter bothAreProven
              where
	      bothAreProven (pb1,pb2) = (allAreProven pb1) && (allAreProven pb2)
	      allAreProven = all $ liftE (\l -> (isProven l) && (isGlobalEdge l))

--filterPairsByProveStatus [] = = []
--filterPairsByProveStatus (pb@(pb1, pb2):xs) = if((hasUnproven pb1)||(hasUnproven pb2)) then filterPairsByProveStatus xs
	--									    else pb:filterPairsByProveStatus xs 

{- removes all pairs from the given list whose second path does not have a
   conservativity greater than or equal to Cons -}
filterPairsByConservativityOfSecondPath ::
    [([LEdge DGLinkLab],[LEdge DGLinkLab])]
      -> [([LEdge DGLinkLab],[LEdge DGLinkLab])]
filterPairsByConservativityOfSecondPath [] = []
filterPairsByConservativityOfSecondPath (([],_):list) =
  filterPairsByConservativityOfSecondPath list
filterPairsByConservativityOfSecondPath ((_,[]):list) =
  filterPairsByConservativityOfSecondPath list
filterPairsByConservativityOfSecondPath (pair:list) =
  if (and [cons >= Cons | cons <- map getConservativity (snd pair)])
   then pair:(filterPairsByConservativityOfSecondPath list)
    else filterPairsByConservativityOfSecondPath list


{- selects a proofBasis (i.e. a path tuple) from the list of possible ones:
   If there is exaclty one proofBasis in the list, this is returned.
   If there are more than one and the method is called in automatic mode Nothing is returned. In non-automatic mode the user is asked to select a proofBasis via listBox. -}
hideTheoremShift_selectProofBase ::
    DGraph -> [([LEdge DGLinkLab], [LEdge DGLinkLab])]
                 -> IO (Maybe ([LEdge DGLinkLab], [LEdge DGLinkLab]))
hideTheoremShift_selectProofBase dgraph basisList =
  case basisList of
    [] -> return Nothing
    [basis] -> return $ Just basis
    _ -> do
         sel <- listBox
                       "Choose a path tuple as the proof basis"
                       (map (prettyPrintPathTuple dgraph) basisList)
         case sel of
             Just j -> return $ Just (basisList !! j)
             _ -> return Nothing -- error ("Proofs.Proofs: " ++
                               -- "selection of proof basis failed")
                  -- failing or outputting something here may be a bad idea

{-

-- ken's debugging function

debug_show :: [([LEdge DGLinkLab], [LEdge DGLinkLab])] -> String
--debug_show ((a, b):((c, d):_)) = show (debug_show_edge b==debug_show_edge d)
debug_show [] = ""
debug_show ((a, b):xs) = "("++(debug_show_edge a)++", "++(debug_show_edge b)++") "++(debug_show xs) 

debug_show_edge :: [LEdge DGLinkLab] -> String
debug_show_edge [] = ""
debug_show_edge ((src, tgt, lab):xs) = "("++(show src)++"->"++(show tgt)++" with prove status: "++(debug_show_pro_sta lab)++") -> " ++debug_show_edge xs 

debug_show_pro_sta :: DGLinkLab -> String
debug_show_pro_sta lab = 
    case (dgl_type lab) of 
       (GlobalThm (Proven _ _) _ _) -> "global proven"
       (LocalThm (Proven _ _) _ _) -> "local proven"
       (HidingThm _ (Proven _ _)) -> "hiding proven"
       (LocalThm LeftOpen _ _) -> "local unproven"
       _ -> "other unproven"
-}




{- returns a string representation of the given paths: for each path a
   tuple of the names of its nodes is shown, the two are combined by
   an \'and\' -}
prettyPrintPathTuple :: DGraph -> ([LEdge DGLinkLab],[LEdge DGLinkLab])
                     -> String
prettyPrintPathTuple dgraph (p1,p2) =
  (prettyPrintPath dgraph p1) ++ " and " ++ (prettyPrintPath dgraph p2)

{- returns the names of the nodes of the path, separated by commas-}
prettyPrintNodesOfPath :: DGraph -> [LEdge DGLinkLab] -> String
prettyPrintNodesOfPath _ [] = ""
prettyPrintNodesOfPath dgraph (edge:path) =
  (prettyPrintSourceNode dgraph edge) ++ ", "
  ++ (prettyPrintNodesOfPath dgraph path)
  ++ end
  where
    end = case path of
            [] -> prettyPrintTargetNode dgraph edge
            _ -> ""

{- returns a string reprentation of the path: showing a tuple of its nodes-}
prettyPrintPath :: DGraph -> [LEdge DGLinkLab] -> String
prettyPrintPath _ [] = "<empty path>"
prettyPrintPath dgraph path =
  "(" ++ (prettyPrintNodesOfPath dgraph path) ++ ")"


{- returns the name of the source node of the given edge-}
prettyPrintSourceNode :: DGraph -> LEdge DGLinkLab -> String
prettyPrintSourceNode dgraph (src,_,_) =
   getDGNodeName $ lab' $
      safeContextDG "Proofs.HideTheoremShift.prettyPrintSourceNode" dgraph src


{- returns the name of the target node of the given edge-}
prettyPrintTargetNode :: DGraph -> LEdge DGLinkLab -> String
prettyPrintTargetNode dgraph (_,tgt,_) =
   getDGNodeName $ lab' $
     safeContextDG "Proofs.HideTheoremShift.prettyPrintTargetNode"  dgraph tgt


{- creates a unproven global thm edge for the given path, i.e. with
the same source and target nodes and the same morphism as the path -}
createEdgeForPath :: [LEdge DGLinkLab] -> LEdge DGLinkLab
createEdgeForPath path =
  case (calculateMorphismOfPath path, path) of
    (Just morphism, (s, _, _) : _) -> 
        let (_, t, _) = last path 
        in (s, t,
                      DGLink {dgl_morphism = morphism,
                              dgl_type = (GlobalThm LeftOpen None
                                          LeftOpen),
                              dgl_origin = DGProof,
			      dgl_id = defaultEdgeID}
                     )
    _ -> error "createEdgeForPath"


{- returns a list of path pairs, as shorthand the pairs are not
returned as path-path tuples but as path-<list of path> tuples. Every
path in the list is a pair of the single path.  The path pairs are
selected by having the same target node. -}
selectPathPairs :: [[LEdge DGLinkLab]] -> [[LEdge DGLinkLab]]
                -> [([LEdge DGLinkLab],[[LEdge DGLinkLab]])]
selectPathPairs [] _ = []
selectPathPairs _ [] = []
selectPathPairs paths1 paths2 =
  [(p1,  [p2| p2 <- paths2, haveSameTgt (last p1) (last p2) ] )| p1 <- paths1]

  where
    haveSameTgt :: LEdge DGLinkLab -> LEdge DGLinkLab -> Bool
    haveSameTgt (_,tgt1,_) (_,tgt2,_) = tgt1 == tgt2

{- returns a list of path pairs by keeping those pairs, whose first path composed with the first given morphism and whose second path composed with the second given morphism result in the same morphism, and dropping all other pairs.-}
filterPairsByResultingMorphisms :: [([LEdge DGLinkLab],[[LEdge DGLinkLab]])]
                                -> GMorphism -> GMorphism
                                -> [([LEdge DGLinkLab],[LEdge DGLinkLab])]
filterPairsByResultingMorphisms [] _ _ = []
filterPairsByResultingMorphisms (pair:pairs) morph1 morph2 =
  [((fst pair),path)| path <- suitingPaths]
          ++ (filterPairsByResultingMorphisms pairs morph1 morph2)

  where
    compMorph1
        = compMaybeMorphisms (Just morph1) (calculateMorphismOfPath (fst pair))
    suitingPaths :: [[LEdge DGLinkLab]]
    suitingPaths = if (compMorph1 /= Nothing) then
                      [path |path <- (snd pair),
                       (compMaybeMorphisms (Just morph2)
                                           (calculateMorphismOfPath path))
                       == compMorph1]
                    else []

{- if the given maybe morphisms are both not Nothing,
   this method composes their GMorphisms -
   returns Nothing otherwise -}
compMaybeMorphisms :: Maybe GMorphism -> Maybe GMorphism -> Maybe GMorphism
compMaybeMorphisms morph1 morph2 =
  case (morph1, morph2) of
    (Just m1, Just m2) -> resultToMaybe $ compHomInclusion m1 m2
       -- This should be compInclusion, but this would need the logic graph
    _ -> Nothing

{- returns the Conservativity if the given edge has one,
   otherwise an error is raised -}
getConservativity :: LEdge DGLinkLab -> Conservativity
getConservativity (_,_,edgeLab) =
  case dgl_type edgeLab of
    (LocalThm _ cons _) -> cons
    (GlobalThm _ cons _) -> cons
    _ -> None
