-- (c) 2000 - 2002 by Martin Erwig [see file COPYRIGHT]
-- | Minimum-Spanning-Tree Algorithms 

module Data.Graph.Inductive.Query.MST (
    msTreeAt,msTree,
    -- * Path in MST
    msPath
) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Aux.RootPath
import qualified Data.Graph.Inductive.Aux.Heap as H


newEdges :: Ord b => LPath b -> Context a b -> [H.Heap b (LPath b)]
newEdges p (_,_,_,s) = map (\(l,v)->H.unit l ((v,l):p)) s

prim :: (Graph gr,Real b) => H.Heap b (LPath b) -> gr a b -> LRTree b
prim h g | H.isEmpty h || isEmpty g = []
prim h g =
    case match v g of
         (Just c,g')  -> p:prim (H.mergeAll (h':newEdges p c)) g'
         (Nothing,g') -> prim h' g'  
    where (_,p@((v,_):_),h') = H.splitMin h

msTreeAt :: (Graph gr,Real b) => Node -> gr a b -> LRTree b
msTreeAt v g = prim (H.unit 0 [(v,0)]) g

msTree :: (Graph gr,Real b) => gr a b -> LRTree b
msTree g = msTreeAt v g where ((_,v,_,_),_) = matchAny g

msPath :: Real b => LRTree b -> Node -> Node -> Path
msPath t a b = joinPaths (map fst (getLPath a t)) (map fst (getLPath b t))
            
joinPaths :: Path -> Path -> Path 
joinPaths p q = joinAt (head p) p q

joinAt :: Node -> Path -> Path -> Path
joinAt _ (v:vs) (w:ws) | v==w = joinAt v vs ws
joinAt x p      q             = reverse p++(x:q)

