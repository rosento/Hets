{- | 
Module      :  $Header$
Copyright   :  (c) Christian Maeder, Uni Bremen 2002-2005
License     :  similar to LGPL, see HetCATS/LICENSE.txt or LIZENZ.txt

Maintainer  :  maeder@tzi.de
Stability   :  provisional
Portability :  portable

a hand written 'ShATermConvertible' instance for 'BasicProof' 
-}

-- previously part of a header file that are deprecated now

module ATC.BasicProof where

import Static.DevGraph
import Common.ATerm.Lib
import Comorphisms.LogicGraph
import Logic.Logic
import ATC.Prover()

instance ShATermConvertible BasicProof where
     toShATerm att0 (BasicProof lid p) = 
	 case toShATerm att0 (language_name lid) of { (att1,i1) ->
         case toShATerm att1 p of { (att2,i2) ->
            addATerm (ShAAppl "BasicProof" [i1,i2] []) att2}}
     toShATerm att0 Guessed =
         case toShATerm att0 (show Guessed) of { (att1, i1) ->
            addATerm (ShAAppl "BasicProof" [i1] []) att1}
     toShATerm att0 Conjectured =
         case toShATerm att0 (show Conjectured) of { (att1, i1) ->
            addATerm (ShAAppl "BasicProof" [i1] []) att1}
     toShATerm att0 Handwritten =
         case toShATerm att0 (show Handwritten) of { (att1, i1) ->
            addATerm (ShAAppl "BasicProof" [i1] []) att1}

     fromShATerm att = 
         case getATerm att of
	    (ShAAppl "BasicProof" [i1,i2] _) ->
	       case fromShATerm (getATermByIndex1 i1 att) of { i1' ->
               case getATermByIndex1 i2 att of { att' ->
               case lookupLogic_in_LG 
                        ("ShATermConvertible BasicProof") i1'  of {
                    Logic lid -> (BasicProof lid (fromShATerm att'))}}}
            v@(ShAAppl "BasicProof" [i1] _) ->
               case fromShATerm (getATermByIndex1 i1 att) of { i1' ->
               case i1' of
                 "Guessed" -> Guessed
                 "Conjectured" -> Conjectured
                 "Handwritten" -> Handwritten
                 _ -> fromShATermError "BasicProof" v}
	    u     -> fromShATermError "BasicProof" u

instance ShATermConvertible G_theory where
     toShATerm att0 (G_theory lid sign sens) = 
	 case toShATerm att0 (language_name lid) of { (att1,i1) ->
         case toShATerm att1 sign of { (att2,i2) ->
         case toShATerm att2 sens of { (att3,i3) ->
           addATerm (ShAAppl "G_theory" [i1,i2,i3] []) att3}}}
     fromShATerm att = 
         case aterm of
	    (ShAAppl "G_theory" [i1,i2,i3] _) ->
		let i1' = fromShATerm (getATermByIndex1 i1 att)
                    att' = getATermByIndex1 i2 att
                    att'' = getATermByIndex1 i3 att'
                    l = lookupLogic_in_LG ("ShATermConvertible G_sign:") i1' 
                in case l of
                    Logic lid -> (G_theory lid (fromShATerm att') 
                                               (fromShATerm att''))
	    u     -> fromShATermError "G_theory" u
         where
         aterm = getATerm att
