{-# LANGUAGE MultiParamTypeClasses #-}
{- |
 TODO adjust comments
Module      :  ./UMLState/Logic_UMLState.hs
Description : Instance of class Logic for the UMLState logic
Copyright   :  (c) Tobias Rosenberger, Swansea University and Universit{'e} Grenoble Alpes 2020
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  tobias.rosenberger@univ-grenoble-alpes.fr
Stability   :  provisional
Portability :  non-portable (imports Logic.Logic)
-}

module UMLState.Logic_UMLState where

import Logic.Logic

data UMLState = UMLState deriving Show

instance Category () ()

instance Language UMLState where
instance Syntax UMLState () () () () where
instance Sentences UMLState () () () () where
instance StaticAnalysis UMLState () () () () () () () () where
instance Logic UMLState () () () () () () () () () () where
