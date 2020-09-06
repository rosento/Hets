{- |
 TODO adjust comments
Module      :  ./UMLState/UMLState2DG.hs
Description :  UMLState Development Graphs
Copyright   :  (c) Tobias Rosenberger, Swansea University and Universit{'e} Grenoble Alpes 2020
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  
Stability   :  experimental
Portability :  portable

Conversion of UMLState to Development Graphs.
-}

module UMLState.UMLState2DG (anaUMLStateFile) where

import Common.AS_Annotation
import Common.IRI (simpleIdToIRI)
import Common.Id
import Common.LibName
import Common.Result
import Common.Utils
import Common.AnnoState

import qualified Data.Map as Map
import Data.Functor.Identity

import Driver.Options

import Logic.ExtSign
import Logic.Grothendieck
import Logic.Logic
import Logic.Prover

import Static.ComputeTheory
import Static.DevGraph
import Static.DgUtils
import Static.GTheory

-- import Text.ParserCombinators.Parsec
import Text.Parsec

import UMLState.Logic_UMLState

parseFromFile :: ParsecT String (AnnoState ()) Identity a -> SourceName ->  IO (Either ParseError a)
parseFromFile parser file = do
  input <- readFile file
  let sourceName = file
      state0 = AnnoState [] ()
  return $ runIdentity $ runParserT parser state0 sourceName input


{- | generates the library and the development graph from the path of the
umls file -}
-- anaUMLStateFile :: HetcatsOpts -> FilePath -> IO (Maybe (LibName, LibEnv))
anaUMLStateFile opts file = do 
  putIfVerbose opts 2 $ "Reading file " ++ file
  parseResult <- parseFromFile (namedSpec undefined undefined) file
  case parseResult of
    Left msg -> putStrLn (show msg) >> return Nothing
    Right (name := spec) ->
      let libName = emptyLibName file -- TODO consider 'name'
          libEnv  = Map.singleton libName $ umlState2DG spec
      in return $ Just (libName, libEnv)

-- umlState2DG :: BASIC_SPEC -> DGraph
umlState2DG = error "only parsing implemented for UMLState" -- TODO
