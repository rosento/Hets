{-# LANGUAGE CPP #-}
{- |
Module      : $Header$
Description : The definition of CMDL interface for
              standard input and file input
Copyright   : uni-bremen and DFKI
License     : similar to LGPL, see HetCATS/LICENSE.txt or LIZENZ.txt
Maintainer  : r.pascanu@jacobs-university.de
Stability   : provisional
Portability : portable

CMDL.Interface describes the interface specific function
for standard input and file input
-}

module CMDL.Interface where

import System.Console.Shell(ShellDescription(defaultCompletions), runShell)
import System.Console.Shell.Backend(ShellBackend(..))
#ifdef EDITLINE
import System.Console.Shell.Backend.Editline
#else
import System.Console.Shell.Backend.Haskeline
#endif
import System.IO(IO, hIsTerminalDevice, stdin)

import CMDL.Commands(getCommands)
import CMDL.DataTypes(CmdlMessage(..), CmdlPrompterState(..), CmdlState(..))
import CMDL.DgCommands(cUse)
import CMDL.Shell(cmdlCompletionFn)
import CMDL.Utils(stripComments)

import CMDL.FileInterface(fileBackend, fileShellDescription)
import CMDL.StdInterface(stdShellDescription)
import CMDL.StringInterface(stringBackend, stringShellDescription)

import Interfaces.DataTypes

import Common.Utils(trim)
import Driver.Options (HetcatsOpts, InType(..), guess)

-- | Creates an empty CmdlState
emptyCmdlState :: HetcatsOpts -> CmdlState
emptyCmdlState opts = CmdlState
  { intState = IntState
      { i_state = Nothing
      , i_hist = IntHistory
          { undoList = []
          , redoList = [] }
      , filename = [] }
  , prompter = CmdlPrompterState
      { fileLoaded = ""
      , prompterHead = "> " }
  , output = CmdlMessage
      { errorMsg = []
      , outputMsg = []
      , warningMsg = [] }
  , openComment = False
  , connections = []
  , hetsOpts = opts }

-- | Processes a list of input files
processInput :: HetcatsOpts -> [FilePath] -> CmdlState -> IO CmdlState
processInput opts ls state
 = case ls of
    []   -> return state
    l:ll -> (case guess l GuessIn of
               ProofCommand -> cmdlProcessFileInState
               _            -> cUse) l state >>= processInput opts ll

-- | The function runs hets in a shell
cmdlRunShell :: HetcatsOpts -> [FilePath] -> IO CmdlState
cmdlRunShell opts files = do
  isTerm <- hIsTerminalDevice stdin
  state <- processInput opts files (emptyCmdlState opts)
  let backend =
#ifdef EDITLINE
                editlineBackend
#else
                haskelineBackend
#endif
      backendEcho = backend { getInput = \ h s ->
                             do
                               res <- (getInput backend h s)
                               case res of
                                 Just str -> putStrLn $ trim (stripComments str)
                                 Nothing -> return ()
                               return res
                         }
  runShell stdShellDescription
             { defaultCompletions = Just (cmdlCompletionFn getCommands) }
             (if isTerm then backend else backendEcho) state

-- | The function processes the file of instructions
cmdlProcessFile :: HetcatsOpts -> FilePath -> IO CmdlState
cmdlProcessFile opts file = cmdlProcessFileInState file $ emptyCmdlState opts

cmdlProcessFileInState :: FilePath -> CmdlState -> IO CmdlState
cmdlProcessFileInState = runShell fileShellDescription . fileBackend

-- | The function processes a string of instructions starting from a given
-- state
cmdlProcessString :: String -> CmdlState -> IO CmdlState
cmdlProcessString input st =
    runShell stringShellDescription (stringBackend input) st
    `catch` const (return st)
