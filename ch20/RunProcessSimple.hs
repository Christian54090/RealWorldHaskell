{-# OPTIONS_GHC -fglasgow-exts #-}

module RunProcessSimple where

import System.Process
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Exit
import Text.Regex
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types

{- | The type for running external commands. The first part of the tuple
is the program name. The list represents the command-line parameters
to pass the command. -}
type SysCommand = (String, [String])

{- | The result of running any command -}
data CommandResult = CommandResult {
    cmdOutput :: IO String            -- ^ IO action that yields the output
  , getExitStatus :: IO ProcessStatus -- ^ IO action that yields exit result
  }

{- | The type for handling global lists of FDs to always close in the clients -}
type CloseFDs = MVar [Fd]

{- | Class representing anything that is a runnable command -}
class CommandLike a where
  {- | Given the command and a String representing input, invokes the command.
       Returns a String representing the output of the command. -}
  invoke :: a -> CloseFDs -> String -> IO CommandResult

-- Support for running system commands
instance CommandLike SysCommand where
  invoke (cmd,args) closefds input = do
    -- create two pipes: one to handle stdin and the other to handle stdout.
    -- we do not redirect stderr in this program.
    (stdinread, stdinwrite) <- createPipe
    (stdoutread, stdoutwrite) <- createPipe

    -- we add the parent FDs to this list because we always need to close them
    -- in the clients
    addCloseFDs closefds [stdinwrite, stdoutread]

    -- now, grab the closed FDs list and fork the child.
    childPID <- withMVar closefds (\fds ->
                    forkprocess (child fds stdinread stdoutwrite))

    -- now, on the parent, close the client-side FDs
    closeFd stdinread
    closeFd stdoutwrite

    -- write the input to the command
    stdinhdl <- fdToHandle stdinwrite
    forkIO $ do hPutStr stdinhdl input
                hClose stdinhdl

    -- prepare to receive the output from the command
    stdouthdl <- fdToHandle stdoutread

    -- set up the function to call when ready to wail for the child to exit.
    let waitfunc = do
      status <- getProcessStatus True False childPID
      case status of
        Nothing -> fail $ "Error: Nothing from getProcessStatus"
        Just ps -> do removeCloseFDs closefds [stdinwrite, stdoutread]
                      return ps
    return $ CommandResult {cmdOutput = hGetContents stdouthdl,
                            getExitStatus = waitfunc}

    -- define what happens in the child process
    where child closefds stdinread stdoutwrite = do
      -- copy our pipes over the regular stdin/stdout FDs
      dupTo stdinread stdInput
      dupTo stdoutwrite stdOutput

      -- now close the original pipe FDs
      closeFd stdinread
      closeFd stdoutwrite

      -- close all the open FDs we inherited from the parent
      mapM_ (\fd -> catch (closeFd fd) (\_ -> return ())) closefds

      -- start the program
      executeFile cmd True args Nothing

-- adds FDs to the list of FDs that must be closed post-fork in a child
addCloseFDs