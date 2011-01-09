module System.Process.Safe where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception   as C
import           Control.Monad
import           Control.Monad.Error
import           Data.Maybe
import           GHC.IO.Exception
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Process

-- -----------------------------------------------------------------------------
--
-- | readProcess forks an external process, reads its standard output
-- strictly, blocking until the process terminates, and returns the output
-- string.
--
-- Output is returned strictly, so this is not suitable for
-- interactive applications.
--
-- Users of this function should compile with @-threaded@ if they
-- want other Haskell threads to keep running while waiting on
-- the result of readProcess.
--
-- >  > readProcess "date" [] []
-- >  "Thu Feb  7 10:03:39 PST 2008\n"
--
-- The arguments are:
--
-- * The command to run, which must be in the $PATH, or an absolute path
--
-- * A list of separate command line arguments to the program
--
-- * A string to pass on the standard input to the program.
--
readProcessWithTimeout 
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> Int                      -- ^ timeout in milliseconds
    -> IO String                -- ^ stdout
readProcessWithTimeout cmd args input microseconds = do
    (Just inh, Just outh, _, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = Inherit }

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ C.evaluate (length output) >> putMVar outMVar (Just ())

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- kill on a timeout
    forkIO $ do threadDelay $ microseconds * 1000
                got <- isJust <$> tryTakeMVar outMVar
                unless got $ do
                  terminateProcess pid
                  putMVar outMVar Nothing

    -- wait on the output
    success <- isJust <$> takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    case if success then ex else ExitFailure (-1) of
     ExitSuccess   -> return output
     ExitFailure r -> 
      ioError (mkIOError GHC.IO.Exception.OtherError
                         ("readProcess: " ++ cmd ++ 
                                     ' ':unwords (map show args) ++ 
                                     " (exit " ++ show r ++ ")")
                                 Nothing Nothing)
