module NetBeans where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.MVar      (MVar, putMVar, takeMVar)
import           Control.Concurrent.STM.TChan (TChan, dupTChan, isEmptyTChan,
                                               newTChan, readTChan,
                                               tryReadTChan, writeTChan)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Reader         (ask, runReaderT)
import           Control.Monad.Trans          (liftIO)
import           Network                      (PortID (PortNumber))
import           Vim.Netbeans

import qualified Vim.Netbeans.Protocol        as P

import           Types                        (Action (..))



netBeans :: MVar String -> MVar Action -> IO ()
netBeans incoming outgoing = runNetbeans
            (PortNumber 4444)
            "password"
            (NetbeansCallbacks
                (liftIO $ putStrLn "Waiting for connection")
                (\e -> case e of
                    StartupDone -> liftIO $ putStrLn "Startup done"
                    _ -> liftIO $ putStrLn "other event"
                )
            )
            (handleOutgoing outgoing)
            (handleIncoming incoming)


            {- $ do -}
                  {- [> b <- editFile "ttt" <] -}

                  {- conf <- ask -}
                  {- [> q <- messageQueue `liftM` ask <] -}
                  {- liftIO $ forkIO $ runNetbeans (handleIncoming incoming) conf -}

                  {- sendOutgoing -}



    where
      handleIncoming :: MonadIO m => MVar String -> Netbeans m ()
      handleIncoming incoming = do
        ne <- nextEvent
        case ne of
            (_, Disconnect) -> return ()
            (_, e) -> do
                liftIO $ putStrLn $ "printing event " ++ (show e)
                liftIO $ putMVar incoming $ show e
                handleIncoming incoming

      handleOutgoing :: MVar Action -> Netbeans IO ()
      handleOutgoing outgoing = do
        action <- liftIO $ takeMVar outgoing
        case action of
          OpenFile filename -> do
            liftIO $ putStrLn $ "opening file: " ++ filename
            editFile filename
            handleOutgoing outgoing
          _ -> do
            liftIO $ putStrLn $ "Unknown action: " ++ (show action)
            handleOutgoing outgoing


