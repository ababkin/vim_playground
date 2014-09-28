{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (MVar (..), newEmptyMVar, putMVar,
                                          takeMVar)

import           Haste.App               (App, MonadIO, addChild, forkServerIO,
                                          liftIO, mkConfig, onServer, remote,
                                          runApp, runClient)
#ifndef __HASTE__

import           Control.Monad.Trans     (liftIO)
import           Network
import           Network.Socket.Internal (PortNumber (..))
import           Vim.Netbeans


#endif

import           Control.Monad           (join)
import           Haste                   (Event (..), onEvent)
import           Haste.App               (Client, MonadIO, Remote, Server,
                                          alert, newElem, newTextElem, setClass,
                                          withElem)
import           Haste.DOM               (Elem, setClass, toggleClass)
import           Haste.JSON
import           Haste.Prim
import           Haste.Serialize

data API = API {
    {- apiForkNb     :: Remote (Server ()) -}
    apiOpenedFile :: Remote (Server String)
  }

#ifdef __HASTE__

nb = undefined
openedFile = undefined

#else

nb :: MVar String -> IO ()
nb em = runNetbeans
            (PortNumber 4444)
            "password"
            (NetbeansCallbacks
                (liftIO $ putStrLn "Waiting for connection")
                (\e -> case e of
                    StartupDone -> liftIO $ putStrLn "Startup done"
                    _ -> liftIO $ putStrLn "other event"
                )
            ) $ do
    b <- editFile "ttt"
    pollAllEvents em

    where
      pollAllEvents :: MonadIO m => MVar String -> Netbeans m ()
      pollAllEvents em = do
          ne <- nextEvent
          case ne of
              (_, Disconnect) -> return ()
              (_, e) -> do
                  liftIO $ putStrLn $ "printing event " ++ (show e)
                  liftIO $ putMVar em $ show e
                  pollAllEvents em

openedFile :: MVar String -> Server String
openedFile = liftIO . takeMVar

#endif



main :: IO ()
main = do
  em <- newEmptyMVar

  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    forkServerIO $ liftIO $ nb em

    api <- API  <$> (remote $ openedFile em)
                {- <*> (remote $ openedFile em) -}

    runClient $ do
      withElem "files" $ render api

      where
        render :: API -> Elem -> Client ()
        render api filesContainer = do
          file <- onServer $ apiOpenedFile api
          addOpenFileEvent file filesContainer
          render api filesContainer

          where
            addOpenFileEvent :: MonadIO m => String -> Elem -> m ()
            addOpenFileEvent fileName filesContainer = do
              file <- newElem "div"

              appendTextElWithClasses "span" fileName ["file-name"] file

              setClass file "file" True
              file `addChild` filesContainer

              {- onEvent request OnClick $ \_ _ -> toggleRequestExpand request -}
              return ()


              where

                toggleExpand file = do
                  toggleClass file "expanded"

                appendTextElWithClasses :: MonadIO m => String -> String -> [String] -> Elem -> m ()
                appendTextElWithClasses tag text cssClasses parent = do
                  el <- newElem tag

                  textEl <- newTextElem text
                  textEl `addChild` el

                  mapM_ (\c -> setClass el c True) cssClasses
                  el `addChild` parent


