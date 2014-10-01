{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

#ifndef __HASTE__
import           Control.Monad.Trans     (liftIO)

import           NetBeans                (netBeans)
#endif

import           Control.Applicative     ((<$>), (<*>))
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (MVar (..), newEmptyMVar, putMVar,
                                          takeMVar)
import           Haste.App               (MonadIO, Server, forkServerIO, liftIO,
                                          mkConfig, newElem, remote, runApp,
                                          runClient)


import           Client                  (client)
import           Types                   (API (..), Action (..))


#ifdef __HASTE__

nb = undefined
openedFile = undefined

#else

nb :: MVar String -> MVar Action -> IO ()
nb = netBeans

openedFile :: MVar String -> Server String
openedFile = liftIO . takeMVar

invokeAction :: MVar Action -> String -> Server ()
invokeAction mactions fileName = liftIO $ putMVar mactions (OpenFile fileName)


#endif



main :: IO ()
main = do
  events <- newEmptyMVar
  actions <- newEmptyMVar

  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    forkServerIO $ liftIO $ nb events actions

    api <- API  <$> remote (openedFile events)
                <*> remote (invokeAction actions)

    runClient $ client api


