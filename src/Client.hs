module Client where

import           Haste     (Event (..))
import           Haste.App (Client, MonadIO, MonadIO, Remote, Server, addChild,
                            alert, forkServerIO, liftIO, mkConfig, newElem,
                            newTextElem, onServer, remote, runApp, runClient,
                            setClass, withElem, (<.>), onEvent)
import           Haste.DOM (Elem, setClass, toggleClass)
{- import           Haste.JSON -}
{- import           Haste.Prim -}
{- import           Haste.Serialize -}

import           Types     (API (..), Action (..))

client api = do
  withElem "files" $ render api

  where
    render :: API -> Elem -> Client ()
    render api filesContainer = do
      file <- onServer $ apiOpenedFile api
      addOpenFileEvent file filesContainer
      render api filesContainer

      where
        addOpenFileEvent :: String -> Elem -> Client ()
        addOpenFileEvent fileName filesContainer = do
          file <- newElem "div"

          appendTextElWithClasses "span" fileName ["file-name"] file

          setClass file "file" True
          file `addChild` filesContainer

          file `onEvent` OnClick $ \_ _ -> onServer $ (apiAction api) <.> fileName


          where
            appendTextElWithClasses :: String -> String -> [String] -> Elem -> Client ()
            appendTextElWithClasses tag text cssClasses parent = do
              el <- newElem tag

              textEl <- newTextElem text
              textEl `addChild` el

              mapM_ (\c -> setClass el c True) cssClasses
              el `addChild` parent
