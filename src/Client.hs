module Client where

import           Haste     (Event (..), onEvent)
import           Haste.App (Client, MonadIO, MonadIO, Remote, Server, addChild,
                            alert, forkServerIO, liftIO, mkConfig, newElem,
                            newTextElem, onServer, remote, runApp, runClient,
                            setClass, withElem, (<.>))
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

          {- onServer $ (apiAction api) <.> fileName -}

          file `onEvent` OnClick $ \_ _ -> do
            onServer $ apiOpenedFile api
            {- sendFilename fileName -}
            return ()

          {- f <- onServer $ apiOpenedFile api -}
          {- file `onEvent` OnClick $ \_ _ -> onServer $ do -}
            {- onServer $ apiOpenedFile api -}
            {- return () -}

          return ()


          where
            {- sendFilename :: MonadIO m => String -> m () -}
            {- sendFilename fileName = onServer $ (apiAction api) <.> fileName -}
            {- sendFilename fileName = do -}
              {- textEl <- newTextElem fileName -}
              {- return () -}

            {- toggleExpand file = do -}
              {- toggleClass file "expanded" -}

            appendTextElWithClasses :: String -> String -> [String] -> Elem -> Client ()
            appendTextElWithClasses tag text cssClasses parent = do
              el <- newElem tag

              textEl <- newTextElem text
              textEl `addChild` el

              mapM_ (\c -> setClass el c True) cssClasses
              el `addChild` parent
