module Types where

import           Haste.App (Client, Remote, Server)

data API = API {
    apiOpenedFile :: Remote (Server String)
  , apiAction     :: Remote (String -> Server ())
  }

data Action = OpenFile{
    filename :: String
  } deriving Show
