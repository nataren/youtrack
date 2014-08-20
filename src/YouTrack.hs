{-# LANGUAGE OverloadedStrings #-}

module YouTrack where
import Data.Text
import Network.Wreq
import Control.Lens

type Hostname = Text
type Username = Text
type Password = Text

mkAuth :: Hostname -> Username -> Password -> IO()
mkAuth = undefined
