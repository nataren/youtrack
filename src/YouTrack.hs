{-# LANGUAGE OverloadedStrings #-}

-- | Driver for the YouTrack REST API
-- http://confluence.jetbrains.com/display/YTD5/Log+in+to+YouTrack
module YouTrack where
import Data.Map as Map
import Data.Aeson (Value(Null))
import qualified Data.Text as T
import Network.Wreq
import Control.Lens
import OpenSSL.Session (context)
import Network.HTTP.Client.OpenSSL
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

type Hostname = String
type Username = String
type Password = String
type Issue = String

userLoginPath :: String
userLoginPath = "/rest/user/login"

issuePath :: String
issuePath = "/rest/issue/"

data YouTrackAuth = YouTrackAuth {
  hostname :: String,
  sessionId :: String,
  principal :: String
} deriving Show

type Resp = Response LBS.ByteString
type RespMap = Response (Map String Value)

mkAuth :: Hostname -> Username -> Password -> IO YouTrackAuth
mkAuth hostname username password = do
  res <- withManager $ \_ -> postWith opts (hostname ++ userLoginPath) Data.Aeson.Null
  return YouTrackAuth {
    sessionId = (res ^. responseCookie "JSESSIONID" . cookieValue),
    principal = (res ^. responseCookie "jetbrains.charisma.main.security.PRINCIPAL" . cookieValue),
    hostname = "bar"
  }
    where opts = defaults & param "login" .~ [(T.pack username)] & param "password" .~ [(T.pack password)] & header "Accept" .~ ["application/json"]

-- getIssue :: YouTrackAuth -> Issue -> IO Resp
-- getIssue auth issue =
--   withManager $ \_ -> getWith opts issueUrl
--   where
--     issueUrl = () ++ issuePath ++ (show issue)
--     opts = defaults & header "Set-Cookie" .~ [C.pack $ auth ^. sessionId, C.pack $ auth ^. principal]

-- issueExists :: YouTrackAuth -> Issue -> IO Bool
-- issueExists auth issue = do
--  r <- getWith opts (issueUrl ++ "/" ++ (show issue) ++ "/" ++ "exists")
--  r ^. responseStatus . statusCode == 200

-- verifyIssue :: YouTrackAuth -> Issue -> IO Something
-- verifyIssue = undefined
