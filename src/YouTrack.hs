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
import Control.Exception as E
import qualified Network.HTTP.Client as HttpClient

type Hostname = String
type Username = String
type Password = String
type Issue = String

userLoginPath :: String
userLoginPath = "/rest/user/login"

issuePath :: String
issuePath = "/rest/issue/"

data YouTrackAuth = YouTrackAuth {
  hostname :: C.ByteString,
  sessionId :: C.ByteString,
  principal :: C.ByteString
} deriving Show

type Resp = Response LBS.ByteString
type RespMap = Response (Map String Value)

mkYouTrackAuth :: Hostname -> Username -> Password -> IO YouTrackAuth
mkYouTrackAuth hostname username password = do
  res <- withManager $ \_ -> postWith opts (hostname ++ userLoginPath) Data.Aeson.Null
  return YouTrackAuth {
    sessionId = (res ^. responseCookie "JSESSIONID" . cookieValue),
    principal = (res ^. responseCookie "jetbrains.charisma.main.security.PRINCIPAL" . cookieValue),
    hostname = (C.pack hostname)
  }
    where opts = defaults & param "login" .~ [(T.pack username)] & param "password" .~ [(T.pack password)] & header "Accept" .~ ["application/json"]

getIssue :: YouTrackAuth -> Issue -> IO Resp
getIssue auth issue =
  withManager $ \_ -> getWith opts issueUrl
  where
    issueUrl = (C.unpack $ hostname auth) ++ issuePath ++ issue
    opts = buildAuthOptions auth

issueExists :: YouTrackAuth -> Issue -> IO Bool
issueExists auth issue = do
  withManager $ \_ -> do
    r <- getWith opts issueExistsUrl
    return $ r ^. responseStatus . statusCode == 200
  where
    issueExistsUrl = (C.unpack $ hostname auth) ++ issuePath ++ issue ++ "/exists"
    opts = buildAuthOptions auth

buildAuthOptions :: YouTrackAuth -> Options
buildAuthOptions auth = defaults &
                        header "Accept" .~ ["application/json"] &
                        header "Cookie" .~ [C.append (C.pack "JSESSIONID=") (sessionId auth), C.append (C.pack "jetbrains.charisma.main.security.PRINCIPAL=") (principal auth)]

verifyIssue :: YouTrackAuth -> Issue -> String -> String -> String -> String -> IO ()
verifyIssue auth issue uri release assignee comment = do
  withManager $ \_ ->
    postWith (opts & param "command" .~ [setReleaseCommand]) issueExecuteUrl (C.pack "") >>
    postWith (opts & param "command" .~ [setVerifyCommand]) issueExecuteUrl (C.pack "") >>
    postWith (opts & param "command" .~ [setAssigneeCommand]) issueExecuteUrl (C.pack "") >>
    postWith (opts & param "comment" .~ [fullComment]) issueExecuteUrl (C.pack "") >>
    return ()
    where
      opts = buildAuthOptions auth
      issueExecuteUrl = (C.unpack $ hostname auth) ++ issuePath ++ issue ++ "/execute"
      setReleaseCommand = T.pack $ "In Release " ++ release
      setVerifyCommand = T.pack $ "State Verified"
      setAssigneeCommand = T.pack $ "Assignee " ++ assignee
      fullComment = T.pack $ comment ++ "\nPull request: " ++ uri ++ "\nAuthor: " ++ assignee
