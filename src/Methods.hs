{-# LANGUAGE OverloadedStrings #-}

module Methods where

import API_Types
import Aux_Types
import Data.Aeson
import Method_Body
import Network.HTTP.Simple (httpJSON, getResponseBody, parseRequest, setRequestBodyJSON)

getMe :: Bot -> IO (Package User)
getMe bot = do
	initReq <- parseRequest $ apiTelegram ++ token bot ++ "/GetMe"
	response <- httpJSON initReq
	return $ getResponseBody response

data POSTRequest = SendMessage
				 | ForwardMessage
				 | SendLocation
				 | SendVenue
				 | SendContact
				 | SendChatAction
				 | GetUserProfilePhotos
				 | GetFile
				 | KickChatMember
				 | LeaveChat
				 | UnbanChatMember
				 | GetChat
				 | GetChatAdministrators
				 | GetChatMembersCount
				 | GetChatMember
				 | AnswerCallbackQuery deriving Show

sendPOSTRequest :: (FromJSON a) => Bot -> POSTRequest -> POSTBody -> IO (Package a)
sendPOSTRequest bot req body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/" ++ (show req)
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

sendMessage :: Bot -> POSTBody -> IO (Package Message)
sendMessage bot body = sendPOSTRequest bot SendMessage body

forwardMessage :: Bot -> POSTBody -> IO (Package Message)
forwardMessage bot body = sendPOSTRequest bot ForwardMessage body

sendLocation :: Bot -> POSTBody -> IO (Package Message)
sendLocation bot body = sendPOSTRequest bot SendLocation body

sendVenue :: Bot -> POSTBody -> IO (Package Message)
sendVenue bot body = sendPOSTRequest bot SendVenue body

sendContact :: Bot -> POSTBody -> IO (Package Message)
sendContact bot body = sendPOSTRequest bot SendContact body

sendChatAction :: Bot -> POSTBody -> IO (Package Bool)
sendChatAction bot body = sendPOSTRequest bot SendChatAction body

getUserProfilePhotos :: Bot -> POSTBody -> IO (Package UserProfilePhotos)
getUserProfilePhotos bot body = sendPOSTRequest bot GetUserProfilePhotos body

getFile :: Bot -> POSTBody -> IO (Package File)
getFile bot body = sendPOSTRequest bot GetFile body

kickChatMember :: Bot -> POSTBody -> IO (Package Bool)
kickChatMember bot body = sendPOSTRequest bot KickChatMember body

leaveChat :: Bot -> POSTBody -> IO (Package Bool)
leaveChat bot body = sendPOSTRequest bot LeaveChat body

unbanChatMember :: Bot -> POSTBody -> IO (Package Bool)
unbanChatMember bot body = sendPOSTRequest bot UnbanChatMember body

getChat :: Bot -> POSTBody -> IO (Package Chat)
getChat bot body = sendPOSTRequest bot GetChat body

getChatAdministrators :: Bot -> POSTBody -> IO (Package [ChatMember])
getChatAdministrators bot body = sendPOSTRequest bot GetChatAdministrators body

getChatMembersCount :: Bot -> POSTBody -> IO (Package Int)
getChatMembersCount bot body = sendPOSTRequest bot GetChatMembersCount body

getChatMember :: Bot -> POSTBody -> IO (Package ChatMember)
getChatMember bot body = sendPOSTRequest bot GetChatMember body

answerCallbackQuery :: Bot -> POSTBody -> IO (Package Bool)
answerCallbackQuery bot body = sendPOSTRequest bot AnswerCallbackQuery body

-- sendPhoto ::

-- sendAudio ::

-- sendDocument ::

-- sendSticker ::

-- sendVideo ::

-- sendVoice ::