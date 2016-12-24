{-# LANGUAGE OverloadedStrings #-}

module Methods where

import API_Types
import API_Types_JSON
import Aux_Types
import Data.Aeson
import Method_Body
import Network.HTTP.Simple (httpJSON, getResponseBody, parseRequest, setRequestBodyJSON)

getMe :: Bot -> IO (Package User)
getMe bot = do
	initReq <- parseRequest $ apiTelegram ++ token bot ++ "/getMe"
	response <- httpJSON initReq
	return $ getResponseBody response

sendMessage :: Bot -> SendMessageBody -> IO (Package Message)
sendMessage bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/sendMessage"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

forwardMessage :: Bot -> ForwardMessageBody -> IO (Package Message)
forwardMessage bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/forwardMessage"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

-- sendPhoto ::

-- sendAudio ::

-- sendDocument ::

-- sendSticker ::

-- sendVideo ::

-- sendVoice ::

sendLocation :: Bot -> SendLocationBody -> IO (Package Message)
sendLocation bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/sendLocation"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

sendVenue :: Bot -> SendVenueBody -> IO (Package Message)
sendVenue bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/sendVenue"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

sendContact :: Bot -> SendContactBody -> IO (Package Message)
sendContact bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/sendContact"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

sendChatAction :: Bot -> SendChatActionBody -> IO (Package Bool)
sendChatAction bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/sendChatAction"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

getUserProfilePhotos :: Bot -> GetUserProfilePhotosBody -> IO (Package UserProfilePhotos)
getUserProfilePhotos bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/getUserProfilePhotos"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

-- getFile ::

kickChatMember :: Bot -> KickChatMemberBody -> IO (Package Bool)
kickChatMember bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/kickChatMember"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

leaveChat :: Bot -> LeaveChatBody -> IO (Package Bool)
leaveChat bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/leaveChat"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

unbanChatMember :: Bot -> UnbanChatMemberBody -> IO (Package Bool)
unbanChatMember bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/unbanChatMember"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

getChat :: Bot -> GetChatBody -> IO (Package Chat)
getChat bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/getChat"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

getChatAdministrators :: Bot -> GetChatAdministratorsBody -> IO (Package [ChatMember])
getChatAdministrators bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/getChatAdministrators"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

getChatMembersCount :: Bot -> GetChatMembersCountBody -> IO (Package Int)
getChatMembersCount bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/getChatMembersCount"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

getChatMember :: Bot -> GetChatMemberBody -> IO (Package ChatMember)
getChatMember bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/getChatMember"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response

answerCallbackQuery :: Bot -> AnswerCallbackQueryBody -> IO (Package ChatMember)
answerCallbackQuery bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/answerCallbackQuery"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response	