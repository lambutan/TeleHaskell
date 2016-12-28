{-# LANGUAGE OverloadedStrings #-}

module Method_Body where

import API_Types
import Aux_Types
import Data.Aeson

data POSTBody = SendMessageBody { sendMessageChatId :: ChatId
								, sendMessageText :: String
								, sendMessageParseMode :: Maybe String
								, sendMessageDisableWebPagePreview :: Maybe Bool
								, sendMessageDisableNotification :: Maybe Bool
								, sendMessageReplyToMessageId :: Maybe Integer
								, sendMessageReplyMarkup :: Maybe ReplyMarkup}

			  |	SendMessageBody_ { sendMessageChatId_ :: ChatId
								 , sendMessageText_ :: String}

			  | ForwardMessageBody { forwardMessageChatId :: ChatId
								   , forwardMessageFromChatId :: ChatId
								   , forwardMessageDisableNotification :: Maybe Bool
								   , forwardMessageMessageId :: Integer}

			  | ForwardMessageBody_ { forwardMessageChatId_ :: ChatId
								    , forwardMessageFromChatId_ :: ChatId
								    , forwardMessageMessageId_ :: Integer}	

			  | SendLocationBody {sendLocationChatId :: ChatId
								, sendLocationLatitude :: Float
								, sendLocationLongitude :: Float
								, sendLocationDisableNotification :: Maybe Bool
								, sendLocationReplyToMessageId :: Maybe Integer
								, sendLocationReplyMarkup :: Maybe ReplyMarkup} 

			  | SendLocationBody_ {sendLocationChatId_ :: ChatId
								 , sendLocationLatitude_ :: Float
								 , sendLocationLongitude_ :: Float} 	

			  | SendVenueBody {sendVenueChatId :: ChatId
							 , sendVenueLatitude :: Float
							 , sendVenueLongitude :: Float
							 , sendVenueTitle :: String
							 , sendVenueAddress :: String
							 , sendVenueFoursquareId :: Maybe String
							 , sendVenueDisableNotification :: Maybe Bool
							 , sendVenueReplyToMessageId :: Maybe Integer
							 , sendVenueReplyMarkup :: Maybe ReplyMarkup} 		

			  | SendVenueBody_ {sendVenueChatId_ :: ChatId
							  , sendVenueLatitude_ :: Float
							  , sendVenueLongitude_ :: Float
							  , sendVenueTitle_ :: String
							  , sendVenueAddress_ :: String} 

			  | SendContactBody {sendContactBodyChatId :: ChatId
							   , sendContactPhoneNumber :: String
							   , sendContactFirstName :: String
							   , sendContactLastName :: Maybe String
							   , sendContactDisableNotification :: Maybe Bool
							   , sendContactReplyToMessageId :: Maybe Integer
							   , sendContactReplyMarkup :: Maybe ReplyMarkup}	

			  | SendContactBody_ {sendContactBodyChatId_ :: ChatId
							    , sendContactPhoneNumber_ :: String
							    , sendContactFirstName_ :: String}	

			  | SendChatActionBody {sendChatActionChatId :: ChatId
								  , sendChatActionAction :: String}		

			  | GetUserProfilePhotosBody {getUserProfilePhotosUserId :: Integer
							   			, getUserProfilePhotosOffset :: Maybe Integer
							   			, getUserProfilePhotosLimit :: Maybe Integer}

			  | GetUserProfilePhotosBody_ {getUserProfilePhotosUserId_ :: Integer}		

			  | GetFileBody {getFileBodyFileId :: String}

			  | KickChatMemberBody { kickChatMemberChatId :: ChatId
								   , kickChatMemberUserId :: Integer}

			  | LeaveChatBody { leaveChatChatId :: ChatId}

			  | UnbanChatMemberBody { unbanChatMemberChatId :: ChatId
									, unbanChatMemberUserId :: Integer}

			  | GetChatBody { getChatChatId :: ChatId}

			  | GetChatAdministratorsBody { getChatAdministratorsChatId :: ChatId}

			  | GetChatMembersCountBody { getChatMembersCountChatId :: ChatId}

			  | GetChatMemberBody { getChatMemberChatId :: ChatId
								  , getChatMemberUserId :: Integer}

			  | AnswerCallbackQueryBody {answerCallbackQueryCallbackQueryId :: String
									   , answerCallbackQueryText :: String
									   , answerCallbackQueryShowAlert :: Maybe Bool
									   , answerCallbackQueryUrl :: Maybe String
									   , answerCallbackQueryCacheTime :: Maybe Integer}

			  | AnswerCallbackQueryBody_ {answerCallbackQueryCallbackQueryId_ :: String
									    , answerCallbackQueryText_ :: String}						  


instance ToJSON POSTBody where
	toJSON n =
		case n of
			SendMessageBody chat_id text parse_mode disable_web_page_preview disable_notification reply_to_message_id reply_markup -> 
				object $ ["chat_id" .= chat_id]
	 			++ ["text" .= text]
	 			++ (addMaybeParam "parse_mode" parse_mode)
	 			++ (addMaybeParam "disable_web_page_preview" disable_web_page_preview)
	 			++ (addMaybeParam "disable_notification" disable_notification)
	 			++ (addMaybeParam "reply_to_message_id" reply_to_message_id)
	 			++ (addMaybeParam "reply_markup" reply_markup)
	 		SendMessageBody_ chat_id text -> 
				object $ ["chat_id" .= chat_id]
	 			++ ["text" .= text]
	 		ForwardMessageBody chat_id from_chat_id disable_notification message_id ->
	 			object $ ["chat_id" .= chat_id]
				++ ["from_chat_id" .= from_chat_id]
				++ (addMaybeParam "disable_notification" disable_notification)
				++ ["message_id" .= message_id]
			ForwardMessageBody_ chat_id from_chat_id message_id	->
				object $ ["chat_id" .= chat_id]
				++ ["from_chat_id" .= from_chat_id]
				++ ["message_id" .= message_id]
			SendLocationBody chat_id latitude longitude disable_notification reply_to_message_id reply_markup ->	
				object $ ["chat_id" .= chat_id]
				++ ["latitude" .= latitude]
				++ ["longitude" .= longitude]
				++ (addMaybeParam "disable_notification" disable_notification)
				++ (addMaybeParam "reply_to_message_id" reply_to_message_id)
				++ (addMaybeParam "reply_markup" reply_markup)
			SendLocationBody_ chat_id latitude longitude ->	
				object $ ["chat_id" .= chat_id]
				++ ["latitude" .= latitude]
				++ ["longitude" .= longitude]
			SendVenueBody chat_id latitude longitude title address foursquare_id disable_notification reply_to_message_id reply_markup ->
				object $ ["chat_id" .= chat_id]
				++ ["latitude" .= latitude]
				++ ["longitude" .= longitude]
				++ ["title" .= title]
				++ ["address" .= address]
				++ (addMaybeParam "foursquare_id" foursquare_id)
				++ (addMaybeParam "disable_notification" disable_notification)
				++ (addMaybeParam "reply_to_message_id" reply_to_message_id)
				++ (addMaybeParam "reply_markup" reply_markup)
			SendVenueBody_ chat_id latitude longitude title address ->
				object $ ["chat_id" .= chat_id]
				++ ["latitude" .= latitude]
				++ ["longitude" .= longitude]
				++ ["title" .= title]
				++ ["address" .= address]
			SendContactBody chat_id phone_number first_name last_name disable_notification reply_to_message_id reply_markup ->
				object $ ["chat_id" .= chat_id]
  				++ ["phone_number" .= phone_number]
  				++ ["first_name" .= first_name]
  				++ (addMaybeParam "last_name" last_name)
  				++ (addMaybeParam "disable_notification" disable_notification)
  				++ (addMaybeParam "reply_to_message_id" reply_to_message_id)
  				++ (addMaybeParam "reply_markup" reply_markup)
  			SendContactBody_ chat_id phone_number first_name ->
				object $ ["chat_id" .= chat_id]
  				++ ["phone_number" .= phone_number]
  				++ ["first_name" .= first_name]	
  			SendChatActionBody chat_id action ->	
  				object $ ["chat_id" .= chat_id]
				++ ["action" .= action]	
			GetUserProfilePhotosBody user_id offset limit -> 		
				object $ ["user_id" .= user_id]
				++ (addMaybeParam "offset" offset)
				++ (addMaybeParam "limit" limit)
			GetUserProfilePhotosBody_ user_id  -> 		
				object $ ["user_id" .= user_id] 
			GetFileBody file_id ->
				object $ ["file_id" .= file_id]
			KickChatMemberBody chat_id user_id ->
				object $ ["chat_id" .= chat_id]
				++ ["user_id" .= user_id]
			LeaveChatBody chat_id ->
				object $ ["chat_id" .= chat_id]
			UnbanChatMemberBody chat_id user_id ->
				object $ ["chat_id" .= chat_id]
				++ ["user_id" .= user_id] 
			GetChatBody chat_id -> 
				object $ ["chat_id" .= chat_id]
			GetChatAdministratorsBody chat_id ->
				object $ ["chat_id" .= chat_id]
			GetChatMembersCountBody chat_id -> 
				object $ ["chat_id" .= chat_id]
			GetChatMemberBody chat_id user_id ->
				object $ ["chat_id" .= chat_id]
				++ ["user_id" .= user_id]
			AnswerCallbackQueryBody callback_query_id text show_alert url cache_time -> 
				object $ ["callback_query_id" .= callback_query_id]
				++ ["text" .= text]
				++ (addMaybeParam "show_alert" show_alert)
				++ (addMaybeParam "url" url)
				++ (addMaybeParam "cache_time" cache_time)
			AnswerCallbackQueryBody_ callback_query_id text -> 
				object $ ["callback_query_id" .= callback_query_id]
				++ ["text" .= text]

															  

-- sendPhoto ::

-- sendAudio ::

-- sendDocument ::

-- sendSticker ::

-- sendVideo ::

-- sendVoice ::	 	
