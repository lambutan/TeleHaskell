{-# LANGUAGE OverloadedStrings #-}

module Method_Body where

import API_Types
import Aux_Types
import API_Types_JSON
import Data.Aeson

data SendMessageBody = SendMessageBody { sendMessageChatId :: ChatId
									   , sendMessageText :: String
									   , sendMessageParseMode :: Maybe String
									   , sendMessageDisableWebPagePreview :: Maybe Bool
									   , sendMessageDisableNotification :: Maybe Bool
									   , sendMessageReplyToMessageId :: Maybe Integer
									   , sendMessageReplyMarkup :: Maybe ReplyMarkup}

instance ToJSON SendMessageBody where
	toJSON (SendMessageBody chat_id text parse_mode disable_web_page_preview disable_notification reply_to_message_id reply_markup) =
	 object $ ["chat_id" .= chat_id]
	 	++ ["text" .= text]
	 	++ (addMaybeParam "parse_mode" parse_mode)
	 	++ (addMaybeParam "disable_web_page_preview" disable_web_page_preview)
	 	++ (addMaybeParam "disable_notification" disable_notification)
	 	++ (addMaybeParam "reply_to_message_id" reply_to_message_id)
	 	++ (addMaybeParam "reply_markup" reply_markup)

data ForwardMessageBody = ForwardMessageBody { forwardMessageChatId :: ChatId
											 , forwardMessageFromChatId :: ChatId
											 , forwardMessageDisableNotification :: Maybe Bool
											 , forwardMessageMessageId :: Integer}

instance ToJSON ForwardMessageBody where
	toJSON (ForwardMessageBody chat_id from_chat_id disable_notification message_id) = 
		object $ ["chat_id" .= chat_id]
		++ ["from_chat_id" .= from_chat_id]
		++ (addMaybeParam "disable_notification" disable_notification)
		++ ["message_id" .= message_id]

-- sendPhoto ::

-- sendAudio ::

-- sendDocument ::

-- sendSticker ::

-- sendVideo ::

-- sendVoice ::	 	

data SendLocationBody = SendLocationBody {sendLocationChatId :: ChatId
										, sendLocationLatitude :: Float
										, sendLocationLongitude :: Float
										, sendLocationDisableNotification :: Maybe Bool
										, sendLocationReplyToMessageId :: Maybe Integer
										, sendLocationReplyMarkup :: Maybe ReplyMarkup} 

instance ToJSON SendLocationBody where
	toJSON (SendLocationBody chat_id latitude longitude disable_notification reply_to_message_id reply_markup) = 
		object $ ["chat_id" .= chat_id]
			++ ["latitude" .= latitude]
			++ ["longitude" .= longitude]
			++ (addMaybeParam "disable_notification" disable_notification)
			++ (addMaybeParam "reply_to_message_id" reply_to_message_id)
			++ (addMaybeParam "reply_markup" reply_markup)

data SendVenueBody = SendVenueBody {sendVenueChatId :: ChatId
								  , sendVenueLatitude :: Float
								  , sendVenueLongitude :: Float
								  , sendVenueTitle :: String
								  , sendVenueAddress :: String
								  , sendVenueFoursquareId :: Maybe String
								  , sendVenueDisableNotification :: Maybe Bool
								  , sendVenueReplyToMessageId :: Maybe Integer
								  , sendVenueReplyMarkup :: Maybe ReplyMarkup} 

instance ToJSON SendVenueBody where
	toJSON (SendVenueBody chat_id latitude longitude title address foursquare_id disable_notification reply_to_message_id reply_markup) = 
		object $ ["chat_id" .= chat_id]
			++ ["latitude" .= latitude]
			++ ["longitude" .= longitude]
			++ ["title" .= title]
			++ ["address" .= address]
			++ (addMaybeParam "foursquare_id" foursquare_id)
			++ (addMaybeParam "disable_notification" disable_notification)
			++ (addMaybeParam "reply_to_message_id" reply_to_message_id)
			++ (addMaybeParam "reply_markup" reply_markup)

data SendContactBody = SendContactBody {sendContactBodyChatId :: ChatId
									  , sendContactPhoneNumber :: String
									  , sendContactFirstName :: String
									  , sendContactLastName :: Maybe String
									  , sendContactDisableNotification :: Maybe Bool
									  , sendContactReplyToMessageId :: Maybe Integer
									  , sendContactReplyMarkup :: Maybe ReplyMarkup}

instance ToJSON SendContactBody where
  	toJSON (SendContactBody chat_id phone_number first_name last_name disable_notification reply_to_message_id reply_markup) = 									  
  		object $ ["chat_id" .= chat_id]
  			++ ["phone_number" .= phone_number]
  			++ ["first_name" .= first_name]
  			++ (addMaybeParam "last_name" last_name)
  			++ (addMaybeParam "disable_notification" disable_notification)
  			++ (addMaybeParam "reply_to_message_id" reply_to_message_id)
  			++ (addMaybeParam "reply_markup" reply_markup)

data SendChatActionBody = SendChatActionBody {sendChatActionChatId :: ChatId
											, sendChatActionAction :: String}
instance ToJSON SendChatActionBody where
	toJSON (SendChatActionBody chat_id action) = 
		object $ ["chat_id" .= chat_id]
			++ ["action" .= action]										

data GetUserProfilePhotosBody = GetUserProfilePhotosBody {getUserProfilePhotosUserId :: Integer
							   							, getUserProfilePhotosOffset :: Maybe Integer
							   							, getUserProfilePhotosLimit :: Maybe Integer}

instance ToJSON GetUserProfilePhotosBody where
	toJSON (GetUserProfilePhotosBody user_id offset limit) =
		object $ ["user_id" .= user_id]
			++ (addMaybeParam "offset" offset)
			++ (addMaybeParam "limit" limit) 

data GetFileBody = GetFileBody {getFileBodyFileId :: String}
instance ToJSON GetFileBody where
	toJSON (GetFileBody file_id) =
		object $ ["file_id" .= file_id]

data KickChatMemberBody = KickChatMemberBody { kickChatMemberChatId :: ChatId
										   , kickChatMemberUserId :: Integer}
instance ToJSON KickChatMemberBody where
	toJSON (KickChatMemberBody chat_id user_id) =
		object $ ["chat_id" .= chat_id]
			++ ["user_id" .= user_id] 

data LeaveChatBody = LeaveChatBody { leaveChatChatId :: ChatId}
instance ToJSON LeaveChatBody where
	toJSON (LeaveChatBody chat_id) =
		object $ ["chat_id" .= chat_id]

data UnbanChatMemberBody = UnbanChatMemberBody { unbanChatMemberChatId :: ChatId
										   , unbanChatMemberUserId :: Integer}
instance ToJSON UnbanChatMemberBody where
	toJSON (UnbanChatMemberBody chat_id user_id) =
		object $ ["chat_id" .= chat_id]
			++ ["user_id" .= user_id] 


data GetChatBody = GetChatBody { getChatChatId :: ChatId}
instance ToJSON GetChatBody where
	toJSON (GetChatBody chat_id) =
		object $ ["chat_id" .= chat_id]

data GetChatAdministratorsBody = GetChatAdministratorsBody { getChatAdministratorsChatId :: ChatId}
instance ToJSON GetChatAdministratorsBody where
	toJSON (GetChatAdministratorsBody chat_id) =
		object $ ["chat_id" .= chat_id]

data GetChatMembersCountBody = GetChatMembersCountBody { getChatMembersCountChatId :: ChatId}
instance ToJSON GetChatMembersCountBody where
	toJSON (GetChatMembersCountBody chat_id) =
		object $ ["chat_id" .= chat_id]

data GetChatMemberBody = GetChatMemberBody { getChatMemberChatId :: ChatId
										   , getChatMemberUserId :: Integer}
instance ToJSON GetChatMemberBody where
	toJSON (GetChatMemberBody chat_id user_id) =
		object $ ["chat_id" .= chat_id]
			++ ["user_id" .= user_id] 

data AnswerCallbackQueryBody = AnswerCallbackQueryBody {answerCallbackQueryCallbackQueryId :: String
													  , answerCallbackQueryText :: String
													  , answerCallbackQueryShowAlert :: Maybe Bool
													  , answerCallbackQueryUrl :: Maybe String
													  , answerCallbackQueryCacheTime :: Maybe Integer}
instance ToJSON AnswerCallbackQueryBody where
	toJSON (AnswerCallbackQueryBody callback_query_id text show_alert url cache_time) = 
		object $ ["callback_query_id" .= callback_query_id]
			++ ["text" .= text]
			++ (addMaybeParam "show_alert" show_alert)
			++ (addMaybeParam "url" url)
			++ (addMaybeParam "cache_time" cache_time)													  