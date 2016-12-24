{-# LANGUAGE OverloadedStrings #-}

module API_Types_JSON where

import API_Types
import Aux_Types
import Data.Aeson
import Data.Maybe
import Data.Text (pack,unpack,Text)


instance (FromJSON n) => FromJSON (Package n) where
	parseJSON (Object v) = Package <$>
							v .: "ok" <*>
							v .: "result" 
	parseJSON _ = mempty

instance FromJSON Update where
	parseJSON (Object v ) = Update <$>
							v .: "update_id" <*>
							v .:? "message" <*>
							v .:? "edited_message" <*>
							v .:? "channel_post" <*>
							v .:? "edited_channel_post" <*>
							v .:? "callback_query"
	parseJSON _ = mempty		
						
instance FromJSON User where
	parseJSON (Object v) = User <$>
						   v .: "id" <*>
						   v .: "first_name" <*>
						   v .:? "last_name" <*>
						   v .:? "username"	
	parseJSON _ = mempty

instance FromJSON Chat where
	parseJSON (Object v) = Chat <$>
							v .: "id" <*>
							v .: "type" <*>
							v .:? "title" <*>
							v .:? "username" <*>
							v .:? "first_name" <*>
							v .:? "last_name" <*>
							v .:? "all_members_are_administrators"
	parseJSON _ = mempty	

instance FromJSON Message where
	parseJSON (Object v) = Message <$>
							v .: "message_id" <*>
							v .:? "from" <*>
							v .: "date" <*>
							v .: "chat" <*>
							v .:? "forward_from" <*>
							v .:? "forward_from_chat" <*>
							v .:? "forward_from_message_id" <*>
							v .:? "forward_date" <*>
							v .:? "reply_to_message" <*>
							v .:? "edit_date" <*>
							v .:? "text" <*>
							v .:? "entities" <*>
							v .:? "audio" <*>
							v .:? "document" <*>
							v .:? "game" <*>
							v .:? "photo" <*>
							v .:? "sticker" <*>
							v .:? "video" <*>
							v .:? "voice" <*>
							v .:? "caption" <*>
							v .:? "contact" <*>
							v .:? "location" <*>
							v .:? "venue" <*>
							v .:? "new_chat_member" <*>
							v .:? "left_chat_member" <*>
							v .:? "new_chat_title" <*>
							v .:? "new_chat_photo" <*>
							v .:? "delete_chat_photo" <*>
							v .:? "group_chat_created" <*>
							v .:? "supergroup_chat_created" <*>
							v .:? "channel_chat_created" <*>
							v .:? "migrate_to_chat_id" <*>
							v .:? "migrate_from_chat_id" <*>
							v .:? "pinned_message"														
	parseJSON _ = mempty

instance FromJSON MessageEntity where
	parseJSON (Object v) = MessageEntity <$>
							v .: "type" <*>
							v .: "offset" <*>
							v .: "length" <*>
							v .:? "url" <*>
							v .:? "user"
	parseJSON _ = mempty

instance FromJSON PhotoSize where
	parseJSON (Object v) = PhotoSize <$>
							v .: "file_id" <*>
							v .: "width" <*>
							v .: "height" <*>
							v .:? "file_size"
	parseJSON _ = mempty						

instance FromJSON Audio where
	parseJSON (Object v) = Audio <$>
							v .: "file_id" <*>
							v .: "duration" <*>
							v .:? "performer" <*>
							v .:? "title" <*>
							v .:? "mime_type" <*>
							v .:? "file_size"	
	parseJSON _ = mempty						

instance FromJSON Document where
	parseJSON (Object v) = Document <$>
							v .: "file_id" <*>
							v .:? "thumb" <*>
							v .:? "file_name" <*>
							v .:? "mime_type" <*>
							v .:? "file_size"	
	parseJSON _ = mempty						

instance FromJSON Sticker where
	parseJSON (Object v) = Sticker <$>
							v .: "file_id" <*>
							v .: "width" <*>
							v .: "height" <*>
							v .:? "thumb" <*>
							v .:? "emoji" <*>
							v .:? "file_size"
	parseJSON _ = mempty							

instance FromJSON Video where
	parseJSON (Object v) = Video <$>
							v .: "file_id" <*>
							v .: "width" <*>
							v .: "heigh" <*>
							v .: "duration" <*>
							v .:? "thumb" <*>
							v .:? "mime_type" <*>
							v .:? "file_size"
	parseJSON _ = mempty						

instance FromJSON Voice where
	parseJSON (Object v) = Voice <$>
							v .: "file_id" <*>
							v .: "duration" <*>
							v .:? "mime_type" <*>
							v .:? "file_size"
	parseJSON _ = mempty						

instance FromJSON Contact where
	parseJSON (Object v) = Contact <$>
							v .: "phone_number" <*>
							v .: "first_name" <*>
							v .:? "last_name" <*>
							v .:? "user_id"
	parseJSON _ = mempty						

instance FromJSON Location where
	parseJSON (Object v) = Location <$>
							v .: "longitude" <*>
							v .: "latitude"
	parseJSON _ = mempty						

instance FromJSON Venue where
	parseJSON (Object v) = Venue <$>
							v .: "location" <*>
							v .: "title" <*>
							v .: "address" <*>
							v .:? "foursquare_id"
	parseJSON _ = mempty						

instance FromJSON UserProfilePhotos where
	parseJSON (Object v) = UserProfilePhotos <$>
							v .: "total_count" <*>
							v .: "photos" 	
	parseJSON _ = mempty												

instance FromJSON File where
	parseJSON (Object v) = File <$>
							v .: "file_id" <*>
							v .:? "file_size" <*>
							v .:? "file_path"	
	parseJSON _ = mempty						

instance FromJSON ReplyKeyboardMarkup where
	parseJSON (Object v) = ReplyKeyboardMarkup <$>
							v .: "keyboard" <*>
							v .:? "resize_keybord" <*>
							v .:? "one_time_keyboard" <*>
							v .:? "selective"
	parseJSON _ = mempty						

instance FromJSON KeyboardButton where
	parseJSON (Object v) = KeyboardButton <$>
							v .: "text" <*>
							v .:? "request_contact" <*>
							v .:? "request_location" 
	parseJSON _ = mempty																

instance FromJSON ReplyKeyboardRemove where
	parseJSON (Object v) = ReplyKeyboardRemove <$>
							v .: "hide_keyboard" <*>
							v .:? "selective"	
	parseJSON _ = mempty							

instance FromJSON InlineKeyboardMarkup where
	parseJSON (Object v) = InlineKeyboardMarkup <$>
							v .: "inline_keyboard"
	parseJSON _ = mempty						

instance FromJSON InlineKeyboardButton where
	parseJSON (Object v) = InlineKeyboardButton <$>
							v .: "text" <*>
							v .:? "url" <*>
							v .:? "callback_data" <*>
							v .:? "switch_inline_query" <*>
							v .:? "switch_inline_query_current_chat" <*>
							v .:? "callback_game" 	
	parseJSON _ = mempty						

instance FromJSON CallbackQuery where
	parseJSON (Object v) = CallbackQuery <$>						
							v .: "id" <*>
							v .: "from" <*>
							v .:? "message" <*>
							v .:? "inline_message_id" <*>
							v .: "chat_instance" <*>
							v .: "data" <*>
							v .:? "game_short_name"
	parseJSON _ = mempty						
																																						
instance FromJSON ForceReply where
	parseJSON (Object v) = ForceReply <$>
							v .: "force_reply" <*>
							v .:? "selective"
	parseJSON _ = mempty						

instance FromJSON ChatMember where
	parseJSON (Object v) = ChatMember <$>
							v .: "user" <*>
							v .: "status"
	parseJSON _ = mempty						

instance FromJSON ResponseParameters where
	parseJSON (Object v) = ResponseParameters <$>
							v .:? "migrate_to_chat_id"<*>
							v .:? "retry_after"
	parseJSON _ = mempty						

instance FromJSON Game where
	parseJSON (Object v) = Game <$>
							v .: "title" <*>
							v .: "description" <*>
							v .: "photo" <*>
							v .:? "text" <*>
							v .:? "text_entities" <*>
							v .:? "animation"
	parseJSON _ = mempty						

instance FromJSON Animation where
	parseJSON (Object v) = Animation <$>
							v .: "file_id" <*>
							v .:? "thumb" <*>
							v .:? "file_name" <*>
							v .:? "mime_type" <*>
							v .:? "file_size"
	parseJSON _ = mempty						

instance FromJSON CallbackGame

instance FromJSON GameHighScore where
	parseJSON (Object v) = GameHighScore <$>
							 v .: "position" <*>
							 v .: "user" <*>
							 v .: "score"
	parseJSON _ = mempty						 


-- TO JSON

instance ToJSON KeyboardButton where
	toJSON (KeyboardButton text request_contact request_location) =
		object $ ["text" .= text]
			++ (addMaybeParam "request_contact" request_contact)
			++ (addMaybeParam "request_location" request_location)
		
instance ToJSON InlineKeyboardButton where
	toJSON (InlineKeyboardButton text url callback_data switch_inline_query switch_inline_query_current_chat callback_game) = 
		object $ ["text" .= text]
		++ (addMaybeParam "url" url)
		++ (addMaybeParam "callback_data" callback_data)
		++ (addMaybeParam "switch_inline_query" switch_inline_query)
		++ (addMaybeParam "switch_inline_query_current_chat" switch_inline_query_current_chat)
		++ (addMaybeParam "callback_game" callback_game)

instance ToJSON CallbackGame	
 
instance ToJSON ChatId where
	toJSON x =
		case x of
			IsInt n -> String $ Data.Text.pack $ show n
			IsString n -> String $ Data.Text.pack n

instance ToJSON ReplyMarkup where
	toJSON x =
		case x of
			IsRKM n -> object ["reply_markup" .= n]
			IsRKR n -> object ["reply_markup" .= n]
			IsIKM n -> object ["reply_markup" .= n]
			IsFR n -> object ["reply_markup" .= n]

instance ToJSON ReplyKeyboardMarkup where
	toJSON (ReplyKeyboardMarkup keyboard resize_keybord one_time_keyboard selective) = 
		object $ ["keyboard" .= keyboard] 
			++ (addMaybeParam	"resize_keybord" resize_keybord)	
			++ (addMaybeParam "one_time_keyboard" one_time_keyboard)
			++ (addMaybeParam "selective" selective)

instance ToJSON ReplyKeyboardRemove where
	toJSON (ReplyKeyboardRemove keyboard selective) = 
		object $ ["keyboard" .= keyboard] 
			++ (addMaybeParam "selective" selective)

instance ToJSON InlineKeyboardMarkup where
	toJSON (InlineKeyboardMarkup inline_keyboard) =
		object ["inline_keyboard" .= inline_keyboard] 		

instance ToJSON ForceReply where
	toJSON (ForceReply force_reply selective) =
		object $ ["force_reply" .= force_reply]
			++ (addMaybeParam "selective" selective) 				