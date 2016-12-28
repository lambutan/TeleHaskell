{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module API_Types where

import GHC.Generics
import Aux_Types
import Data.Aeson
import Data.Maybe
import Data.Text (pack,unpack,Text)

-- Types defined in the API

data Update = Update {updateId :: Integer
					, updateMessage :: Maybe Message
					, updateEditedMessage :: Maybe Message
					, updateChannelPost :: Maybe Message
					, updateEditedChannelPost :: Maybe Message
					, updateCallbackQuery :: Maybe CallbackQuery} deriving Show

instance FromJSON Update where
	parseJSON (Object v ) = Update <$>
							v .: "update_id" <*>
							v .:? "message" <*>
							v .:? "edited_message" <*>
							v .:? "channel_post" <*>
							v .:? "edited_channel_post" <*>
							v .:? "callback_query"
	parseJSON _ = mempty						

data WebhookInfo = WebhookInfo {webhookInfoUrl :: String
							  , webhookInfoHasCustomCertificate :: Bool
							  , webhookInfoPendingUpdateCount :: Integer
							  , webhookInfoLastErrorDate :: Maybe Integer
							  , webhookInfoLastErrorMessage :: Maybe String
							  , webhookInfoMaxConnections :: Maybe Integer
							  , webhookInfoAllowedUpdates :: Maybe [String]} deriving Show



data User = User {userId :: Integer
				, userFirstName :: String
				, userLastName :: Maybe String
				, userUserName :: Maybe String} deriving Show

instance FromJSON User where
	parseJSON (Object v) = User <$>
						   v .: "id" <*>
						   v .: "first_name" <*>
						   v .:? "last_name" <*>
						   v .:? "username"	
	parseJSON _ = mempty

data Chat = Chat {chatId :: Integer
				, chatType :: String
				, chatTitle :: Maybe String
				, chatUserName :: Maybe String
				, chatFirstName :: Maybe String
				, chatLastName :: Maybe String
				, chatAllAdmins :: Maybe Bool} deriving Show

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

data Message = Message {messageMessageId :: Integer
					  , messageFrom :: Maybe User
					  , messageDate :: Integer
					  , messageChat :: Chat
					  , messageForwardFrom :: Maybe User
					  , messageForwardFromChat :: Maybe Chat
					  , messageForwardFromMessageId :: Maybe Integer
					  , messageForwardDate :: Maybe Integer
					  , messageReplyToMessage :: Maybe Message
					  , messageEditDate :: Maybe Integer
					  , messageText :: Maybe String
					  , messageEntities :: Maybe [MessageEntity]
					  , messageAudio :: Maybe Audio
					  , messageDocument :: Maybe Document
					  , messageGame :: Maybe Game
					  , messagePhoto :: Maybe [PhotoSize]
					  , messageSticker :: Maybe Sticker
					  , messageVideo :: Maybe Video
					  , messageVoice :: Maybe Voice
					  , messageCaption :: Maybe String
					  , messageContact :: Maybe Contact
					  , messageLocation :: Maybe Location
					  , messageVenue :: Maybe Venue
					  , messageNewChatMember :: Maybe User
					  , messageLeftChatMember :: Maybe User
					  , messageNewChatTitle :: Maybe String
					  , messageNewChatPhoto :: Maybe [PhotoSize]
					  , messageDeleteChatPhoto :: Maybe Bool
					  , messageGroupChatCreated :: Maybe Bool
					  , messageSupergroupChatCreated :: Maybe Bool
					  , messageChannelChatCreated :: Maybe Bool
					  , messageMigrateToChatId :: Maybe Integer
					  , messageMigrateFromChatId :: Maybe Integer
					  , messagePinnedMessage :: Maybe Message} deriving Show

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

data MessageEntity = MessageEntity {messageEntityType :: String
								  , messageEntityOffset :: Integer
								  , messageEntityLength :: Integer
								  , messageEntityUrl :: Maybe String
								  , messageEntityUser :: Maybe User} deriving Show

instance FromJSON MessageEntity where
	parseJSON (Object v) = MessageEntity <$>
							v .: "type" <*>
							v .: "offset" <*>
							v .: "length" <*>
							v .:? "url" <*>
							v .:? "user"
	parseJSON _ = mempty


data PhotoSize = PhotoSize {photoSizeFileId :: String
						  , photoSizeWidth :: Integer
						  , photoSizeHeight :: Integer
						  , photoSizeFileSize :: Maybe Integer} deriving Show

instance FromJSON PhotoSize where
	parseJSON (Object v) = PhotoSize <$>
							v .: "file_id" <*>
							v .: "width" <*>
							v .: "height" <*>
							v .:? "file_size"
	parseJSON _ = mempty						

data Audio = Audio {audioFileId :: String
				  , audioDuration :: Integer
				  , audioPerformer :: Maybe String
				  , audioTitle :: Maybe String
				  , audioMimeType :: Maybe String
				  , audioFileSize :: Maybe Integer} deriving Show

instance FromJSON Audio where
	parseJSON (Object v) = Audio <$>
							v .: "file_id" <*>
							v .: "duration" <*>
							v .:? "performer" <*>
							v .:? "title" <*>
							v .:? "mime_type" <*>
							v .:? "file_size"	
	parseJSON _ = mempty						

data Document = Document {documentFileId :: String
					, documentThumb :: Maybe PhotoSize
					, documentMimeType :: Maybe String
					, documentFileName :: Maybe String
					, documentFileSize :: Maybe Integer} deriving Show

instance FromJSON Document where
	parseJSON (Object v) = Document <$>
							v .: "file_id" <*>
							v .:? "thumb" <*>
							v .:? "file_name" <*>
							v .:? "mime_type" <*>
							v .:? "file_size"	
	parseJSON _ = mempty	

data Sticker = Sticker {stickerFileId :: String
					  , stickerWidth :: Integer
					  , stickerHeight :: Integer
					  , stickerThumb :: Maybe PhotoSize
					  , stickerEmoji :: Maybe String
					  , stickerFileSize :: Maybe Integer} deriving Show

instance FromJSON Sticker where
	parseJSON (Object v) = Sticker <$>
							v .: "file_id" <*>
							v .: "width" <*>
							v .: "height" <*>
							v .:? "thumb" <*>
							v .:? "emoji" <*>
							v .:? "file_size"
	parseJSON _ = mempty							

data Video = Video {videoFileId :: String
				  , videoWidth :: Integer
				  , videoHeight :: Integer
				  , videoDuration :: Integer
				  , videoThumb :: Maybe PhotoSize
				  , videoMimeType :: Maybe String
				  , videoFileSize :: Maybe Integer} deriving Show

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

data Voice = Voice {voiceFileId :: String
				  , voiceDuration :: Integer
				  , voiceMimeType :: Maybe String
				  , voiceFileSize :: Maybe Integer} deriving Show

instance FromJSON Voice where
	parseJSON (Object v) = Voice <$>
							v .: "file_id" <*>
							v .: "duration" <*>
							v .:? "mime_type" <*>
							v .:? "file_size"
	parseJSON _ = mempty						

data Contact = Contact {contactPhoneNumber :: String
					  , contactFirstName :: String
					  , contactLastName :: Maybe String
					  , contactUserId :: Maybe Integer} deriving Show

instance FromJSON Contact where
	parseJSON (Object v) = Contact <$>
							v .: "phone_number" <*>
							v .: "first_name" <*>
							v .:? "last_name" <*>
							v .:? "user_id"
	parseJSON _ = mempty						

data Location = Location {locationLongitude :: Float
						, locationLatitude :: Float} deriving Show

instance FromJSON Location where
	parseJSON (Object v) = Location <$>
							v .: "longitude" <*>
							v .: "latitude"
	parseJSON _ = mempty						

data Venue = Venue {venueLocation :: Location
				  , venueTitle :: String
				  , venueAddress :: String
				  , venueFoursquareId :: Maybe String} deriving Show

instance FromJSON Venue where
	parseJSON (Object v) = Venue <$>
							v .: "location" <*>
							v .: "title" <*>
							v .: "address" <*>
							v .:? "foursquare_id"
	parseJSON _ = mempty						

data UserProfilePhotos = UserProfilePhotos {userProfilePhotosTotalCount :: Integer
										  , userProfilePHotosPhotos :: [[PhotoSize]]}	deriving Show												

instance FromJSON UserProfilePhotos where
	parseJSON (Object v) = UserProfilePhotos <$>
							v .: "total_count" <*>
							v .: "photos" 	
	parseJSON _ = mempty												

data File = File {fileFileId :: String
				, fileFileSize :: Maybe Integer
				, fileFilePath :: Maybe String} deriving Show								

instance FromJSON File where
	parseJSON (Object v) = File <$>
							v .: "file_id" <*>
							v .:? "file_size" <*>
							v .:? "file_path"	
	parseJSON _ = mempty						

data ReplyMarkup = IsRKM ReplyKeyboardMarkup
				 | IsRKR ReplyKeyboardRemove
				 | IsIKM InlineKeyboardMarkup
				 | IsFR ForceReply

instance ToJSON ReplyMarkup where
	toJSON x =
		case x of
			IsRKM n -> object ["reply_markup" .= n]
			IsRKR n -> object ["reply_markup" .= n]
			IsIKM n -> object ["reply_markup" .= n]
			IsFR n -> object ["reply_markup" .= n]

data ReplyKeyboardMarkup = ReplyKeyboardMarkup {rKMKeyboard :: [[KeyboardButton]]
											  , rKMResizeKeyboard :: Maybe Bool
											  , rKMOneTimeKeyboard :: Maybe Bool
											  , rKMSelective :: Maybe Bool} deriving Show

instance FromJSON ReplyKeyboardMarkup where
	parseJSON (Object v) = ReplyKeyboardMarkup <$>
							v .: "keyboard" <*>
							v .:? "resize_keybord" <*>
							v .:? "one_time_keyboard" <*>
							v .:? "selective"
	parseJSON _ = mempty

instance ToJSON ReplyKeyboardMarkup where
	toJSON (ReplyKeyboardMarkup keyboard resize_keybord one_time_keyboard selective) = 
		object $ ["keyboard" .= keyboard] 
			++ (addMaybeParam	"resize_keybord" resize_keybord)	
			++ (addMaybeParam "one_time_keyboard" one_time_keyboard)
			++ (addMaybeParam "selective" selective)


data KeyboardButton = KeyboardButton {keyboardButtonText :: String
									, keyboardButtonRequestContact :: Maybe Bool
									, keyboardButtonRequestLocation :: Maybe Bool} deriving Show

instance FromJSON KeyboardButton where
	parseJSON (Object v) = KeyboardButton <$>
							v .: "text" <*>
							v .:? "request_contact" <*>
							v .:? "request_location" 
	parseJSON _ = mempty	

instance ToJSON KeyboardButton where
	toJSON (KeyboardButton text request_contact request_location) =
		object $ ["text" .= text]
			++ (addMaybeParam "request_contact" request_contact)
			++ (addMaybeParam "request_location" request_location)																

data ReplyKeyboardRemove = ReplyKeyboardRemove {rKRKeyboard :: Bool
											, rKRSelective :: Maybe Bool} deriving Show

instance FromJSON ReplyKeyboardRemove where
	parseJSON (Object v) = ReplyKeyboardRemove <$>
							v .: "hide_keyboard" <*>
							v .:? "selective"	
	parseJSON _ = mempty							

instance ToJSON ReplyKeyboardRemove where
	toJSON (ReplyKeyboardRemove keyboard selective) = 
		object $ ["keyboard" .= keyboard] 
			++ (addMaybeParam "selective" selective)	

data InlineKeyboardMarkup = InlineKeyboardMarkup {iKMInlineKeyboard :: [[InlineKeyboardButton]]} deriving Show

instance FromJSON InlineKeyboardMarkup where
	parseJSON (Object v) = InlineKeyboardMarkup <$>
							v .: "inline_keyboard"
	parseJSON _ = mempty						

instance ToJSON InlineKeyboardMarkup where
	toJSON (InlineKeyboardMarkup inline_keyboard) =
		object ["inline_keyboard" .= inline_keyboard] 

data InlineKeyboardButton = InlineKeyboardButton {iKBText :: String
												, iKBUrl :: Maybe String
												, iKBCallbackData :: Maybe String
												, iKBSwitchInlineQuery :: Maybe String
												, iKBSwitchInlineQueryCC :: Maybe String
												, iKBCallbackGame :: Maybe CallbackGame} deriving Show

instance FromJSON InlineKeyboardButton where
	parseJSON (Object v) = InlineKeyboardButton <$>
							v .: "text" <*>
							v .:? "url" <*>
							v .:? "callback_data" <*>
							v .:? "switch_inline_query" <*>
							v .:? "switch_inline_query_current_chat" <*>
							v .:? "callback_game" 	
	parseJSON _ = mempty						

instance ToJSON InlineKeyboardButton where
	toJSON (InlineKeyboardButton text url callback_data switch_inline_query switch_inline_query_current_chat callback_game) = 
		object $ ["text" .= text]
		++ (addMaybeParam "url" url)
		++ (addMaybeParam "callback_data" callback_data)
		++ (addMaybeParam "switch_inline_query" switch_inline_query)
		++ (addMaybeParam "switch_inline_query_current_chat" switch_inline_query_current_chat)
		++ (addMaybeParam "callback_game" callback_game)


data CallbackQuery = CallbackQuery {callbackQueryId :: String
								  , callbackQueryFrom :: User
								  , callbackQueryMessage :: Maybe Message
								  , callbackQueryInlineMessageId :: Maybe String
								  , callbackQueryChatInstance :: String
								  , callbackQueryData :: String
								  , callbackQueryGameShortName :: Maybe String} deriving Show

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

data ForceReply = ForceReply {forceReplyForceReply :: Bool
							, forceReplySelective :: Maybe Bool} deriving Show											

instance FromJSON ForceReply where
	parseJSON (Object v) = ForceReply <$>
							v .: "force_reply" <*>
							v .:? "selective"
	parseJSON _ = mempty

instance ToJSON ForceReply where
	toJSON (ForceReply force_reply selective) =
		object $ ["force_reply" .= force_reply]
			++ (addMaybeParam "selective" selective) 							

data ChatMember = ChatMember {chatMemberUser :: User
							, chatMemberStatus :: String} deriving Show										

instance FromJSON ChatMember where
	parseJSON (Object v) = ChatMember <$>
							v .: "user" <*>
							v .: "status"
	parseJSON _ = mempty						

data ResponseParameters = ResponseParameters {responseParametersMigrateToChatId :: Maybe Integer
											, responseParametersRetryAfter :: Maybe Integer} deriving Show			

instance FromJSON ResponseParameters where
	parseJSON (Object v) = ResponseParameters <$>
							v .:? "migrate_to_chat_id"<*>
							v .:? "retry_after"
	parseJSON _ = mempty						

data Game = Game {gameTitle :: String
				, gameDescription :: String
				, gamePhoto :: [PhotoSize]
				, gameText :: Maybe String
				, gameTextEntities :: Maybe [MessageEntity]
				, gameAnimation :: Maybe Animation} deriving Show

instance FromJSON Game where
	parseJSON (Object v) = Game <$>
							v .: "title" <*>
							v .: "description" <*>
							v .: "photo" <*>
							v .:? "text" <*>
							v .:? "text_entities" <*>
							v .:? "animation"
	parseJSON _ = mempty						

data Animation = Animation {animationFileId :: String
						  , animationThumb :: Maybe PhotoSize
						  , animationFileName :: Maybe String
						  , animationMimeType :: Maybe String
						  , animationFileSize :: Maybe Integer} deriving Show

instance FromJSON Animation where
	parseJSON (Object v) = Animation <$>
							v .: "file_id" <*>
							v .:? "thumb" <*>
							v .:? "file_name" <*>
							v .:? "mime_type" <*>
							v .:? "file_size"
	parseJSON _ = mempty						

data CallbackGame = CallbackGame deriving (Show, Generic)

instance FromJSON CallbackGame

instance ToJSON CallbackGame	

data GameHighScore = GameHighScore {gameHighScorePosition :: Integer
								  , gameHighScoreUser :: User
								  , gameHighScoreScore :: Integer} deriving Show

instance FromJSON GameHighScore where
	parseJSON (Object v) = GameHighScore <$>
							 v .: "position" <*>
							 v .: "user" <*>
							 v .: "score"
	parseJSON _ = mempty						 
