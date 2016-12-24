{-# LANGUAGE DeriveGeneric #-}

module API_Types where

import           GHC.Generics

-- Types defined in the API

data Update = Update {updateId :: Integer
					, updateMessage :: Maybe Message
					, updateEditedMessage :: Maybe Message
					, updateChannelPost :: Maybe Message
					, updateEditedChannelPost :: Maybe Message
					, updateCallbackQuery :: Maybe CallbackQuery} deriving Show

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

data Chat = Chat {chatId :: Integer
				, chatType :: String
				, chatTitle :: Maybe String
				, chatUserName :: Maybe String
				, chatFirstName :: Maybe String
				, chatLastName :: Maybe String
				, chatAllAdmins :: Maybe Bool} deriving Show

data Message = Message {messageId :: Integer
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

data MessageEntity = MessageEntity {messageEntityType :: String
								  , messageEntityOffset :: Integer
								  , messageEntityLength :: Integer
								  , messageEntityUrl :: Maybe String
								  , messageEntityUser :: Maybe User} deriving Show

data PhotoSize = PhotoSize {photoSizeFileId :: String
						  , photoSizeWidth :: Integer
						  , photoSizeHeight :: Integer
						  , photoSizeFileSize :: Maybe Integer} deriving Show

data Audio = Audio {audioFileId :: String
				  , audioDuration :: Integer
				  , audioPerformer :: Maybe String
				  , audioTitle :: Maybe String
				  , audioMimeType :: Maybe String
				  , audioFileSize :: Maybe Integer} deriving Show

data Document = Document {documentFileId :: String
					, documentThumb :: Maybe PhotoSize
					, documentMimeType :: Maybe String
					, documentFileName :: Maybe String
					, documentFileSize :: Maybe Integer} deriving Show

data Sticker = Sticker {stickerFileId :: String
					  , stickerWidth :: Integer
					  , stickerHeight :: Integer
					  , stickerThumb :: Maybe PhotoSize
					  , stickerEmoji :: Maybe String
					  , stickerFileSize :: Maybe Integer} deriving Show

data Video = Video {videoFileId :: String
				  , videoWidth :: Integer
				  , videoHeight :: Integer
				  , videoDuration :: Integer
				  , videoThumb :: Maybe PhotoSize
				  , videoMimeType :: Maybe String
				  , videoFileSize :: Maybe Integer} deriving Show

data Voice = Voice {voiceFileId :: String
				  , voiceDuration :: Integer
				  , voiceMimeType :: Maybe String
				  , voiceFileSize :: Maybe Integer} deriving Show

data Contact = Contact {contactPhoneNumber :: String
					  , contactFirstName :: String
					  , contactLastName :: Maybe String
					  , contactUserId :: Maybe Integer} deriving Show

data Location = Location {locationLongitude :: Float
						, locationLatitude :: Float} deriving Show

data Venue = Venue {venueLocation :: Location
				  , venueTitle :: String
				  , venueAddress :: String
				  , venueFoursquareId :: Maybe String} deriving Show

data UserProfilePhotos = UserProfilePhotos {userProfilePhotosTotalCount :: Integer
										  , userProfilePHotosPhotos :: [[PhotoSize]]}	deriving Show												

data File = File {fileFileId :: String
				, fileFileSize :: Maybe Integer
				, fileFilePath :: Maybe String} deriving Show								

data ReplyMarkup = IsRKM ReplyKeyboardMarkup
				 | IsRKR ReplyKeyboardRemove
				 | IsIKM InlineKeyboardMarkup
				 | IsFR ForceReply

data ReplyKeyboardMarkup = ReplyKeyboardMarkup {rKMKeyboard :: [[KeyboardButton]]
											  , rKMResizeKeyboard :: Maybe Bool
											  , rKMOneTimeKeyboard :: Maybe Bool
											  , rKMSelective :: Maybe Bool} deriving Show

data KeyboardButton = KeyboardButton {keyboardButtonText :: String
									, keyboardButtonRequestContact :: Maybe Bool
									, keyboardButtonRequestLocation :: Maybe Bool} deriving Show

data ReplyKeyboardRemove = ReplyKeyboardRemove {rKRKeyboard :: Bool
											, rKRSelective :: Maybe Bool} deriving Show

data InlineKeyboardMarkup = InlineKeyboardMarkup {iKMInlineKeyboard :: [[InlineKeyboardButton]]} deriving Show

data InlineKeyboardButton = InlineKeyboardButton {iKBText :: String
												, iKBUrl :: Maybe String
												, iKBCallbackData :: Maybe String
												, iKBSwitchInlineQuery :: Maybe String
												, iKBSwitchInlineQueryCC :: Maybe String
												, iKBCallbackGame :: Maybe CallbackGame} deriving Show

data CallbackQuery = CallbackQuery {callbackQueryId :: String
								  , callbackQueryFrom :: User
								  , callbackQueryMessage :: Maybe Message
								  , callbackQueryInlineMessageId :: Maybe String
								  , callbackQueryChatInstance :: String
								  , callbackQueryData :: String
								  , callbackQueryGameShortName :: Maybe String} deriving Show

data ForceReply = ForceReply {forceReplyForceReply :: Bool
							, forceReplySelective :: Maybe Bool} deriving Show											

data ChatMember = ChatMember {chatMemberUser :: User
							, chatMemberStatus :: String} deriving Show										

data ResponseParameters = ResponseParameters {responseParametersMigrateToChatId :: Maybe Integer
											, responseParametersRetryAfter :: Maybe Integer} deriving Show			

data Game = Game {gameTitle :: String
				, gameDescription :: String
				, gamePhoto :: [PhotoSize]
				, gameText :: Maybe String
				, gameTextEntities :: Maybe [MessageEntity]
				, gameAnimation :: Maybe Animation} deriving Show

data Animation = Animation {animationFileId :: String
						  , animationThumb :: Maybe PhotoSize
						  , animationFileName :: Maybe String
						  , animationMimeType :: Maybe String
						  , animationFileSize :: Maybe Integer} deriving Show

data CallbackGame = CallbackGame deriving (Show, Generic)

data GameHighScore = GameHighScore {gameHighScorePosition :: Integer
								  , gameHighScoreUser :: User
								  , gameHighScoreScore :: Integer} deriving Show
