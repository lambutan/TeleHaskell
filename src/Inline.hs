module Inline where

import API_Types

data InlineQuery = InlineQuery {iqId :: String
							  , iqFrom :: User
							  , iqLocation :: Maybe Location
							  , iqQuery :: String
							  , iqOffset :: String}


data InlineQueryResult = IsIQRCachedAudio IQRCachedAudio
					   | IsIQRCachedDocument IQRCachedDocument
					   | IsIQRCachedGif IQRCachedGif
					   | IsIQRCachedMpeg4Gif IQRCachedMpeg4Gif
					   | IsIQRCachedPhoto IQRCachedPhoto
					   | IsIQRCachedSticker IQRCachedSticker
					   | IsIQRCachedVideo IQRCachedVideo
					   | IsIQRCachedVoice IQRCachedVoice
					   | IsIQRArticle IQRArticle
					   | IsIQRAudio IQRAudio
					   | IsIQRContact IQRContact
					   | IsIQRGame IQRGame
					   | IsIQRDocument IQRDocument
					   | IsIQRGif IQRGif
					   | IsIQRLocation IQRLocation
					   | IsIQRMpeg4Gif IQRMpeg4Gif
					   | IsIQRPhoto IQRPhoto
					   | IsIQRVenue IQRVenue
					   | IsIQRVideo IQRVideo
					   | IsIQRVoice IQRVoice			

data IQRCachedAudio = IQRCachedAudio {iqrCachedAudioType :: String
					, iqrCachedAudioId :: String
					, iqrCachedAudioAudioFileId :: String
					, iqrCachedAudioCaption :: Maybe String
					, iqrCachedAudioReplyMarkup :: Maybe InlineKeyboardMarkup
					, iqrCachedAudioInputMessageContent :: Maybe InputMessageContent}

data IQRCachedDocument = IQRCachedDocument {iqrCachedDocumentType :: String
							  , iqrCachedDocumentId :: String
							  , iqrCachedDocumentTitle :: String
							  , iqrCachedDocumentDocumentFileId :: String
							  , iqrCachedDocumentDescription :: Maybe String
							  , iqrCachedDocumentCaption :: Maybe String
							  , iqrCachedDocumentReplyMarkup :: Maybe InlineKeyboardMarkup
							  , iqrCachedDocumentInputMessageContent :: Maybe InputMessageContent}

data IQRCachedGif = IQRCachedGif {iqrCachedGifType :: String
					, iqrCachedGifId :: String
					, iqrGifFileId :: String
					, iqrCachedGifTitle :: Maybe String
					, iqrCachedGifCaption :: Maybe String
					, iqrCachedGifReplyMarkup :: Maybe InlineKeyboardMarkup
					, iqrCachedGifInputMessageContent :: Maybe InputMessageContent}

data IQRCachedMpeg4Gif = IQRCachedMpeg4Gif {iqrCachedMpeg4GifType :: String
							  , iqrCachedMpeg4GifId :: String
							  , iqrCachedMpeg4GifMpeg4FileId :: String
							  , iqrCachedMpeg4GifTitle :: Maybe String
							  , iqrCachedMpeg4GifCaption :: Maybe String
							  , iqrCachedMpeg4GifReplyMarkup :: Maybe InlineKeyboardMarkup
							  , iqrCachedMpeg4GifInputMessageContent :: Maybe InputMessageContent}

data IQRCachedPhoto = IQRCachedPhoto {iqrCachedPhotoType :: String
						, iqrCachedPhotoId :: String
						, iqrCachedPhotoPhotoFileId :: String
						, iqrCachedPhotoTitle :: Maybe String
						, iqrCachedPhotoDescription :: Maybe String
						, iqrCachedPhotoCaption :: Maybe String
						, iqrCachedPhotoReplyMarkup :: Maybe InlineKeyboardMarkup
						, iqrCachedPhotoInputMessageContent :: Maybe InputMessageContent}

data IQRCachedSticker = IQRCachedSticker {iqrCachedStickerType :: String
										, iqrCachedStickerId :: String
										, iqrCachedStickerStickerFileId :: String
										, iqrCachedStickerReplyMarkup :: Maybe InlineKeyboardMarkup
										, iqrCachedStickerInputMessageContent :: Maybe InputMessageContent}

data IQRCachedVideo = IQRCachedVideo {iqrCachedVideoType :: String
						, iqrCachedVideoId :: String
						, iqrCachedVideoVideoFileId :: String
						, iqrCachedVideoTitle :: String
						, iqrCachedVideoDescription :: Maybe String
						, iqrCachedVideoCaption :: Maybe String
						, iqrCachedVideoReplyMarkup :: Maybe InlineKeyboardMarkup
						, iqrCachedVideoMessageContent :: Maybe InputMessageContent}

data IQRCachedVoice = IQRCachedVoice {iqrCachedVoiceType :: String
						, iqrCachedVoiceId :: String
						, iqrCachedVoiceVoiceFileId :: String
						, iqrCachedVoiceTitle :: String
						, iqrCachedVoiceCaption :: Maybe String
						, iqrCachedVoiceReplyMarkup :: Maybe InlineKeyboardMarkup
						, iqrCachedVoiceInputMessageContent :: Maybe InputMessageContent}	

data IQRArticle = IQRArticle {iqrArticleType :: String
							, iqrArticleId :: String
							, iqrArticleTitle :: String
							, iqrArticleInputMessageContent :: InputMessageContent
							, iqrArticleReplyMarkup :: Maybe InlineKeyboardMarkup
							, iqrArticleUrl :: Maybe String
							, iqrArticleHideUrl :: Maybe Bool
							, iqrArticleDescription :: Maybe String
							, iqrArticleThumbUrl :: Maybe String
							, iqrArticleThumbWidth :: Maybe Integer
							, iqrArticleThumbHeight :: Maybe Integer}

data IQRAudio = IQRAudio {iqrAudioType :: String
					, iqrAudioId :: String
					, iqrAudioAudioUrl :: String
					, iqrAudioTitle :: String
					, iqrAudioCaption :: Maybe String
					, iqrAudioPerformer :: Maybe String
					, iqrAudioAudioDuration :: Maybe Integer
					, iqrAudioReplyMarkup :: Maybe InlineKeyboardMarkup
					, iqrAudioInputMessageContent :: Maybe InputMessageContent}

data IQRContact = IQRContact {iqrContactType :: String
							, iqrContactId :: String
							, iqrContactPhoneNumber :: String
							, iqrContactFirstName :: String
							, iqrContactLastName :: Maybe String
							, iqrContactReplyMarkup :: Maybe InlineKeyboardMarkup
							, iqrContactInputMessageContent :: Maybe InputMessageContent
							, iqrContactThumbUrl :: Maybe String
							, iqrContactThumbWidth :: Maybe Integer
							, iqrContactThumbHeight :: Maybe Integer}

data IQRGame = IQRGame {iqrGameType :: String
					  , iqrGameId :: String
					  , iqrGameGameShortName :: String
					  , iqrGameReplyMarkup :: Maybe InlineKeyboardMarkup}

data IQRDocument = IQRDocument {iqrDocumentType :: String
							  , iqrDocumentId :: String
							  , iqrDocumentTitle :: String
							  , iqrDocumentCaption :: Maybe String
							  , iqrDocumentDocumentUrl :: String
							  , iqrDocumentMimeType :: String
							  , iqrDocumentDescription :: Maybe String
							  , iqrDocumentReplyMarkup :: Maybe InlineKeyboardMarkup
							  , iqrDocumentInputMessageContent :: Maybe InputMessageContent
							  , iqrDocumentThumbUrl :: Maybe String
							  , iqrDocumentThumbWidth :: Maybe Integer
							  , iqrDocumentThumbHeight :: Maybe Integer}

data IQRGif = IQRGif {iqrGifType :: String
					, iqrGifId :: String
					, iqrGifGifUrl :: String
					, iqrGifGifWidth :: Maybe Integer
					, iqrGifGifHeight :: Maybe Integer
					, iqrGifThumbUrl :: String
					, iqrGifTitle :: Maybe String
					, iqrGifCaption :: Maybe String
					, iqrGifReplyMarkup :: Maybe InlineKeyboardMarkup
					, iqrGifInputMessageContent :: Maybe InputMessageContent}

data IQRLocation = IQRLocation {iqrLocationType :: String
						, iqrLocationId :: String
						, iqrLocationLatitude :: Float
						, iqrLocationLongitude :: Float
						, iqrLocationTitle :: String
						, iqrLocationReplyMarkup :: Maybe InlineKeyboardMarkup
						, iqrLocationInputMessageContent :: Maybe InputMessageContent
						, iqrLocationThumbUrl :: Maybe String
						, iqrLocationThumbWidth :: Maybe Integer
						, iqrLocationThumbHeight :: Maybe Integer}

data IQRMpeg4Gif = IQRMpeg4Gif {iqrMpeg4GifType :: String
							  , iqrMpeg4GifId :: String
							  , iqrMpeg4GifMpeg4Url :: String
							  , iqrMpeg4GifMpeg4Width :: Maybe Integer
							  , iqrMpeg4GifMpegHeight :: Maybe Integer
							  , iqrMpeg4GifThumbUrl :: String
							  , iqrMpeg4GifTitle :: Maybe String
							  , iqrMpeg4GifCaption :: Maybe String
							  , iqrMpeg4GifReplyMarkup :: Maybe InlineKeyboardMarkup
							  , iqrMpeg4GifInputMessageContent :: Maybe InputMessageContent}

data IQRPhoto = IQRPhoto {iqrPhotoType :: String
						, iqrPhotoId :: String
						, iqrPhotoPhotoUrl :: String
						, iqrPhotoThumbUrl ::  String
						, iqrPhotoPhotoWidth :: Maybe Integer
						, iqrPhotoPhotoHeight :: Maybe Integer
						, iqrPhotoTitle :: Maybe String
						, iqrPhotoDescription :: Maybe String
						, iqrPhotoCaption :: Maybe String
						, iqrPhotoReplyMarkup :: Maybe InlineKeyboardMarkup
						, iqrPhotoInputMessageContent :: Maybe InputMessageContent}
					
data IQRVenue = IQRVenue {iqrVenueType :: String
						, iqrVenueId :: String
						, iqrVenueLatitude :: Float
						, iqrVenueLongitude :: Float
						, iqrVenueTitle :: String
						, iqrVenueAddress :: String
						, iqrVenueFoursquareId :: Maybe String
						, iqrVenueReplyMarkup :: Maybe InlineKeyboardMarkup
						, iqrVenueInputMessageContent :: Maybe InputMessageContent
						, iqrVenueThumbUrl :: Maybe String
						, iqrVenueThumbWidth :: Maybe Integer
						, iqrVenueThumbHeight :: Maybe Integer}

data IQRVideo = IQRVideo {iqrVideoType :: String
						, iqrVideoId :: String
						, iqrVideoVideoUrl :: String
						, iqrVideoMimeType :: String
						, iqrVideoThumpUrl :: String
						, iqrVideoTitle :: String
						, iqrVideoCaption :: Maybe String
						, iqrVideoVideoWidth :: Maybe Integer
						, iqrVideoVideoHeight :: Maybe Integer
						, iqrVideoVideoDuration :: Maybe Integer
						, iqrVideoDescription :: Maybe String
						, iqrVideoReplyMarkup :: Maybe InlineKeyboardMarkup
						, iqrVideoMessageContent :: Maybe InputMessageContent}

data IQRVoice = IQRVoice {iqrVoiceType :: String
						, iqrVoiceId :: String
						, iqrVoiceVoiceUrl :: String
						, iqrVoiceTitle :: String
						, iqrVoiceCaption :: Maybe String
						, iqrVoiceVoiceDuration :: Maybe Integer
						, iqrVoiceReplyMarkup :: Maybe InlineKeyboardMarkup
						, iqrVoiceInputMessageContent :: Maybe InputMessageContent}	

data InputMessageContent = IsITextMC ITextMC
						 | IsILocationMC ILocationMC
						 | IsIVenueMC IVenueMC
						 | IsIContactMC IContactMC

data ITextMC = ITextMC{imcTextMessageText :: String
					 , imcTextParseMode :: Maybe String
					 , imcTextDisableWebPagePreview :: Maybe Bool}

data ILocationMC = ILocationMC{imcLocationLatitude :: Float
					 , imcLocationLongitude :: Float}

data IVenueMC = IVenueMC{imcVenueLatitude :: Float
					 , imcVenueLongitude :: Float
					 , imcVenueTitle :: String
					 , imcVenueAddress :: String
					 , imcVenueFoursquareId :: Maybe String}

data IContactMC = IContactMC {imcContactPhoneNumber :: String
							, imcContactFirstName :: String
							, imcContactLastName :: Maybe String}

data ChosenInlineResult = ChosenInlineResult {cirResultId :: String
											, cirFrom :: User
											, cirLocation :: Maybe Location
											, cirInlineMessageId :: Maybe String
											, cirQuery :: String}