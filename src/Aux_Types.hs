{-# LANGUAGE OverloadedStrings #-}
module Aux_Types where

import Data.Text
import Data.Aeson
import Data.Aeson.Types (Pair)

-- String for the first part of the Telegram API URL
apiTelegram = "https://api.telegram.org/bot"

newtype Bot = Bot {token :: String} deriving Show



data ChatId = IsInt Integer
			| IsString String

instance ToJSON ChatId where
	toJSON x =
		case x of
			IsInt n -> String $ Data.Text.pack $ show n
			IsString n -> String $ Data.Text.pack n			

data Package a = Package {getStatus :: Bool
						, getResult :: a} deriving Show	

instance (FromJSON n) => FromJSON (Package n) where
	parseJSON (Object v) = Package <$>
							v .: "ok" <*>
							v .: "result" 
	parseJSON _ = mempty						

addMaybeParam :: (ToJSON n) => Text -> Maybe n -> [Pair]
addMaybeParam txt mbn =
	case mbn of
		Just n -> [txt .= n]
		_ -> []						

			
