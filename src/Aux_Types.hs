module Aux_Types where

import Data.Text
import Data.Aeson
import Data.Aeson.Types (Pair)

-- String for the first part of the Telegram API URL
apiTelegram = "https://api.telegram.org/bot"

newtype Bot = Bot {token :: String} deriving Show

data ChatId = IsInt Integer
			| IsString String

data Package a = Package {getStatus :: Bool
						, getResult :: a} deriving Show	

addMaybeParam :: (ToJSON n) => Text -> Maybe n -> [Pair]
addMaybeParam txt mbn =
	case mbn of
		Just n -> [txt .= n]
		_ -> []						

			
