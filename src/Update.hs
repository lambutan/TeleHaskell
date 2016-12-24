{-# LANGUAGE OverloadedStrings #-}

module Update where

import Aux_Types
import API_Types
import API_Types_JSON
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Network.HTTP.Simple (httpJSON, getResponseBody, parseRequest, setRequestBodyJSON)

data GetUpdatesBody = GetUpdatesBody {getUpdatesOffset :: Maybe Integer
									, getUpdatesLimit :: Maybe Integer
									, getUpdatesTimeout :: Maybe Integer
									, getUpdatesAllowedUpdates :: Maybe [String]}

instance ToJSON GetUpdatesBody where
	toJSON (GetUpdatesBody offset limit timeout allowed_updates) =
	 object $ (addMaybeParam "offset" offset) 
	 	   ++ (addMaybeParam "limit" limit)
	 	   ++ (addMaybeParam "timeout" timeout) 
	 	   ++ (addMaybeParam "allowed_updates" allowed_updates)

getUpdates :: Bot -> GetUpdatesBody -> IO (Package [Update])
getUpdates bot body = do
	initReq <- parseRequest $ "POST " ++ apiTelegram ++ token bot ++ "/getUpdates"
	response <- httpJSON $ setRequestBodyJSON body initReq
	return $ getResponseBody response


-- setWebhook ::

-- deleteWebhook :: Bot -> IO Bool

-- getWebhookInfo ::						
