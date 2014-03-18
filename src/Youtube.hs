module Youtube where

import Import hiding (urlField)
import qualified Data.Text as T    
import Data.Aeson
import Data.Aeson.TH
import Control.Monad
import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Conduit

searchURL :: String -> String
searchURL searchTerm = "https://www.googleapis.com/youtube/v3/search?type=video&fields=items(id(videoId),snippet(title,thumbnails(default)))&part=snippet&key=AIzaSyBmCL2WaqD8xECqVB15JzSBIqKKhx6lTHU&q=" ++ searchTerm

data ThumbnailUrl = ThumbnailUrl{urlField :: T.Text} deriving (Show)
data Thumbnails = Thumbnails{defaultField :: ThumbnailUrl} deriving (Show)
data Snippet = Snippet{titleField :: T.Text, thumbnailsField :: Thumbnails} deriving (Show)
data VideoId = VideoId{videoIdField :: T.Text} deriving (Show)
data Video = Video{idField :: VideoId, snippetField :: Snippet} deriving (Show)
data SearchResult = SearchResult{itemsField :: [Video]} deriving (Show)

$(deriveJSON (reverse . drop 5 . reverse) ''ThumbnailUrl)
$(deriveJSON (reverse . drop 5 . reverse) ''Thumbnails)
$(deriveJSON (reverse . drop 5 . reverse) ''Snippet)
$(deriveJSON (reverse . drop 5 . reverse) ''VideoId)
$(deriveJSON (reverse . drop 5 . reverse) ''Video)
$(deriveJSON (reverse . drop 5 . reverse) ''SearchResult)

searchYoutube :: (MonadIO m, Functor m) => String -> m (Maybe SearchResult)
searchYoutube searchTerm = fmap (decode' :: LB.ByteString -> Maybe SearchResult) $ simpleHttp $ searchURL searchTerm