module Handler.YoutubeSearch where

import Import hiding (urlField)
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Monoid
import Network.HTTP.Conduit
import Control.Monad.Trans.Resource
import Text.Julius
import Youtube

searchForm :: T.Text -> Html -> MForm Handler (FormResult T.Text, Widget)
searchForm searchText extra = do
    searchFormId <- newIdent
    let searchFieldSettings = FieldSettings{fsLabel = "Search", fsId = Just searchFormId, fsTooltip = Nothing, fsName = Nothing, fsAttrs = []}
    (searchResult, searchField) <- mreq textField searchFieldSettings $ Just searchText
    let widget = do
            toWidget [julius|
                function updateResults() {
                    $.post("/youtube/" + $("##{rawJS searchFormId}").val(), function(data) {
                        $("#result-container").replaceWith(data);
                    });
                }
            |]
            [whamlet|
                ^{extra}
                <form>
                     ^{fvLabel searchField}^{fvInput searchField}
                    <button onclick="updateResults();return false;">Search
            |]
    return (searchResult, widget)

resultToHtml :: Maybe SearchResult -> HtmlUrl (Route App)
resultToHtml (Just searchResult) = [hamlet|
                <div .container-fluid .result-container #result-container>
                    $forall video <- itemsField searchResult
                        <a href="http://www.youtube.com/watch?v=#{videoIdField $ idField video}">
                            <div .row .result-row>
                                <div .col-md-2>
                                    <img src=#{urlField $ defaultField $ thumbnailsField $ snippetField video}>
                                <div .col-md-4 .col-md-offset-1 .text-left>
                                    #{titleField $ snippetField video}
             |]
resultToHtml Nothing = [hamlet|
                <div .container-fluid>
                    <div .row>
                        <div .col-md-3 .col-md-offset-1>No Result Bro
             |]

postYoutubeSearchR :: T.Text -> Handler Html
postYoutubeSearchR searchTerm = do
    render <- getUrlRenderParams
    result <- fmap resultToHtml (searchYoutube (T.unpack searchTerm))
    return (result render)

getYoutubeSearchR :: T.Text -> Handler Html
getYoutubeSearchR searchTerm = do
    ((result, widget), enctype) <- runFormPost $ searchForm searchTerm
    
    defaultLayout $ do
        setTitle "Youtube Viewer"

        addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"
        addScriptRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"
        addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"

        possibleSearchResult <- searchYoutube $ T.unpack searchTerm
        $(logInfo) $ (T.pack . show) possibleSearchResult

        let innerContent = resultToHtml possibleSearchResult

        toWidget [cassius|
            div.result-container
                div.result-row
                    padding: 10px
                    background-color: #eee
                    border: 1px solid #ddd
                    border-radius: 10px
                    background-color: rgba(86,61,124,.15)
                    border: 1px solid rgba(86,61,124,.2)
        |]

        [whamlet|
            <div .container-fluid .col-md-8 .col-md-offset-2>
                <div .row-fluid>
                    <div>
                        ^{widget}
                <div .row-fluid>
                    <div> 
                        <h3>Result:
                        ^{innerContent}
        |]