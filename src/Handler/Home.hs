module Handler.Home where

import Import
import Handler.YoutubeSearch

{-

Yesod follows a naming convention for handler function names: the lower-cased
HTTP request method, followed by the route name. Therefore, the function for
HomeR's GET handler would be getHomeR.

Each handler function lives in the Handler monad, and has to return a value
that can be serialized over an HTTP connection. Two common examples of such
values are HTML and JSON data. In this case, we'll return Html.

-}
getHomeR :: Handler Html
-- defaultLayout uses the application's standard page layout to display
-- some contents. In our application, we're just using the standard
-- layout, which includes a basic HTML 5 page outline.
getHomeR = getYoutubeSearchR "haskell"