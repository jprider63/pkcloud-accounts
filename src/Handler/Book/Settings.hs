module Handler.Book.Settings where

import qualified Book
import Import

getBookSettingsR :: BookId -> Handler Html
getBookSettingsR = Book.layout $ \(Entity bookId book) accountTree -> do
    [whamlet|
        TODO
    |]
