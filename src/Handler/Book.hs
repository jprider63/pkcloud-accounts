module Handler.Book where

import qualified Book
import Import

-- JP: I'd probably move this to Handler.Book.Create or something.
postBookCreateR :: Handler Value
postBookCreateR = do
    book <- (requireJsonBody :: Handler Book)
    maybeCurrentUserId <- requireAuthId
    now <- lift getCurrentTime
    let book' = book { bookCreatedBy = maybeCurrentUserId
                     , bookDateCreated = now }
    insertedBook <- runDB $ insertEntity book'
    returnJson insertedBook

--JP: Change BookId to a unique BookUrl?
getBookR :: BookId -> Handler Html
getBookR = Book.layout $ \(Entity bookId book) -> do
    setTitle $ toHtml $ bookName book
    [whamlet|
        <h1>
            #{bookName book}
    |]
