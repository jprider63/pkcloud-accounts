module Handler.Book where

import qualified Book
import Import

-- JP: I'd probably move this to Handler.Book.Create or something.
postBookCreateR :: Handler Value
postBookCreateR = do
    currentUserId <- requireAuthId

    -- Insert book.
    book <- (requireJsonBody :: Handler Book)
    now <- lift getCurrentTime
    let book' = book { bookCreatedBy = currentUserId
                     , bookDateCreated = now }
    insertedBook <- runDB $ do
        insertedBook@(Entity bId _) <- insertEntity book'

        -- Insert default accounts.
        assetsId <- insert $ FolderAccount "Assets" Nothing False
        insert_ $ BookFolderAccount bId assetsId

        liabilitiesId <- insert $ FolderAccount "Liabilities" Nothing True
        insert_ $ BookFolderAccount bId liabilitiesId

        equityId <- insert $ FolderAccount "Equity" Nothing True
        insert_ $ BookFolderAccount bId equityId

        return insertedBook

    returnJson insertedBook

--JP: Change BookId to a unique BookUrl?
getBookR :: BookId -> Handler Html
getBookR = Book.layout $ \(Entity bookId book) -> do
    setTitle $ toHtml $ bookName book
    [whamlet|
        <h1>
            #{bookName book}
    |]
