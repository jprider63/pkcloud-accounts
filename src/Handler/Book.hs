module Handler.Book where

import Import

postBookR :: Handler Value
postBookR = do
    book <- (requireJsonBody :: Handler Book)
    maybeCurrentUserId <- requireAuthId
    now <- lift getCurrentTime
    let book' = book { bookCreatedBy = maybeCurrentUserId
                     , bookDateCreated = now }
    insertedBook <- runDB $ insertEntity book'
    returnJson insertedBook