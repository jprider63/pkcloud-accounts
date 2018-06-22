module Book where

import Import

canViewBook :: UserId -> Book -> Bool
canViewBook uId book = bookCreatedBy book == uId

layout :: (Entity Book -> Widget) -> BookId -> Handler Html
layout w bookId = do
    -- Check if user is authenticated.
    uId <- requireAuthId
    
    -- Lookup book.
    book <- runDB $ get404 bookId

    -- Check if user can view book.
    unless (canViewBook uId book) $ 
        permissionDenied ""

    defaultLayout $ w $ Entity bookId book


