module Handler.Account.Edit where

import qualified Account
import qualified Book
import Handler.Account.Create (FormData(..), renderForm)
import Import

generateHTML :: BookId -> Entity Account -> Bool -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId (Entity accountId account) isDebit trees formM = do
    setTitle $ toHtml ("Edit Account" :: Text)

    (formW, enctype) <- handlerToWidget $ maybe (generateFormPost $ renderForm trees (checkParentField isDebit trees) $ Just account) return formM


    [whamlet|
        <h2>
            Edit Account
        <form method=post action=@{AccountEditR bookId accountId} enctype=#{enctype}>
            ^{formW}
            <div .form-group>
                <a class="btn btn-default" href="@{AccountR bookId accountId}">
                    Cancel
                <button type="submit" class="btn btn-primary">
                    Edit Account
    |]

checkParentField :: Monad m => Bool -> [AccountTree] -> Field m FolderAccountId -> Field m FolderAccountId
checkParentField isDebit trees = check $ \fId ->
    -- Require folder to be in book.
    case Account.getFolderNode trees fId of
        Nothing ->
            Left ("Invalid parent folder." :: Text)
        Just folderNode -> 
            -- Make sure parent is same type of folder.
            if folderNodeIsDebit folderNode /= isDebit then
                Left "Invalid parent account type."
            else
                Right fId


getAccountEditR :: Key Book -> Key Account -> Handler Html
getAccountEditR = Account.layout $ \(Entity bookId _) accountE isDebit accountTree -> 
    generateHTML bookId accountE isDebit accountTree Nothing

postAccountEditR :: Key Book -> Key Account -> Handler Html
postAccountEditR = Account.layout $ \(Entity bookId book) accountE@(Entity accountId account) isDebit accountTree -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    ((res, formW), formE) <- handlerToWidget $ runFormPost $ renderForm accountTree (checkParentField isDebit accountTree) $ Just account
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Editing account failed."
            generateHTML bookId accountE isDebit accountTree $ Just (formW, formE)
        FormFailure _msg -> do
            pkcloudSetMessageDanger "Editing account failed."
            generateHTML bookId accountE isDebit accountTree $ Just (formW, formE)
        FormSuccess (FormData name parentId shadowAccountId featured) -> do
            -- Check that shadow account is in this book.
            maybe (return ()) (Account.requireInBook accountTree) shadowAccountId

            -- Update account.
            handlerToWidget $ runDB $ update accountId [
                  AccountName =. name
                , AccountParent =. parentId
                , AccountFeatured =. featured
                , AccountShadow =. shadowAccountId
                ]

            -- Set message.
            pkcloudSetMessageSuccess "Successfully edited account!"

            -- Redirect
            redirect $ AccountR bookId accountId

