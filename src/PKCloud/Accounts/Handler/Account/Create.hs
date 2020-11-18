module PKCloud.Accounts.Handler.Account.Create where

import qualified Account
import qualified Book
import qualified Breadcrumb
import qualified Folder
import Import

data FormData = FormData {
      formDataAccount :: Text
    , formDataParent :: FolderAccountId
    , formDataShadow :: Maybe AccountId
    , formDataFeatured :: Bool
    } 

renderForm trees parentF accountM = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> areq textField accountSettings (accountName <$> accountM)
    <*> areq (parentF $ selectFieldKeys folders) parentSettings (accountParent <$> accountM)
    <*> aopt (selectFieldKeys accounts) shadowSettings (accountShadow <$> accountM)
    <*> areq checkBoxField featuredSettings (accountFeatured <$> accountM) -- TODO: Switch to bootstrapCheckBoxField XXX

    where
        accountSettings = withPlaceholder "Account" $ bfs ("Account Name" :: Text)
        parentSettings = bfs ("Folder" :: Text)
        shadowSettings = bfs ("Shadow Account" :: Text)
        featuredSettings = bfs ("Featured" :: Text)

        folders = Folder.treesToFolders trees
        accounts = Folder.treesToAccounts trees -- TODO: Combine these.

generateHTML :: BookId -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId trees formM = do
    setTitle $ toHtml ("New Account" :: Text)

    (formW, enctype) <- handlerToWidget $ maybe (generateFormPost $ renderForm trees id Nothing) return formM

    --     ^{msgH}
    [whamlet|
        <h2>
            New Account
        <form method=post action=@{AccountCreateR bookId} enctype=#{enctype}>
            ^{formW}
            <div .form-group>
                <button type="submit" class="btn btn-default">
                    Create Account
    |]

getAccountCreateR :: BookId -> Handler Html
getAccountCreateR = Book.layout (Breadcrumb.Account Breadcrumb.Create) $ \(Entity bookId book) accountTree -> do
    generateHTML bookId accountTree Nothing

postAccountCreateR :: BookId -> Handler Html
postAccountCreateR = Book.layout (Breadcrumb.Account Breadcrumb.Create) $ \(Entity bookId book) accountTree -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    ((res, formW), formE) <- handlerToWidget $ runFormPost $ renderForm accountTree id Nothing
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Creating account failed."
            generateHTML bookId accountTree $ Just (formW, formE)
        FormFailure _msg -> do
            pkcloudSetMessageDanger "Creating account failed."
            generateHTML bookId accountTree $ Just (formW, formE)
        FormSuccess (FormData account parentId shadowAccountId featured) -> do
            -- Check that shadow account is in this book.
            maybe (return ()) (Account.requireInBook accountTree) shadowAccountId

            -- Insert account.
            now <- liftIO getCurrentTime
            handlerToWidget $ runDB $ insert_ $ Account account now parentId shadowAccountId featured -- TODO: Check if insert failed. XXX

            -- Set message.
            pkcloudSetMessageSuccess "Successfully created account!"

            -- Redirect
            redirect $ AccountCreateR bookId

