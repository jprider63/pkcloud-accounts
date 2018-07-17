module Handler.Account.Create where

import qualified Book
import Import

data FormData = FormData {
      formDataAccount :: Text
    , formDataParent :: FolderAccountId
    , formDataFeatured :: Bool
    } 

renderForm trees = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> areq textField accountSettings Nothing
    <*> areq (selectField folders) parentSettings Nothing
    <*> areq checkBoxField featuredSettings Nothing -- TODO: Switch to bootstrapCheckBoxField XXX

    where
        accountSettings = withPlaceholder "Account" $ bfs ("Account Name" :: Text)
        parentSettings = bfs ("Folder" :: Text)
        featuredSettings = bfs ("Featured" :: Text)

        folders = optionsPairs $ concatMap (toFolders "") trees

        toFolders spacing (Book.FolderNode (Entity folderId folder) _ children) = 
            let spacing' = spacingChar <> spacing in
            (spacing <> folderAccountName folder, folderId):(concatMap (toFolders spacing') children)
        toFolders _ (Book.AccountLeaf _ _) = []

        spacingChar = "-"

generateHTML :: BookId -> [Book.AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId trees formM = do
    setTitle $ toHtml ("New Account" :: Text)

    (formW, enctype) <- handlerToWidget $ maybe (generateFormPost $ renderForm trees) return formM

    --     ^{msgH}
    [whamlet|
        <form method=post action=@{AccountCreateR bookId} enctype=#{enctype}>
            ^{formW}
            <div .form-group>
                <div class="col-xs-12">
                    <button type="submit" class="btn btn-default">
                        Create Account
    |]

getAccountCreateR :: BookId -> Handler Html
getAccountCreateR = Book.layout (const "New Account") $ \(Entity bookId book) accountTree -> do
    generateHTML bookId accountTree Nothing

postAccountCreateR :: BookId -> Handler Html
postAccountCreateR = Book.layout (const "New Account") $ \(Entity bookId book) accountTree -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    ((res, formW), formE) <- handlerToWidget $ runFormPost $ renderForm accountTree
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Creating account failed."
            generateHTML bookId accountTree $ Just (formW, formE)
        FormFailure _msg -> do
            pkcloudSetMessageDanger "Creating account failed."
            generateHTML bookId accountTree $ Just (formW, formE)
        FormSuccess (FormData account parentId featured) -> do
            -- Get whether account is credit.
            isCredit <- folderAccountIsCredit <$> (handlerToWidget $ runDB $ get404 parentId)

            -- Insert account.
            now <- liftIO getCurrentTime
            handlerToWidget $ runDB $ insert_ $ Account account now parentId isCredit featured

            -- Set message.
            pkcloudSetMessageSuccess "Successfully created account!"

            -- Redirect
            redirect $ AccountCreateR bookId

