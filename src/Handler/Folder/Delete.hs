module Handler.Folder.Delete where

import qualified Book
import qualified Breadcrumb
import qualified Folder
import           Import

data FormData = FormData

renderForm = renderBootstrap3 layout $ pure FormData
    where
        layout = BootstrapHorizontalForm (ColXs 0) (ColXs 3) (ColXs 0) (ColXs 9)

canDeleteFolder :: AccountTree -> Bool
canDeleteFolder FolderNode{..} = null folderNodeChildren

generateHTML :: BookId -> AccountTree -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId folderNode formM = do
    setTitle $ toHtml ("Delete Folder" :: Text)

    [whamlet|
        <h2>
            Delete Folder
    |]

    -- Can't delete if there are children.
    if not $ canDeleteFolder folderNode then
        [whamlet|
            <p>
                You can't delete folder <b>#{folderAccountName folder}</b> since it has sub-accounts or sub-folders. Delete the sub-accounts and sub-folders first.
            <a href="@{FolderR bookId folderId}" .btn .btn-default>
                Cancel
        |]
    else do

        (formW, enctype) <- handlerToWidget $ maybe (generateFormPost renderForm) return formM
        let accountType = if isDebit then "Debit" else "Credit" :: Text
        [whamlet|
            <form .form-horizontal method=post enctype=#{enctype} action="@{FolderDeleteR bookId folderId}">
                ^{formW}
                <div .form-group>
                    <label .col-xs-2 .control-label>
                        Folder
                    <p .col-xs-10 .form-control-static>
                        #{folderAccountName folder}
                <div .form-group>
                    <label .col-xs-2 .control-label>
                        Type
                    <p .col-xs-10 .form-control-static>
                        #{accountType}
                <div .form-group>
                    <p .text-danger .col-xs-offset-2 .col-xs-10>
                        Are you sure you want to delete this folder?
                <div .form-group>
                    <div .col-xs-offset-2 .col-xs-10>
                        <a class="btn btn-default" href="@{FolderR bookId folderId}">
                            Cancel
                        <button type="submit" class="btn btn-danger">
                            Delete Folder
        |]

    where
        (FolderNode (Entity folderId folder) _ isDebit _) = folderNode


getFolderDeleteR :: Key Book -> FolderAccountId -> Handler Html
getFolderDeleteR = Folder.layout Breadcrumb.Delete $ \(Entity bookId book) accountTree folderNode@(FolderNode folderE balance isDebit children) ->
    generateHTML bookId folderNode Nothing

postFolderDeleteR :: Key Book -> FolderAccountId -> Handler Html
postFolderDeleteR = Folder.layout Breadcrumb.Delete $ \(Entity bookId book) accountTree folderNode@(FolderNode folderE balance isDebit children) -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    -- Check that the folder is empty.
    let folderId = entityKey folderE
    unless (canDeleteFolder folderNode) $ 
        redirect $ FolderDeleteR bookId folderId

    ((res, formW), formE) <- handlerToWidget $ runFormPost renderForm
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Deleting folder failed."
            generateHTML bookId folderNode $ Just (formW, formE)
        FormFailure _msg -> do
            pkcloudSetMessageDanger "Deleting folder failed."
            generateHTML bookId folderNode $ Just (formW, formE)
        FormSuccess FormData -> do
            -- Delete folder.
            handlerToWidget $ runDB $ do
                -- Delete root folder if it exists.
                deleteBy $ UniqueBookFolder bookId folderId
                
                delete folderId


            -- Set message.
            pkcloudSetMessageSuccess "Deleted folder!"

            -- Redirect.
            redirect $ BookR bookId
