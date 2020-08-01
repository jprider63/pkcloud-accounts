module Handler.Folder.Edit where

import qualified Account
import qualified Book
import qualified Folder
import Import

data FormData = FormData {
      formDataName :: Text
    , formDataParent :: Maybe FolderAccountId
    }

renderForm (Entity folderId folder) isDebit children trees = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> areq textField folderSettings (Just $ folderAccountName folder)
    <*> areq (parentField folders) parentSettings (Just <$> folderAccountParent folder)

    where
        parentField = check checkParent . selectFieldKeysM
        checkParent Nothing = Right Nothing
        checkParent (Just fId) =
            -- Require folder to be in book.
            case Account.getFolderNode trees fId of
                Nothing ->
                    Left ("Invalid parent folder." :: Text)
                Just folderNode -> 
                    -- Make sure parent is same type of folder.
                    if folderNodeIsDebit folderNode /= isDebit then
                        Left "Invalid parent account type."
                    -- Make sure it is not the same folder.
                    else if folderId == fId then
                        Left "Invalid parent folder."
                    -- Make sure it is not a child folder.
                    else if isJust (Account.getFolderNode children fId) then
                        Left "Invalid parent folder."
                    else
                        Right $ Just fId

        folderSettings = withPlaceholder "Name" $ bfs ("Folder Name" :: Text)
        parentSettings = bfs ("Parent" :: Text)

        folders = ("",Nothing):(second Just <$> Folder.treesToFolders (filterTrees trees))

        filterTrees :: [AccountTree] -> [AccountTree]
        filterTrees as = concatMap filterTree as


        filterTree :: AccountTree -> [AccountTree]
        filterTree a@AccountLeaf{..} = [a]

        -- Filter folders of the different type (debit/credit).
        filterTree f | folderNodeIsDebit f /= isDebit = []

        -- Filter out folder and children.
        filterTree f | (Entity fId f) <- folderNode f, fId == folderId = []
        
        -- Recursively filter children.
        filterTree f = [f {folderNodeChildren = filterTrees $ folderNodeChildren f}]


generateHTML :: BookId -> Entity FolderAccount -> Bool ->  [AccountTree] -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId folderE@(Entity folderId _) isDebit children trees formM = do
    setTitle $ toHtml ("Edit Folder" :: Text)

    (formW, enctype) <- handlerToWidget $ maybe (generateFormPost $ renderForm folderE isDebit children trees) return formM

    [whamlet|
        <h2>
            Edit Folder
        <form method=post action=@{FolderEditR bookId folderId} enctype=#{enctype}>
            ^{formW}
            <div .form-group>
                <a class="btn btn-default" href="@{FolderR bookId folderId}">
                    Cancel
                <button type="submit" class="btn btn-primary">
                    Edit Folder
    |]

getFolderEditR :: Key Book -> Key FolderAccount -> Handler Html
getFolderEditR = Folder.layout $ \(Entity bookId book) accountTree (FolderNode folderE balance isDebit children) ->
    generateHTML bookId folderE isDebit children accountTree Nothing

postFolderEditR :: Key Book -> Key FolderAccount -> Handler Html
postFolderEditR = Folder.layout $ \(Entity bookId book) accountTree (FolderNode folderE@(Entity faId _) balance isDebit children) -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    ((res, formW), formE) <- handlerToWidget $ runFormPost $ renderForm folderE isDebit children accountTree
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Editing folder failed."
            generateHTML bookId folderE isDebit children accountTree $ Just (formW, formE)
        FormFailure _ -> do
            pkcloudSetMessageDanger "Editing folder failed."
            generateHTML bookId folderE isDebit children accountTree $ Just (formW, formE)
        FormSuccess (FormData name parentM) -> do
            handlerToWidget $ runDB $ do
                case parentM of
                    Nothing -> do
                        -- If there's no parent, create a root (if it doesn't exist).
                        _ <- insertUnique $ BookFolderAccount bookId faId isDebit
                        return ()

                    Just _ ->
                        -- If there's a parent, delete root (if it exists).
                        deleteBy $ UniqueBookFolder bookId faId

                -- Update name and parent.
                update faId [FolderAccountName =. name, FolderAccountParent =. parentM]

            -- Set message.
            pkcloudSetMessageSuccess "Edited folder!"

            -- Redirect.
            redirect $ FolderR bookId faId

