module Handler.Folder.Create where

import qualified Book
import qualified Folder
import Import

data FormData = FormData {
      formDataName :: Text
    , formDataParentOrCredit :: Either FolderAccountId Bool -- JP: Optional? What about credit?
    }

renderForm trees = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> areq textField folderSettings Nothing
    <*> areq (eitherField ("Child folder", parentSettings, selectFieldList folders) ("Root folder", typeSettings, bootstrapRadioFieldList [("Debit" :: Text, True),("Credit", False)])) eitherSettings Nothing

    where
        folderSettings = withPlaceholder "Name" $ bfs ("Folder Name" :: Text)
        eitherSettings = "Folder type" -- bfs ("Folder type" :: Text)

        parentSettings = bfs ("Parent" :: Text)
        typeSettings = "Account type" -- bfs ("Account type" :: Text)

        folders = Folder.treesToFolders trees

generateHTML :: BookId -> [Book.AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId trees formM = do
    setTitle $ toHtml ("New Folder" :: Text)
    
    (formW, enctype) <- handlerToWidget $ maybe (generateFormPost $ renderForm trees) return formM

    [whamlet|
        <form method=post action=@{FolderCreateR bookId} enctype=#{enctype}>
            ^{formW}
            <div .form-group>
                <button type="submit" class="btn btn-default">
                    Create Folder
    |]

getFolderCreateR :: Key Book -> HandlerT App IO Html
getFolderCreateR = Book.layout (const "New Folder") $ \(Entity bookId book) accountTree -> do
    generateHTML bookId accountTree Nothing

postFolderCreateR :: Key Book -> HandlerT App IO Html
postFolderCreateR = Book.layout (const "New Folder") $ \(Entity bookId book) accountTree -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    ((res, formW), formE) <- handlerToWidget $ runFormPost $ renderForm accountTree
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Creating folder failed."
            generateHTML bookId accountTree $ Just (formW, formE)
        FormFailure _msg -> do
            pkcloudSetMessageDanger "Creating folder failed."
            generateHTML bookId accountTree $ Just (formW, formE)
        FormSuccess _ -> undefined
