module Handler.Book.Settings.FrequentTransaction.Create where

import Handler.Transaction.Create (insertTransactionAccount)
import qualified Book
import qualified Folder
import Import

data FormData = FormData {
      formDataDescription :: Text
    , formDataEntries :: [(Key Account, Either Nano Nano)]
    }

renderForm descM entriesM trees = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> areq textField descriptionSettings descM
    <*> areq (entriesField accounts shadows) entriesSettings entriesM
    
    where
        descriptionSettings = withPlaceholder "Description" $ bfs ("Description" :: Text)
        entriesSettings = bfs ("Entries" :: Text)

        accounts = Folder.treesToAccounts trees
        shadows = Folder.treesToShadows trees

generateHTML :: BookId -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId trees formM = do
    setTitle $ toHtml ("New Frequent Transaction" :: Text)
    
    (formW, enctype) <- handlerToWidget $ maybe (generateFormPost $ renderForm Nothing Nothing trees) return formM

    [whamlet|
        <h2>
            New Frequent Transaction
        <form method=post action=@{BookSettingsFrequentCreateR bookId} enctype=#{enctype}>
            ^{formW}
            <div .form-group>
                <button type="submit" class="btn btn-default">
                    Create Frequent Transaction
    |]


getBookSettingsFrequentCreateR :: BookId -> Handler Html
getBookSettingsFrequentCreateR = Book.layout $ \(Entity bookId book) accountTree -> do
    generateHTML bookId accountTree Nothing

postBookSettingsFrequentCreateR :: BookId -> Handler Html
postBookSettingsFrequentCreateR = Book.layout $ \(Entity bookId book) accountTree -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    ((res, formW), formE) <- handlerToWidget $ runFormPost $ renderForm Nothing Nothing accountTree
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Creating frequent transaction failed."
            generateHTML bookId accountTree $ Just (formW, formE)
        FormFailure _msg -> do
            pkcloudSetMessageDanger "Creating frequent transaction failed."
            generateHTML bookId accountTree $ Just (formW, formE)
        FormSuccess (FormData description entries) -> do
            -- Get user.
            uId <- handlerToWidget requireAuthId

            handlerToWidget $ runDB $ do
                -- Insert transaction.
                transactionId <- insert $ FrequentTransaction bookId description uId

                -- Insert transaction amounts.
                mapM_ (insertTransactionAccount FrequentTransactionAccount transactionId accountTree) entries

            -- Set message.
            pkcloudSetMessageSuccess "Created frequent transaction!"

            -- Redirect.
            redirect $ BookSettingsFrequentCreateR bookId

