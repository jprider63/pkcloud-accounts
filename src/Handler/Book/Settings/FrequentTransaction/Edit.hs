module Handler.Book.Settings.FrequentTransaction.Edit where

import qualified Account
import qualified Book
import qualified Breadcrumb
import qualified FrequentTransaction
import           Handler.Book.Settings.FrequentTransaction.Create hiding (generateHTML)
import           Handler.Transaction.Create (insertTransactionAccount)
import           Import

generateHTML :: BookId -> Entity FrequentTransaction -> [Entity FrequentTransactionAccount] -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bId (Entity ftId ft) fts trees formM = do
    setTitle $ toHtml ("Edit Frequent Transaction" :: Text)

    let description = frequentTransactionDescription ft
    entries <- handlerToWidget $ Account.transactionsToEntries trees fts

    (formW, enctype) <- handlerToWidget $ maybe (generateFormPost $ renderForm (Just description) (Just entries) trees) return formM

    [whamlet|
        <h2>
            Edit Frequent Transaction
        <form method=post action="@{BookSettingsFrequentEditR bId ftId}" enctype=#{enctype}>
            ^{formW}
            <div .form-group>
                <a class="btn btn-default" href="@{BookSettingsFrequentR bId ftId}">
                    Cancel
                <button type="submit" class="btn btn-primary">
                    Edit Transaction
    |]

getBookSettingsFrequentEditR :: BookId -> FrequentTransactionId -> Handler Html
getBookSettingsFrequentEditR = FrequentTransaction.layout Breadcrumb.Edit $ \(Entity bookId _book) ftE fts accountTree -> do
    generateHTML bookId ftE fts accountTree Nothing

postBookSettingsFrequentEditR :: BookId -> FrequentTransactionId -> Handler Html
postBookSettingsFrequentEditR = FrequentTransaction.layout Breadcrumb.Edit $ \(Entity bookId book) ftE@(Entity ftId _) fts accountTree -> do

    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    ((res, formW), formE) <- handlerToWidget $ runFormPost $ renderForm Nothing Nothing accountTree
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Editing frequent transaction failed."
            generateHTML bookId ftE fts accountTree $ Just (formW, formE)

        FormFailure _msg -> do
            pkcloudSetMessageDanger "Editing frequent transaction failed."
            generateHTML bookId ftE fts accountTree $ Just (formW, formE)

        FormSuccess (FormData description entries) -> do
            handlerToWidget $ runDB $ do
                -- Delete old frequent transaction amounts.
                deleteWhere [FrequentTransactionAccountTransaction ==. ftId]

                -- Insert frequent transaction amounts.
                mapM_ (insertTransactionAccount FrequentTransactionAccount ftId accountTree) entries
                
                -- Update frequent transaction.
                update ftId [FrequentTransactionDescription =. description]

            -- Set message.
            pkcloudSetMessageSuccess "Edited frequent transaction!"

            -- Redirect.
            redirect $ BookSettingsFrequentR bookId ftId

