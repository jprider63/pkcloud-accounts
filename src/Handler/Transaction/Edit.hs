module Handler.Transaction.Edit where

import qualified Account
import qualified Book
import qualified Breadcrumb
import qualified Transaction
import Handler.Transaction.Create hiding (generateHTML)
import Import

generateHTML :: BookId -> Entity Transaction -> [Entity TransactionAccount] -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId (Entity transactionId transaction) entries trees formM = do
    setTitle $ toHtml ("Edit Transaction" :: Text)

    let description = transactionDescription transaction
    let date = transactionDate transaction
    entries <- handlerToWidget $ Account.transactionsToEntries trees entries
    (formW, enctype) <- handlerToWidget $ maybe (renderForm bookId (Just description) (Just date) (Just entries) trees >>= generateFormPost) return formM

    [whamlet|
        <h2>
            Edit Transaction
        <form method=post action=@{TransactionEditR bookId transactionId} enctype=#{enctype}>
            ^{formW}
            <div .form-group>
                <a class="btn btn-default" href="@{TransactionR bookId transactionId}">
                    Cancel
                <button type="submit" class="btn btn-primary">
                    Edit Transaction
    |]


getTransactionEditR :: BookId -> TransactionId -> Handler Html
getTransactionEditR = Transaction.layout Breadcrumb.Edit $ \(Entity bookId _) transactionE entries accountTree -> do
    generateHTML bookId transactionE entries accountTree Nothing

postTransactionEditR :: BookId -> TransactionId -> Handler Html
postTransactionEditR  = Transaction.layout Breadcrumb.Edit $ \(Entity bookId book) transactionE entries accountTree -> do
    let Entity transactionId transaction = transactionE

    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    ((res, formW), formE) <- handlerToWidget $ renderForm bookId Nothing Nothing Nothing accountTree >>= runFormPost
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Editing transaction failed."
            generateHTML bookId transactionE entries accountTree $ Just (formW, formE)
        FormFailure _msg -> do
            pkcloudSetMessageDanger "Editing transaction failed."
            generateHTML bookId transactionE entries accountTree $ Just (formW, formE)
        FormSuccess (FormData _ description (UTCTime date' _) entries) -> do
            -- Extract date time.
            let (UTCTime _ time) = transactionDate transaction
            let date = UTCTime date' time

            handlerToWidget $ runDB $ do
                -- Delete old transaction amounts.
                deleteWhere [TransactionAccountTransaction ==. transactionId]

                -- Insert transaction amounts.
                mapM_ (insertTransactionAccount TransactionAccount transactionId accountTree) entries

                -- Update transaction.
                update transactionId [TransactionDescription =. description, TransactionDate =. date]

            -- Set message.
            pkcloudSetMessageSuccess "Edited transaction!"

            -- Redirect.
            redirect $ TransactionR bookId transactionId
