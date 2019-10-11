module Handler.Transaction.Edit where

import qualified Account
import qualified Book
import qualified Transaction
import Handler.Transaction.Create hiding (generateHTML)
import Import

generateHTML :: BookId -> Entity Transaction -> [Entity TransactionAccount] -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId (Entity transactionId transaction) entries trees formM = do
    setTitle $ toHtml ("Edit Transaction" :: Text)

    let description = transactionDescription transaction
    let date = transactionDate transaction
    entries <- handlerToWidget convertEntries
    (formW, enctype) <- handlerToWidget $ maybe (generateFormPost $ renderForm (Just description) (Just date) (Just entries) trees) return formM

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

    where

        -- Convert entries.
        convertEntries = do
            -- Return transaction and entries. Implicitly checks if account is in book.
            mapM (\(Entity _ TransactionAccount{..}) -> do
                -- Get account type.
                isDebit <- Account.isDebit trees transactionAccountAccount
                    
                return (transactionAccountAccount, fromAmount isDebit transactionAccountAmount)
              ) entries

        fromAmount True v | v >= 0 = Left v
        fromAmount True v = Right $ negate v
        fromAmount False v | v >= 0 = Right v
        fromAmount False v = Left $ negate v


getTransactionEditR :: BookId -> TransactionId -> Handler Html
getTransactionEditR = Transaction.layout $ \(Entity bookId _) transactionE entries accountTree -> do
    generateHTML bookId transactionE entries accountTree Nothing

postTransactionEditR :: BookId -> TransactionId -> Handler Html
postTransactionEditR  = Transaction.layout $ \(Entity bookId book) transactionE entries accountTree -> do
    let Entity transactionId transaction = transactionE

    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    ((res, formW), formE) <- handlerToWidget $ runFormPost $ renderForm Nothing Nothing Nothing accountTree
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Editing transaction failed."
            generateHTML bookId transactionE entries accountTree $ Just (formW, formE)
        FormFailure _msg -> do
            pkcloudSetMessageDanger "Editing transaction failed."
            generateHTML bookId transactionE entries accountTree $ Just (formW, formE)
        FormSuccess (FormData description (UTCTime date' _) entries) -> do
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
