module Handler.Transaction.Edit where

import qualified Account
import qualified Book
import qualified Folder
import Handler.Transaction.Create hiding (generateHTML)
import Import

generateHTML :: BookId -> TransactionId -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId transactionId trees formM = do
    setTitle $ toHtml ("Edit Transaction" :: Text)

    (description, date, entries) <- handlerToWidget loadTransaction
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

        -- Load transaction and check that accounts are in the book.
        loadTransaction = do
            -- Load transaction.
            (transaction, entries) <- runDB $ do
                t <- get404 transactionId
                e <- selectList [TransactionAccountTransaction ==. transactionId] [Asc TransactionAccountId]

                return (t, e)

            -- -- Check permission on all accounts.
            -- Account.requireAllInBook trees $ map (transactionAccountAccount . entityVal) $ take 1 entries

            -- Return transaction and entries. Implicitly checks if account is in book.
            entries' <- mapM (\(Entity _ TransactionAccount{..}) -> do
                -- Get account type.
                isDebit <- Account.isDebit trees transactionAccountAccount
                    
                return (transactionAccountAccount, fromAmount isDebit transactionAccountAmount)
              ) entries
            return (transactionDescription transaction, transactionDate transaction, entries')

        fromAmount True v | v >= 0 = Left v
        fromAmount True v = Right $ negate v
        fromAmount False v | v >= 0 = Right v
        fromAmount False v = Left $ negate v


getTransactionEditR :: BookId -> TransactionId -> Handler Html
getTransactionEditR bookId transactionId = flip Book.layout bookId $ \(Entity bookId book) accountTree -> do
    generateHTML bookId transactionId accountTree Nothing

postTransactionEditR :: BookId -> TransactionId -> Handler Html
postTransactionEditR  bookId transactionId = flip Book.layout bookId $ \(Entity bookId book) accountTree -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    ((res, formW), formE) <- handlerToWidget $ runFormPost $ renderForm Nothing Nothing Nothing accountTree
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Editing transaction failed."
            generateHTML bookId transactionId accountTree $ Just (formW, formE)
        FormFailure _msg -> do
            pkcloudSetMessageDanger "Editing transaction failed."
            generateHTML bookId transactionId accountTree $ Just (formW, formE)
        FormSuccess (FormData description (UTCTime date' _) entries) -> do
            handlerToWidget $ runDB $ do
                -- Extract date time.
                (UTCTime _ time) <- transactionDate <$> get404 transactionId
                let date = UTCTime date' time

                -- Delete old transaction amounts.
                deleteWhere [TransactionAccountTransaction ==. transactionId]

                -- Insert transaction amounts.
                mapM_ (insertTransactionAccount transactionId accountTree) entries

                -- Update transaction.
                update transactionId [TransactionDescription =. description, TransactionDate =. date]

            -- Set message.
            pkcloudSetMessageSuccess "Edited transaction!"

            -- Redirect.
            redirect $ TransactionR bookId transactionId
