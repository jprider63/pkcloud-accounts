module Handler.Transaction.Delete where

import qualified Account
import qualified Book
import qualified Folder
import Import

data FormData = FormData

renderForm transaction = renderBootstrap3 layout $ pure FormData
    where
        layout = BootstrapHorizontalForm (ColXs 0) (ColXs 3) (ColXs 0) (ColXs 9)

generateHTML :: BookId -> TransactionId -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId transactionId trees formM = do
    setTitle $ toHtml ("Delete Transaction" :: Text)

    -- Get transaction.
    transaction <- handlerToWidget $ runDB $ get404 transactionId

    (formW, enctype) <- handlerToWidget $ maybe (generateFormPost $ renderForm transaction) return formM

    [whamlet|
        <h2>
            Delete Transaction
        <form .form-horizontal method=post action=@{TransactionDeleteR bookId transactionId} enctype=#{enctype}>
            ^{formW}
            <div .form-group>
                <label .col-xs-2 .control-label>
                    Description
                <p .col-xs-10 .form-control-static>
                    #{transactionDescription transaction}
            <div .form-group>
                <label .col-xs-2 .control-label>
                    Date
                <p .col-xs-10 .form-control-static>
                    #{shortDateTime (transactionDate transaction)}
            <div .form-group>
                <p .text-danger .col-xs-offset-2 .col-xs-10>
                    Are you sure you want to delete this transaction?
            <div .form-group>
                <div .col-xs-offset-2 .col-xs-10>
                    <a class="btn btn-default" href="@{TransactionR bookId transactionId}">
                        Cancel
                    <button type="submit" class="btn btn-danger">
                        Delete Transaction
    |]

getTransactionDeleteR :: BookId -> TransactionId -> Handler Html
getTransactionDeleteR bookId transactionId = flip Book.layout bookId $ \(Entity bookId book) accountTree -> do
    generateHTML bookId transactionId accountTree Nothing

postTransactionDeleteR :: BookId -> TransactionId -> Handler Html
postTransactionDeleteR  bookId transactionId = flip Book.layout bookId $ \(Entity bookId book) accountTree -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    undefined
