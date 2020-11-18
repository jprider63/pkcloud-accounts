module PKCloud.Accounts.Handler.Transaction.Delete where

import qualified Account
import qualified Book
import qualified Breadcrumb
import qualified Transaction
import Import

data FormData = FormData

renderForm = renderBootstrap3 layout $ pure FormData
    where
        layout = BootstrapHorizontalForm (ColXs 0) (ColXs 3) (ColXs 0) (ColXs 9)

generateHTML :: BookId -> Entity Transaction -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId (Entity transactionId transaction) trees formM = do
    setTitle $ toHtml ("Delete Transaction" :: Text)

    -- Generate form.
    (formW, enctype) <- handlerToWidget $ maybe (generateFormPost renderForm) return formM

    [whamlet|
        <h2>
            Delete Transaction
        <form .form-horizontal method=post action="@{TransactionDeleteR bookId transactionId}" enctype=#{enctype}>
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
getTransactionDeleteR = Transaction.layout Breadcrumb.Delete $ \(Entity bookId _) transactionE _ accountTree -> do
    generateHTML bookId transactionE accountTree Nothing

postTransactionDeleteR :: BookId -> TransactionId -> Handler Html
postTransactionDeleteR  = Transaction.layout Breadcrumb.Delete $ \(Entity bookId book) transactionE _ accountTree -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    ((res, formW), formE) <- handlerToWidget $ runFormPost renderForm
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Deleting transaction failed."
            generateHTML bookId transactionE accountTree $ Just (formW, formE)
        FormFailure _msg -> do
            pkcloudSetMessageDanger "Deleting transaction failed."
            generateHTML bookId transactionE accountTree $ Just (formW, formE)
        FormSuccess FormData -> do
            -- Delete transaction.
            handlerToWidget $ runDB $ do
                let transactionId = entityKey transactionE
                deleteWhere [TransactionAccountTransaction ==. transactionId]
                delete transactionId

            -- Set message.
            pkcloudSetMessageSuccess "Deleted transaction!"

            -- Redirect.
            redirect $ BookR bookId


