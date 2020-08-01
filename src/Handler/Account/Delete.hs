module Handler.Account.Delete where

import qualified Account
import qualified Book
import Import

data FormData = FormData

renderForm = renderBootstrap3 layout $ pure FormData
    where
        layout = BootstrapHorizontalForm (ColXs 0) (ColXs 3) (ColXs 0) (ColXs 9)

generateHTML :: BookId -> Entity Account -> Bool -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId (Entity accountId account) isDebit formM = do
    setTitle $ toHtml ("Delete Account" :: Text)

    [whamlet|
        <h2>
            Delete Account
    |]

    -- Can't delete if there are any transactions.
    canDelete <- canDeleteAccount accountId
    if not canDelete then
        [whamlet|
            <p>
                You can't delete account <b>#{accountName account}</b> since it has transactions (or frequent transactions). Delete or change the transactions first.
            <a href="@{AccountR bookId accountId}" .btn .btn-default>
                Cancel
        |]
    else do
        (formW, enctype) <- handlerToWidget $ maybe (generateFormPost renderForm) return formM
        let accountType = if isDebit then "Debit" else "Credit" :: Text
        [whamlet|
            <form .form-horizontal method=post enctype=#{enctype} action="@{AccountDeleteR bookId accountId}">
                ^{formW}
                <div .form-group>
                    <label .col-xs-2 .control-label>
                        Account
                    <p .col-xs-10 .form-control-static>
                        #{accountName account}
                <div .form-group>
                    <label .col-xs-2 .control-label>
                        Type
                    <p .col-xs-10 .form-control-static>
                        #{accountType}
                <div .form-group>
                    <p .text-danger .col-xs-offset-2 .col-xs-10>
                        Are you sure you want to delete this account?
                <div .form-group>
                    <div .col-xs-offset-2 .col-xs-10>
                        <a class="btn btn-default" href="@{AccountR bookId accountId}">
                            Cancel
                        <button type="submit" class="btn btn-danger">
                            Delete Account
        |]

canDeleteAccount accountId = do
    handlerToWidget $ runDB $ do
      c <- count [TransactionAccountAccount ==. accountId]
      c' <- count [FrequentTransactionAccountAccount ==. accountId]
      return $ c + c' == 0

getAccountDeleteR :: Key Book -> Key Account -> Handler Html
getAccountDeleteR = Account.layout $ \(Entity bookId book) accountE accountIsDebit accountTree -> do
    generateHTML bookId accountE accountIsDebit Nothing

postAccountDeleteR :: Key Book -> Key Account -> Handler Html
postAccountDeleteR = Account.layout $ \(Entity bookId book) accountE@(Entity accountId _) accountIsDebit accountTree -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    -- Check that there are not transactions.
    canDelete <- canDeleteAccount accountId
    unless canDelete $ 
        redirect $ AccountDeleteR bookId accountId

    ((res, formW), formE) <- handlerToWidget $ runFormPost renderForm
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Deleting account failed."
            generateHTML bookId accountE accountIsDebit $ Just (formW, formE)
        FormFailure _msg -> do
            pkcloudSetMessageDanger "Deleting account failed."
            generateHTML bookId accountE accountIsDebit $ Just (formW, formE)
        FormSuccess FormData -> do
            -- TODO: Remove any foreign references to this account.

            -- Delete account.
            handlerToWidget $ runDB $ do
                
                delete accountId

            -- Set message.
            pkcloudSetMessageSuccess "Deleted account!"

            -- Redirect.
            redirect $ BookR bookId

