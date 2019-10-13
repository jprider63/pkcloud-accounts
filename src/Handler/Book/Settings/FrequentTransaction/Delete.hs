module Handler.Book.Settings.FrequentTransaction.Delete where

import qualified Book
import qualified FrequentTransaction
import Import

data FormData = FormData

renderForm = renderBootstrap3 layout $ pure FormData
    where
        layout = BootstrapHorizontalForm (ColXs 0) (ColXs 3) (ColXs 0) (ColXs 9)

generateHTML :: BookId -> Entity FrequentTransaction -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId (Entity ftId ft) trees formM = do
    setTitle $ toHtml ("Delete Transaction" :: Text)

    -- Generate form.
    (formW, enctype) <- handlerToWidget $ maybe (generateFormPost renderForm) return formM

    [whamlet|
        <h2>
            Delete Frequent Transaction
        <form .form-horizontal method=post action="@{BookSettingsFrequentDeleteR bookId ftId}" enctype=#{enctype}>
            ^{formW}
            <div .form-group>
                <label .col-xs-2 .control-label>
                    Description
                <p .col-xs-10 .form-control-static>
                    #{frequentTransactionDescription ft}
            <div .form-group>
                <p .text-danger .col-xs-offset-2 .col-xs-10>
                    Are you sure you want to delete this frequent transaction?
            <div .form-group>
                <div .col-xs-offset-2 .col-xs-10>
                    <a class="btn btn-default" href="@{BookSettingsFrequentR bookId ftId}">
                        Cancel
                    <button type="submit" class="btn btn-danger">
                        Delete Frequent Transaction
    |]

getBookSettingsFrequentDeleteR :: BookId -> FrequentTransactionId -> Handler Html
getBookSettingsFrequentDeleteR = FrequentTransaction.layout $ \(Entity bookId _) ftE _ accountTree ->
    generateHTML bookId ftE accountTree Nothing

postBookSettingsFrequentDeleteR :: BookId -> FrequentTransactionId -> Handler Html
postBookSettingsFrequentDeleteR = FrequentTransaction.layout $ \(Entity bookId book) ftE _ accountTree -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    ((res, formW), formE) <- handlerToWidget $ runFormPost renderForm
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Deleting frequent transaction failed."
            generateHTML bookId ftE accountTree $ Just (formW, formE)
        FormFailure _msg -> do
            pkcloudSetMessageDanger "Deleting frequent transaction failed."
            generateHTML bookId ftE accountTree $ Just (formW, formE)
        FormSuccess FormData -> do
            -- Delete transaction.
            handlerToWidget $ runDB $ do
                let ftId = entityKey ftE
                deleteWhere [FrequentTransactionAccountTransaction ==. ftId]
                delete ftId

            -- Set message.
            pkcloudSetMessageSuccess "Deleted frequent transaction!"

            -- Redirect.
            redirect $ BookSettingsR bookId

