module Handler.Transaction.Create where

import qualified Account
import qualified Book
import qualified Folder
import Import
import Text.Blaze (toMarkup)

data FormData = FormData {
      formDataEmptyField :: Maybe (Key FrequentTransaction)
    , formDataDescription :: Text
    , formDataDate :: UTCTime
    , formDataEntries :: [(Key Account, Either Nano Nano)]
    }

renderForm bookId descM dateM entriesM trees = do
    ftField <- frequentTransationField trees bookId descriptionId entriesId
    return $ renderBootstrap3 BootstrapBasicForm $ FormData
        <$> aopt ftField frequentTransactionSettings Nothing
        <*> areq textField descriptionSettings descM
        <*> areq dateField dateSettings dateM
        <*> areq (entriesField accounts shadows) entriesSettings entriesM
    
    where
        descriptionId = "form-description-field"
        descriptionSettings = withFieldId descriptionId $ withPlaceholder "Description" $ bfs ("Description" :: Text)

        dateSettings = bfs ("Date" :: Text)

        entriesId = "form-entries-field"
        entriesSettings = withFieldId entriesId $ bfs ("Entries" :: Text)

        frequentTransactionSettings = 
          let s = bfs ("Load frequent transaction" :: Text) in
          s { fsAttrs = ("autocomplete","off"):fsAttrs s }

        accounts = Folder.treesToAccounts trees
        shadows = Folder.treesToShadows trees

generateHTML :: BookId -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId trees formM = do
    setTitle $ toHtml ("New Transaction" :: Text)
    
    (formW, enctype) <- handlerToWidget $ maybe (getCurrentTime >>= \now -> renderForm bookId Nothing (Just now) Nothing trees >>= generateFormPost) return formM

    -- TODO: Button to load saved transactions. XXX

    [whamlet|
        <h2>
            New Transaction
        <form method=post action=@{TransactionCreateR bookId} enctype=#{enctype}>
            ^{formW}
            <div .form-group>
                <button type="submit" class="btn btn-default">
                    Create Transaction
    |]

getTransactionCreateR :: BookId -> Handler Html
getTransactionCreateR = Book.layout $ \(Entity bookId book) accountTree -> do
    generateHTML bookId accountTree Nothing

postTransactionCreateR :: BookId -> Handler Html
postTransactionCreateR = Book.layout $ \(Entity bookId book) accountTree -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    ((res, formW), formE) <- handlerToWidget $ renderForm bookId Nothing Nothing Nothing accountTree >>= runFormPost
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Creating transaction failed."
            generateHTML bookId accountTree $ Just (formW, formE)
        FormFailure _msg -> do
            pkcloudSetMessageDanger "Creating transaction failed."
            generateHTML bookId accountTree $ Just (formW, formE)
        FormSuccess (FormData _ description date' entries) -> do
            -- Get user.
            uId <- handlerToWidget requireAuthId

            date <- extractCurrentTime date'

            handlerToWidget $ runDB $ do
                -- Insert transaction.
                transactionId <- insert $ Transaction description date uId

                -- Insert transaction amounts.
                mapM_ (insertTransactionAccount TransactionAccount transactionId accountTree) entries

            -- Set message.
            pkcloudSetMessageSuccess "Created transaction!"

            -- Redirect.
            redirect $ TransactionCreateR bookId

    where
        extractCurrentTime (UTCTime day _) = do
            (UTCTime _ time) <- getCurrentTime
            return $ UTCTime day time

insertTransactionAccount taConstr tId accountTree (accountId, amountE) = do
    -- Make sure account is in book.
    lift $ Account.requireInBook accountTree accountId

    -- Check account type.
    isDebit <- Account.isDebit accountTree accountId

    -- Compute amount based on type.
    let amount = toAmount isDebit amountE
    
    -- Insert transaction.
    insert_ $ taConstr tId accountId amount

    where

        toAmount True (Left v) = v
        toAmount False (Left v) = negate v
        toAmount True (Right v) = negate v
        toAmount False (Right v) = v
            
