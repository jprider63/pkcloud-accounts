module Handler.Transaction.Create where

import qualified Account
import qualified Book
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Read as TR
import qualified Folder
import Import
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Hamlet as TH
import Text.Julius (RawJavascript(..))

data FormData = FormData {
      formDataDescription :: Text
    , formDataDate :: UTCTime
    , formDataEntries :: [(Key Account, Either Nano Nano)]
    }

renderForm trees = do
    now <- getCurrentTime
    return $ renderBootstrap3 BootstrapBasicForm $ FormData
        <$> areq textField descriptionSettings Nothing
        <*> areq dateField dateSettings (Just now)
        <*> areq (entriesField accounts) entriesSettings Nothing
    
    where
        descriptionSettings = withPlaceholder "Description" $ bfs ("Description" :: Text)
        dateSettings = bfs ("Date" :: Text)
        entriesSettings = bfs ("Entries" :: Text)

        accounts = Folder.treesToAccounts trees

generateHTML :: BookId -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId trees formM = do
    setTitle $ toHtml ("New Transaction" :: Text)
    
    (formW, enctype) <- handlerToWidget $ maybe (renderForm trees >>= generateFormPost) return formM

    -- TODO: Button to load saved transactions. XXX

    [whamlet|
        <form method=post action=@{TransactionCreateR bookId} enctype=#{enctype}>
            ^{formW}
            <div .form-group>
                <button type="submit" class="btn btn-default">
                    Create Transaction
    |]

getTransactionCreateR :: BookId -> Handler Html
getTransactionCreateR = Book.layout (const "New Transaction") $ \(Entity bookId book) accountTree -> do
    generateHTML bookId accountTree Nothing

postTransactionCreateR :: BookId -> Handler Html
postTransactionCreateR = Book.layout (const "New Transaction") $ \(Entity bookId book) accountTree -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book

    ((res, formW), formE) <- handlerToWidget $ renderForm accountTree >>= runFormPost
    case res of
        FormMissing -> do
            pkcloudSetMessageDanger "Creating transaction failed."
            generateHTML bookId accountTree $ Just (formW, formE)
        FormFailure _msg -> do
            pkcloudSetMessageDanger "Creating transaction failed."
            generateHTML bookId accountTree $ Just (formW, formE)
        FormSuccess (FormData description date' entries) -> do
            -- Get user.
            uId <- handlerToWidget requireAuthId

            date <- extractCurrentTime date'

            handlerToWidget $ runDB $ do
                -- Insert transaction.
                transactionId <- insert $ Transaction description date uId

                -- Insert transaction amounts.
                mapM_ (insertTransactionAccount transactionId accountTree) entries

            -- Set message.
            pkcloudSetMessageSuccess "Created transaction!"

            -- Redirect.
            redirect $ TransactionCreateR bookId

    where
        extractCurrentTime (UTCTime day _) = do
            (UTCTime _ time) <- getCurrentTime
            return $ UTCTime day time

        insertTransactionAccount tId accountTree (accountId, amountE) = do
            -- Check account type.
            isDebit <- Account.isDebit accountTree accountId

            -- Compute amount based on type.
            let amount = toAmount isDebit amountE
            
            -- Insert transaction.
            insert_ $ TransactionAccount tId accountId amount

        toAmount True (Left v) = v
        toAmount False (Left v) = negate v
        toAmount True (Right v) = negate v
        toAmount False (Right v) = v
            
entriesField :: forall m a . (ToBackendKey SqlBackend a, HandlerT App IO ~ m, RenderMessage App Text) => [(Text, Key a)] -> Field m [(Key a, Either Nano Nano)]
-- entriesField :: forall m a . (ToBackendKey SqlBackend a, HandlerT App IO ~ m, RenderMessage App Text) => [(Text, Key a)] -> Field m [(Entity a, Either Nano Nano)]
entriesField accounts = -- checkMMap toEntity (map toKey) $ 
    Field parse view UrlEncoded
    
    where
        -- toKey (Entity k _, b) = (k, b)
        -- toEntity rs' = do
        --     rs <- runDB $ mapM (\(k, b) -> do
        --             e <- get404 k
        --             return $ (Entity k e, b)
        --         ) rs'

        --     return ( Right rs :: Either Text [(Entity a, Either Nano Nano)])

        -- parse vs _ | Just ps <- toTriple vs = return $ fmap Just $ sequence $ map parseTriple ps
        -- TODO: Could check that each account only appears once.
        parse vs _ | Just ps <- toTriple vs = return $ case sequence $ map parseTriple ps of
            Left e -> Left e
            Right vs -> 
              let (d,c) = foldr (\v (d,c) -> case v of 
                      (_, Left v) -> (d + v, c)
                      (_, Right v) -> (d, c + v)
                    ) (0,0) vs
              in
              if d == c then
                Right $ Just vs
              else
                Left "Debits and credits must be equal." -- TODO: Move this check later where we know whether it's debit or credit? XXX
        parse _ _ = return $ Left "entriesField: Unreachable?"

        parseTriple (k, "", "") = Left "Amount missing."
        parseTriple (k, "", c) = case (TR.decimal k, TR.rational c) of
            (Right (k, ""),Right (c, "")) -> Right (toSqlKey k, Right c)
            _ -> Left "Invalid number."
        parseTriple (k, d, "") = case (TR.decimal k, TR.rational d) of
            (Right (k, ""),Right (d, "")) -> Right (toSqlKey k, Left d)
            _ -> Left "Invalid number."
        parseTriple _ = Left "Only a debit or credit may be present."
        
        toTriple [] = Just []
        toTriple (a:b:c:t) = ((a,b,c):) <$> toTriple t
        toTriple _ = Nothing

        view theId name attrs val isReq = do
            let test = [shamlet|#{name}|]
            toWidget [julius|
                var _addEntry = function( i) {
                    var k = 1; // TODO XXX
                
                    return function() {
                        // Make a fresh k.
                        k += 1;

                        var parent = $('##{textToJs theId}');
                        parent.append( '<div id="#{textToJs theId}-'+k+'" class="form-inline">#{htmlToJs accountsH}<input name="#{textToJs name}" #{attrsToJs attrs} type="number" value=""></input><input name="#{textToJs name}" #{attrsToJs attrs} type="number" value=""></input><div class="btn-group" role="group"><button type="button" class="btn btn-default" aria-label="Remove" onclick="_removeEntry(\'#{textToJs theId}\',\''+k+'\')"><span class="glyphicon glyphicon-minus" /></button><button type="button" class="btn btn-default" aria-label="Remove" onclick="_addEntry('+k+')"><span class="glyphicon glyphicon-plus" /></button></div></div>');
                    }
                }();

                var _removeEntry = function( parentId, entryC) {
                    var parent = $('#'+parentId);
                    var entry = $('#'+parentId+'-'+entryC);

                    // Remove entry if parent has at least 2 children.
                    var children = parent.children();
                    let childrenC = children.length
                    if ( childrenC >= 2) {
                        entry.remove();
                    }

                    // // Disable children's remove buttons if there is one entry left.
                    // if ( childrenC <= 1) {
                    //         console.log( children);
                    //     children.each( function( i, child) {
                    //         console.log( child); 
                    //         child.attr("disabled", true)
                    //     });
                    // }
                };
            |]
            let i = 1 :: Int
            [whamlet|$newline never
                <div id="#{theId}">
                    <div id="#{theId}-#{i}" .form-inline>
                        #{accountsH}
                        <input name="#{name}" *{attrs} type="number" value="TODO">
                        <input name="#{name}" *{attrs} type="number" value="TODO">
                        <div .btn-group role="group">
                            <button type="button" class="btn btn-default" aria-label="Remove" onclick="_removeEntry( '#{theId}', '#{i}')">
                                <span .glyphicon .glyphicon-minus>
                            <button type="button" class="btn btn-default" aria-label="Remove" onclick="_addEntry('#{i}')">
                                <span .glyphicon .glyphicon-plus>
            |]
            -- :isReq:required="" 
                    -- <div id="#{theId}-#{i}" .form-inline>
                        -- <div .btn-group role="group">
                        -- <div .input-group>

          where
            accountsH = [shamlet|$newline never
                <select name="#{name}" *{attrs} :isReq:required="" value="TODO">
                    ^{mapM_ accountH accounts}
            |]

            accountH (t, aId) = 
                let isSel = False in -- TODO: Impl this XXX
                [shamlet|$newline never
                    <option value="#{fromSqlKey aId}" :isSel:selected>#{t}
                |]

            textToJs = RawJavascript . TB.fromText
            htmlToJs = RawJavascript . TB.fromString . renderHtml
            attrsToJs = htmlToJs . TH.attrsToHtml




