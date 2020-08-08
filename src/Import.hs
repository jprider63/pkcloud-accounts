module Import
    ( module Import
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Read as TR
import qualified Database.Esqueleto as E
import           Foundation            as Import
import           Import.NoFoundation   as Import
import           Text.Blaze
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Hamlet as TH
import           Text.Julius (RawJavascript(..))
import           Types                 as Import
import           Yesod.Form.Bootstrap3 as Import

import qualified Account
-- import           Breadcrumbs           as Import

-- TODO: Delete these. XXX
pkcloudSetMessageDanger :: MonadHandler m => Text -> m ()
pkcloudSetMessageDanger msg = setMessage [shamlet|<div >#{msg}|]
pkcloudSetMessageSuccess :: MonadHandler m => Text -> m ()
pkcloudSetMessageSuccess = pkcloudSetMessageDanger

-- TODO: Move to pkcloud-core XXX
eitherField :: RenderMessage site FormMessage => Text -> (FieldSettings site, Field (HandlerT site m) a) -> (FieldSettings site, Field (HandlerT site m) b) -> Field (HandlerT site m) (Either a b)
eitherField t (s1, f1) (s2, f2) = Field parse view UrlEncoded
    where
        -- parse ["left"]  = undefined
        parse ["on",_,v] x = fmap (fmap Right) <$> fieldParse f2 [v] x
        parse [v,_] x = fmap (fmap Left) <$> fieldParse f1 [v] x
        parse [v] x = fmap (fmap Left) <$> fieldParse f1 [v] x
        parse vs _  = return $ Left "Parse error"
        -- parse vs _  = error $ show vs

        view cssId name attrs res required = do
            langs <- languages
            site <- getYesod
            let leftTarget = cssId <> "-left-target"
            let rightTarget = cssId <> "-right-target"
            let leftId = maybe (cssId <> "-left-field") id $ fsId s1
            let rightId = maybe (cssId <> "-right-field") id $ fsId s2
            -- let leftName = maybe (name <> "-left-field") id $ fsName s1
            -- let rightName = maybe (name <> "-right-field") id $ fsName s2
            let leftName = name
            let rightName = name

            let (leftR, rightR) = 
                  let d = Left "" in -- TODO: What should this be? XXX
                  case res of
                    Left err -> (Left err, Left err)
                    (Right (Left l)) -> (Right l, d)
                    (Right (Right r)) -> (d, Right r)

            -- toWidget [julius|
            --     $('input[type=radio][data-toggle=collapse]').on( 'change', function() {
            --         // Check that radio is selected.
            --         if ( !this.checked)
            --             return;

            --         // Get target.
            --         var targetId = this.getAttribute('data-target');

            --         if ( !targetId) {
            --             console.error("Attribute 'data-target' not defined");
            --             return;
            --         }
            --         var target = $('#'+targetId);

            --         // Get parent.
            --         var parentId = this.getAttribute('data-parent');
            --         if ( !parentId) {
            --             console.error("Attribute 'data-parent' not defined");
            --             return;
            --         }
            --         var parent = $('#'+parentId);

            --         // Iterate over children.
            --         parent.children().each( function ( _, child) {
            --             var c = $( child);

            --             // Skip if controller.
            --             if ( c.hasClass("controller"))
            --                 return;

            --             // Expand if target.
            --             if ( c.attr('id') === targetId) {
            --                 c.collapse('show');

            --                 // If required, set required.
            --                 if ( #{required}) {
            --                     // Get data-required.
            --                     var requiredId = child.getAttribute('data-required');
            --                     if ( !requiredId) {
            --                         console.error("Attribute 'data-required' not defined");
            --                         return;
            --                     }

            --                     // Set required to true.
            --                     var required = $('[id^="'+requiredId+'"]');
            --                     required.prop( 'required', true);
            --                 }
            --             }
            --             // Hide otherwise.
            --             else {
            --                 // Hide if visible.
            --                 if ( c.hasClass("in")) {
            --                     c.collapse('hide');
            --                 }
            --                 // Otherwise set toggle to false.
            --                 else {
            --                     c.collapse({ 'toggle': false});
            --                 }

            --                 // If required, unset required.
            --                 if ( #{required}) {
            --                     // Get data-required.
            --                     var requiredId = child.getAttribute('data-required');
            --                     if ( !requiredId) {
            --                         console.error("Attribute 'data-required' not defined");
            --                         return;
            --                     }

            --                     // Set required to false.
            --                     var required = $('[id^="'+requiredId+'"]');
            --                     required.prop( 'required', false);
            --                 }
            --             }
            --         });
            --     });
            -- |]
            [whamlet|
                <div id="#{cssId}">
                    <div .controller .form-group>
                        <div .checkbox style="margin-top: 0px;">
                            <label>
                                <input type="checkbox" name="#{name}" id="#{cssId}-toggle" :not (isLeftSelected res):checked *{attrs} data-toggle="collapse" data-target="##{rightTarget},##{leftTarget}" data-parent="#{cssId}"> #{t}
                    <div id="#{leftTarget}" .form-group :isLeftSelected res:.in .collapse data-required="#{leftId}">
                        <label for="#{leftId}">#{fromMessage site langs s1}
                        ^{toView f1 s1 leftId leftName leftR required}
                    <div id="#{rightTarget}" .form-group :not (isLeftSelected res):.in .collapse data-required="#{rightId}" style="margin-bottom: 0px;">
                        <label for="#{rightId}">#{fromMessage site langs s2}
                        ^{toView f2 s2 rightId rightName rightR required}
            |]

        fromMessage site langs fs = renderMessage site langs $ fsLabel fs
        toView f s i n r req = fieldView f i n (fsAttrs s) r req

        isLeftSelected (Left "right") = False
        isLeftSelected (Right (Right _)) = False
        isLeftSelected _ = True

        -- leftSelected (Left "left") = True
        -- leftSelected (Left _) = False
        -- leftSelected (Right (Left _)) = True
        -- leftSelected (Right (Right _)) = False

        -- rightSelected (Left "right") = True
        -- rightSelected (Left _) = False
        -- rightSelected (Right (Left _)) = False
        -- rightSelected (Right (Right _)) = True

-- -- TODO: Move to pkcloud-core XXX
-- eitherField :: RenderMessage site FormMessage => (Text, FieldSettings site, Field (HandlerT site m) a) -> (Text, FieldSettings site, Field (HandlerT site m) b) -> Field (HandlerT site m) (Either a b)
-- eitherField (t1, s1, f1) (t2, s2, f2) = Field parse view UrlEncoded
--     where
--         -- parse ["left"]  = undefined
--         parse vs _  = error $ show vs
--         view cssId name attrs res required = do
--             langs <- languages
--             site <- getYesod
--             let leftTarget = cssId <> "-left-target"
--             let rightTarget = cssId <> "-right-target"
--             let leftId = maybe (cssId <> "-left-field") id $ fsId s1
--             let rightId = maybe (cssId <> "-right-field") id $ fsId s2
--             -- let leftName = maybe (name <> "-left-field") id $ fsName s1
--             -- let rightName = maybe (name <> "-right-field") id $ fsName s2
--             let leftName = name
--             let rightName = name
-- 
--             let (leftR, rightR) = 
--                   let d = Left "" in -- TODO: What should this be? XXX
--                   case res of
--                     Left err -> (Left err, Left err)
--                     (Right (Left l)) -> (Right l, d)
--                     (Right (Right r)) -> (d, Right r)
-- 
--             toWidget [julius|
--                 $('input[type=radio][data-toggle=collapse]').on( 'change', function() {
--                     // Check that radio is selected.
--                     if ( !this.checked)
--                         return;
-- 
--                     // Get target.
--                     var targetId = this.getAttribute('data-target');
-- 
--                     if ( !targetId) {
--                         console.error("Attribute 'data-target' not defined");
--                         return;
--                     }
--                     var target = $('#'+targetId);
-- 
--                     // Get parent.
--                     var parentId = this.getAttribute('data-parent');
--                     if ( !parentId) {
--                         console.error("Attribute 'data-parent' not defined");
--                         return;
--                     }
--                     var parent = $('#'+parentId);
-- 
--                     // Iterate over children.
--                     parent.children().each( function ( _, child) {
--                         var c = $( child);
-- 
--                         // Skip if controller.
--                         if ( c.hasClass("controller"))
--                             return;
-- 
--                         // Expand if target.
--                         if ( c.attr('id') === targetId) {
--                             c.collapse('show');
-- 
--                             // If required, set required.
--                             if ( #{required}) {
--                                 // Get data-required.
--                                 var requiredId = child.getAttribute('data-required');
--                                 if ( !requiredId) {
--                                     console.error("Attribute 'data-required' not defined");
--                                     return;
--                                 }
-- 
--                                 // Set required to true.
--                                 var required = $('[id^="'+requiredId+'"]');
--                                 required.prop( 'required', true);
--                             }
--                         }
--                         // Hide otherwise.
--                         else {
--                             // Hide if visible.
--                             if ( c.hasClass("in")) {
--                                 c.collapse('hide');
--                             }
--                             // Otherwise set toggle to false.
--                             else {
--                                 c.collapse({ 'toggle': false});
--                             }
-- 
--                             // If required, unset required.
--                             if ( #{required}) {
--                                 // Get data-required.
--                                 var requiredId = child.getAttribute('data-required');
--                                 if ( !requiredId) {
--                                     console.error("Attribute 'data-required' not defined");
--                                     return;
--                                 }
-- 
--                                 // Set required to false.
--                                 var required = $('[id^="'+requiredId+'"]');
--                                 required.prop( 'required', false);
--                             }
--                         }
--                     });
--                 });
--             |]
--             [whamlet|
--                 <div id="#{cssId}">
--                     <div .controller .form-group>
--                         <div class="radio" style="margin-top: 0px;">
--                             <label>
--                                 <input type="radio" name="#{name}" id="#{cssId}-left" value="left" *{attrs} data-toggle="collapse" data-parent="#{cssId}" data-target="#{leftTarget}" :leftSelected res:checked :required:required>
--                                 #{t1}
--                         <div class="radio" style="margin-top: 0px;">
--                             <label>
--                                 <input type="radio" name="#{name}" id="#{cssId}-right" value="right" *{attrs} data-toggle="collapse" data-parent="#{cssId}" data-target="#{rightTarget}" :rightSelected res:checked :required:required>
--                                 #{t2}
--                     <div id="#{leftTarget}" .form-group :not (leftSelected res):.collapse data-required="#{leftId}">
--                         <label for="#{leftId}">#{fromMessage site langs s1}
--                         ^{toView f1 s1 leftId leftName leftR required}
--                     <div id="#{rightTarget}" .form-group :not (rightSelected res):.collapse data-required="#{rightId}" style="margin-bottom: 0px;">
--                         <label for="#{rightId}">#{fromMessage site langs s2}
--                         ^{toView f2 s2 rightId rightName rightR required}
--             |]
-- 
--         fromMessage site langs fs = renderMessage site langs $ fsLabel fs
--         toView f s i n r req = fieldView f i n (fsAttrs s) r req
-- 
--         leftSelected (Left "left") = True
--         leftSelected (Left _) = False
--         leftSelected (Right (Left _)) = True
--         leftSelected (Right (Right _)) = False
-- 
--         rightSelected (Left "right") = True
--         rightSelected (Left _) = False
--         rightSelected (Right (Left _)) = False
--         rightSelected (Right (Right _)) = True

withFieldId :: Text -> FieldSettings a -> FieldSettings a
withFieldId fId fs = fs {fsId = Just fId}

-- TODO: Move to pkcloud-core, error messages, "None" for maybe results XXX
bootstrapRadioFieldList :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg) => [(msg, a)] -> Field (HandlerT site IO) a
bootstrapRadioFieldList l = (radioFieldList l) -- radioField $ optionsPairs l
    { 
        fieldView = \theId name attrs val req -> do
            let valF v = case val of
                  Left _ -> False
                  Right r -> r == v
            langs <- languages
            site <- getYesod
            mapM_ (\(c :: Int, (msg, v)) -> [whamlet|
                $newline never
                <div .radio style="margin-top: 0px;">
                    <label for="#{theId}-#{c}">
                        <input id="#{theId}-#{c}" *{attrs} type="radio" name=#{name} value=#{c} :valF v:checked> #{renderMessage site langs msg}
              |]) $ zip [1..] l
    }

-- JP: Timezone conversions?
dateField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m UTCTime
dateField = convertField (\d -> UTCTime d 0) utctDay dayField 

transactionQuery :: E.SqlQuery (E.SqlExpr (Entity Transaction), E.SqlExpr (Entity TransactionAccount), E.SqlExpr (E.Alias (E.Value (Maybe Nano))))
transactionQuery = E.from $ \(t `E.InnerJoin` ta) -> do
    E.on (t E.^. TransactionId E.==. ta E.^. TransactionAccountTransaction)
    E.orderBy [E.desc (t E.^. TransactionDate), E.desc (t E.^. TransactionId)]
    E.groupBy (t E.^. TransactionId, ta E.^. TransactionAccountId)
    s <- E.as $ E.over (E.sum_ (ta E.^. TransactionAccountAmount)) (Just $ ta E.^. TransactionAccountAccount) [E.asc (t E.^. TransactionDate), E.asc (t E.^. TransactionId)]
    return (t, ta, s)

entriesField :: forall m a . (ToBackendKey SqlBackend a, HandlerT App IO ~ m, RenderMessage App Text, a ~ Account) => [(Text, Key a)] -> [(Key a, Key a)] -> Field m [(Key a, Either Nano Nano)]
-- entriesField :: forall m a . (ToBackendKey SqlBackend a, HandlerT App IO ~ m, RenderMessage App Text) => [(Text, Key a)] -> Field m [(Entity a, Either Nano Nano)]
entriesField accounts shadows' = -- checkMMap toEntity (map toKey) $ 
    Field parse view UrlEncoded
    
    where
        shadows = Map.fromList $ map (\(k, v) -> (fromSqlKey k, fromSqlKey v)) shadows'

        -- TODO: Break cycle so we can create accounts ourself.
        -- accounts = Folder.treesToAccounts trees

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

        view theId name attrs val' isReq = do
            let val = either (const []) id val'
            let k = length val + 1
            toWidget [julius|
                var _addEntry = function() {
                    var k = #{intToJs k};
                
                    return function( i) {
                        // Make a fresh k.
                        k += 1;

                        var html = '<div id="#{textToJs theId}-'+k+'" data-row-number="'+k+'" class="form-inline">#{htmlToJs $ accountsH Nothing}<input name="#{textToJs name}" #{attrsToJs attrs} type="number" placeholder="Debit" value=""></input><input name="#{textToJs name}" #{attrsToJs attrs} type="number" placeholder="Credit" value=""></input><div class="btn-group" role="group"><button type="button" class="btn btn-default" aria-label="Remove" onclick="_removeEntry(\'#{textToJs theId}\',\''+k+'\')"><span class="glyphicon glyphicon-minus" /></button><button type="button" class="btn btn-default" aria-label="Add" onclick="_addEntry('+k+')"><span class="glyphicon glyphicon-plus" /></button></div></div>';

                        if ( i === undefined) {
                            $('##{textToJs theId}').prepend( html);
                        }
                        else {
                            $('##{textToJs theId}-' + i).after( html);
                        }

                        return $('##{textToJs theId}-' + k);
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

                var _selectionChanged = function() {
                    var parent = $('#'+'#{textToJs theId}');
                    var shadowMap = #{Aeson.toJSON shadows};

                    var getAccount = function( rowDiv) {
                        var i = rowDiv.children().eq(0);
                        return i.val();
                    };

                    var getDebitNode = function( rowDiv) {
                        return rowDiv.children().eq(1);
                    }

                    var getDebit = function( rowDiv) {
                        var i = getDebitNode( rowDiv);
                        return i.val();
                    };

                    var getCreditNode = function( rowDiv) {
                        return rowDiv.children().eq(2);
                    }

                    var getCredit = function( rowDiv) {
                        var i = getCreditNode( rowDiv);
                        return i.val();
                    };

                    var findRowM = function( targetAccountId) {
                      var rows = parent.children();
                      for (var i = 0; i < rows.length; i++) {
                        var rowDiv = rows.eq(i);
                        if ( targetAccountId.toString() === getAccount( rowDiv)) {
                          return rowDiv;
                        }
                      };
                    
                      return null;
                    }

                    var isShadowUnchanged = function( sel, shadowAccountId) {
                        var shadow = sel.shadow;
                        var shadowAccountId = sel.shadowAccountId;

                        // Check if shadow still exists.
                        if ( shadow === undefined || shadow === null || ! $.contains(document.body, shadow[0])) {
                            sel.shadow = null;

                            return false;
                        }

                        // Check that account, debit, and credit values are the same.
                        return shadowAccountId.toString() === getAccount( shadow) && sel.debit === getCredit( shadow) && sel.credit === getDebit( shadow);
                    }

                    return function( sel) {
                        var rowDiv = $("#"+sel.parentNode.id);
                        var i = rowDiv.attr("data-row-number");

                        // Remove previous shadow (if unchanged).
                        if ( sel.shadow) {
                            // Remove callbacks.
                            getDebitNode( rowDiv).unbind('input');
                            getCreditNode( rowDiv).unbind('input');

                            // Check if unchanged.
                            if ( isShadowUnchanged( sel)) {
                                sel.shadow.remove();

                                sel.shadow = null;
                            }
                        }

                        // Insert new shadow.
                        var shadowAccountId = shadowMap[sel.value];
                        if ( shadowAccountId !== undefined) {
                            // Check if row already exists.
                            sel.shadow = findRowM( shadowAccountId);
                            var exists = !(sel.shadow === null);

                            // Otherwise, insert it.
                            if ( !exists) {
                              // Add row.
                              sel.shadow = _addEntry( i);
                            }

                            sel.shadowAccountId = shadowAccountId;

                            // Get account.
                            var shadowAccountField = sel.shadow.children().first();

                            // Get debit.
                            var credit = getCredit( rowDiv);
                            var shadowDebitField = shadowAccountField.next();

                            // Get credit.
                            var debit = getDebit( rowDiv);
                            var shadowCreditField = shadowDebitField.next();

                            // Set values.
                            if ( !exists) {
                              shadowAccountField.val( shadowAccountId);
                              shadowDebitField.val( credit);
                              shadowCreditField.val( debit);
                            }

                            // Cache current values.
                            sel.credit = credit;
                            sel.debit = debit;

                            // Set onchange callbacks for debit/credit inputs.
                            var debitNode = getDebitNode( rowDiv);
                            debitNode.bind('input', function() {
                                // Get current debit value.
                                var currentVal = debitNode.val();

                                // Check if shadow is unchanged.
                                if ( isShadowUnchanged( sel)) {
                                    // Set shadow credit field.
                                    shadowCreditField.val( currentVal);
                                }

                                // Update cached value.
                                sel.debit = currentVal;
                            });

                            var creditNode = getCreditNode( rowDiv);
                            creditNode.bind('input', function() {
                                // Get current credit value.
                                var currentVal = creditNode.val();

                                // Check if shadow is unchanged.
                                if ( isShadowUnchanged( sel)) {
                                    // Set shadow debit field.
                                    shadowDebitField.val( currentVal);
                                }

                                // Update cached value.
                                sel.credit = currentVal;
                            });
                        }
                    }
                }();
            |]

            let rs = if null val then
                      let i = 1 :: Int in
                      rowW Nothing Nothing Nothing i
                  else
                      mapM_ (\((a, m), i) -> rowW (Just a) (either Just (const Nothing) m) (either (const Nothing) Just m) i) $ zip val [1..]

            [whamlet|
                <div id="#{theId}">
                    ^{rs}
            |]

          where
            rowW a dM cM i = 
                let d = maybe mempty show dM in
                let c = maybe mempty show cM in
                [whamlet|$newline never
                        <div id="#{theId}-#{i}" data-row-number="#{i}" .form-inline>
                            #{accountsH a}
                            <input name="#{name}" *{attrs} type="number" placeholder="Debit" value="#{d}" step="0.01">
                            <input name="#{name}" *{attrs} type="number" placeholder="Credit" value="#{c}" step="0.01">
                            <div .btn-group role="group">
                                <button type="button" class="btn btn-default" aria-label="Remove" onclick="_removeEntry( '#{theId}', '#{i}')">
                                    <span .glyphicon .glyphicon-minus>
                                <button type="button" class="btn btn-default" aria-label="Add" onclick="_addEntry('#{i}')">
                                    <span .glyphicon .glyphicon-plus>
                |]
                -- :isReq:required="" 
                        -- <div id="#{theId}-#{i}" .form-inline>
                            -- <div .btn-group role="group">
                        -- <div .input-group>

            accountsH :: Maybe AccountId -> Html
            accountsH accountIdM = 
                let accountV = maybe mempty (toMarkup . fromSqlKey) accountIdM in
                [shamlet|$newline never
                    <select name="#{name}" *{attrs} :isReq:required="" value="#{accountV}" onchange="_selectionChanged( this)">
                        ^{mapM_ (accountH accountIdM) accounts}
                |]

            accountH accountIdM (t, aId) = 
                let isSel = accountIdM == Just aId in
                [shamlet|$newline never
                    <option value="#{fromSqlKey aId}" :isSel:selected>#{t}
                |]

            intToJs = RawJavascript . TB.fromString . show
            textToJs = RawJavascript . TB.fromText
            htmlToJs = RawJavascript . TB.fromString . renderHtml -- TODO: This doesn't escape '
            attrsToJs = htmlToJs . TH.attrsToHtml


selectFieldKeysM :: (RenderMessage site FormMessage, RenderMessage site msg, Eq (Key a), PathPiece (Key a), ToBackendKey SqlBackend a) => [(msg, Maybe (Key a))] -> Field (HandlerFor site) (Maybe (Key a))
selectFieldKeysM = checkMMap keyToKeyM keyMToKey . selectFieldKeys . fmap (second keyMToKey)
  where
    keyMToKey = maybe (toSqlKey (-1)) id
    keyToKeyM k = if k < (toSqlKey 0) then 
        return (Right Nothing :: Either FormMessage (Maybe (Key a))) 
      else 
        return (Right $ Just k)

selectFieldKeys :: (RenderMessage site FormMessage, RenderMessage site msg, Eq (Key a), PathPiece (Key a)) => [(msg, Key a)] -> Field (HandlerFor site) (Key a)
selectFieldKeys = selectField . optionsPersistKey

  where
    optionsPersistKey pairs = fmap mkOptionList $ do
      mr <- getMessageRender
      return $ map (\(msg, key) -> Option
        { optionDisplay = mr msg
        , optionInternalValue = key
        , optionExternalValue = toPathPiece key
        }) pairs

-- TODO drop entriesId XXX
frequentTransationField :: forall m a . (ToBackendKey SqlBackend a, HandlerT App IO ~ m, RenderMessage App Text, a ~ FrequentTransaction) => [AccountTree] -> Key Book -> Text -> Text -> m (Field m (Key a))
frequentTransationField trees bookId descriptionId entriesId = runDB $ do
  fts <- selectList [FrequentTransactionBook ==. bookId] []

  ftas <- Map.fromList <$> mapM (\(Entity eId e) -> do
      ftas <- selectList [FrequentTransactionAccountTransaction ==. eId] []

      entries <- fmap (\(aId, v) -> case v of
          Left d -> (aId, d, 0)
          Right c -> (aId, 0, c)
        ) <$> lift (Account.transactionsToEntries trees ftas)

      return (fromSqlKey eId, Aeson.object ["description" .= frequentTransactionDescription e, "entries" .= entries])
    ) fts

  let field = selectFieldKeys $ map (\(Entity eId e) -> (frequentTransactionDescription e, eId)) fts

  return $ field { fieldView = view field ftas }

  where

    view field ftas theId name attrs val' isReq = do
      toWidget [julius|
        var _ = function() {
          var ftRows = [];
          var ftMap = #{Aeson.toJSON ftas}
          var frequentTransaction = $("##{textToJs theId}");
          var descriptionInput = $("##{textToJs descriptionId}");
          var entriesRow = $("##{textToJs entriesId}");
          frequentTransaction.on('change', function() {
            var ftId = frequentTransaction.val();

            // Remove any previously inserted rows (unless it's the last row).
            ftRows.map( function(r, ri) {
              if ( r.siblings().length > 0) {
                r.remove();
                delete ftRows[ri];
              }
            });

            // Check if none.
            var vt = ftMap[ftId];
            if ( vt === undefined) {
              return;
            }

            // Set description.
            descriptionInput.val( vt["description"]);

            // Remove any empty rows.
            entriesRow.children().map( function(_,c) {
              var fields = c.children;

              // Check if debit and credit are empty.
              if ( fields[1].value === "" && fields[2].value === "") {
                c.remove();
              }
            });

            // Insert rows.
            var prevNumber = undefined;
            vt["entries"].map( function(e) {
              var r = _addEntry( prevNumber);
              prevNumber = r.attr("data-row-number");

              var f = r.children().first();
              f.val(e[0]);
              
              f = f.next();
              if (e[1] > 0) {
                f.val(e[1]);
              }

              f = f.next();
              if (e[2] > 0) {
                f.val(e[2]);
              }

              ftRows.push( r);
            });

            // Set shadows.
            ftRows.map( function(r) {
              _selectionChanged(r[0].children[0]);
            });
          });
        }();
      |]
      fieldView field theId name attrs val' isReq

    textToJs = RawJavascript . TB.fromText



justFirst3 :: (a, b, c) -> (Maybe a, b, c)
justFirst3 (a,b,c) = (Just a, b, c)
