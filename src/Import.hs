module Import
    ( module Import
    ) where

import qualified Database.Esqueleto as E
import Foundation            as Import
import Import.NoFoundation   as Import
import Text.Blaze
import Types                 as Import
import Yesod.Form.Bootstrap3 as Import

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
