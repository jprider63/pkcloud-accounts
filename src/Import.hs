module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import
import Text.Blaze
import Yesod.Form.Bootstrap3 as Import

-- TODO: Delete these. XXX
pkcloudSetMessageDanger :: MonadHandler m => Text -> m ()
pkcloudSetMessageDanger msg = setMessage [shamlet|<div >#{msg}|]
pkcloudSetMessageSuccess :: MonadHandler m => Text -> m ()
pkcloudSetMessageSuccess = pkcloudSetMessageDanger

-- TODO: Move to pkcloud-core XXX
eitherField :: RenderMessage site FormMessage => (Text, FieldSettings site, Field (HandlerT site m) a) -> (Text, FieldSettings site, Field (HandlerT site m) b) -> Field (HandlerT site m) (Either a b)
eitherField (t1, s1, f1) (t2, s2, f2) = Field parse view UrlEncoded
    where
        parse _ _ = undefined
        view cssId name attrs res required = do
            -- accordion <- handlerToWidget newIdent
            langs <- languages
            site <- getYesod
            let leftId = maybe (cssId <> "-left-target") id $ fsId s1
            let rightId = maybe (cssId <> "-right-target") id $ fsId s2
            let leftName = maybe (name <> "-left-target") id $ fsName s1
            let rightName = maybe (name <> "-right-target") id $ fsName s2
            -- let (leftReq, rightReq) = case res of
            --       Left err -> (required, False)
            --       (Right (Left _)) -> (required, False)
            --       (Right (Right _)) -> (False, required)

            let (leftR, rightR) = 
                  let d = Left "TODO" in
                  case res of
                    Left err -> (Left err, Left err)
                    (Right (Left l)) -> (Right l, d)
                    (Right (Right r)) -> (d, Right r)
            [whamlet|
                <div id="#{cssId}" .form-group>
                    <div class="radio">
                        <label>
                            <input type="radio" name="#{name}" id="#{cssId}-left" value="left" *{attrs} data-toggle="collapse" data-parent="#{cssId}" data-target="#{leftId}" :leftSelected res:checked :required:required>
                            #{t1}
                    <div class="radio">
                        <label>
                            <input type="radio" name="#{name}" id="#{cssId}-right" value="right" *{attrs} data-toggle="collapse" data-parent="#{cssId}" data-target="#{cssId}-right" :leftSelected res:checked :required:required>
                            #{t2}
                <div .form-group>
                    <label for="#{leftId}">#{fromMessage site langs s1}
                    ^{toView f1 s1 leftId leftName leftR required}
                <div .form-group>
                    <label for="#{rightId}">#{fromMessage site langs s2}
                    ^{toView f2 s2 rightId rightName rightR required}
            |]

        fromMessage site langs fs = renderMessage site langs $ fsLabel fs
        toView f s i n r req = fieldView f i n (fsAttrs s) r req

        leftSelected (Left "left") = True
        leftSelected (Left _) = False
        leftSelected (Right (Left _)) = True
        leftSelected (Right (Right _)) = False

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
                <div .radio>
                    <label>
                        <input id="#{theId}-#{c}" *{attrs} type="radio" name=#{name} value=#{c} :valF v:checked> #{renderMessage site langs msg}
              |]) $ zip [1..] l
    }

