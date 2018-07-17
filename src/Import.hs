module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import
import Yesod.Form.Bootstrap3 as Import

-- TODO: Delete these. XXX
pkcloudSetMessageDanger :: MonadHandler m => Text -> m ()
pkcloudSetMessageDanger msg = setMessage [shamlet|<div >#{msg}|]
pkcloudSetMessageSuccess :: MonadHandler m => Text -> m ()
pkcloudSetMessageSuccess = pkcloudSetMessageDanger
