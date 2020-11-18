{-# LANGUAGE CPP #-}
module Import.NoFoundation
    ( module Import
    , dollar
    , shortDateTime
    ) where

-- import           ClassyPrelude.Yesod   as Import hiding (getCurrentTime, selectFieldList, breadcrumbs)
-- import qualified ClassyPrelude.Yesod as Y
import           Data.Fixed            as Import (Nano, Centi, Fixed(..))
import           Data.Time.Format      (formatTime, defaultTimeLocale)
import           Prelude               (div, String, (^), show)
import           PKCloud.Import        as Import
-- import           Database.Persist.Sql  as Import (fromSqlKey, toSqlKey)
-- import           Settings              as Import
-- import           Settings.StaticFiles  as Import
-- import           Yesod.Auth            as Import
-- import           Yesod.Core.Types      as Import (loggerSet)
-- import           Yesod.Default.Config2 as Import

dollar :: Nano -> String
dollar d = '$':show (toCenti d)
    where
        -- JP: Negatives? 
        toCenti :: Nano -> Centi
        toCenti (MkFixed x) = MkFixed (x `div` 10^7)

shortDateTime :: UTCTime -> String
shortDateTime = formatTime defaultTimeLocale "%D" -- "%D %H:%M %P"

-- longDateTime :: UTCTime -> String
-- longDateTime = formatTime defaultTimeLocale "%D %H:%M %P"

-- getCurrentTime :: MonadIO m => m UTCTime
-- getCurrentTime = liftIO getCurrentTime

