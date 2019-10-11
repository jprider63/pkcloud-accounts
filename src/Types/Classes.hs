module Types.Classes where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Import -- .NoFoundation

-- class GeneralizedTransaction t where
--     gTransactionDescription :: t -> Text
-- 
--     gTransactionDate :: t -> Maybe UTCTime
-- 
--     gTransactionRoute :: BookId -> Key t -> Route App -- Maybe????
-- 
-- 
-- instance GeneralizedTransaction Transaction where
--     gTransactionDescription = transactionDescription
-- 
--     gTransactionDate = pure . transactionDate
-- 
--     gTransactionRoute = TransactionR
-- 
-- instance GeneralizedTransaction FrequentTransaction where
--     gTransactionDescription = frequentTransactionDescription
-- 
--     gTransactionDate _ = Nothing
-- 
--     gTransactionRoute = BookSettingsFrequentR


class GeneralizedTransactionAccount ta where
    gTransactionAccountAccount :: ta -> AccountId

    gTransactionAccountAmount :: ta -> Nano


instance GeneralizedTransactionAccount TransactionAccount where
    gTransactionAccountAccount = transactionAccountAccount

    gTransactionAccountAmount = transactionAccountAmount

instance GeneralizedTransactionAccount FrequentTransactionAccount where
    gTransactionAccountAccount = frequentTransactionAccountAccount

    gTransactionAccountAmount = frequentTransactionAccountAmount

