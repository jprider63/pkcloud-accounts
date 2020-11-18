module Types.Classes where

import Import.NoFoundation
import PKCloud.Accounts.Import

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


class GeneralizedTransactionAccount master ta where
    gTransactionAccountAccount :: ta -> (AccountId master)

    gTransactionAccountAmount :: ta -> Nano


instance (TransactionAccount master ~ ta) => GeneralizedTransactionAccount master ta where
    gTransactionAccountAccount = pkTransactionAccountAccount

    gTransactionAccountAmount = pkTransactionAccountAmount

instance (FrequentTransactionAccount master ~ fta) => GeneralizedTransactionAccount fta where
    gTransactionAccountAccount = frequentTransactionAccountAccount

    gTransactionAccountAmount = frequentTransactionAccountAmount

