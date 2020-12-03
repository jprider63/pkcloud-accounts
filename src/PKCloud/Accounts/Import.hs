module PKCloud.Accounts.Import where

import Data.Fixed (Nano)
import Prelude (Bool, Maybe)
import PKCloud.Import

type AccountId master = Key (Account master)
type BookId master = Key (Book master)
type FolderAccountId master = Key (FolderAccount master)
type FrequentTransactionId master = Key (FrequentTransaction master)
type TransactionId master = Key (Transaction master)

class (SubEntity (Book master), SubEntity (Account master), SubEntity (BookFolderAccount master), SubEntity (FolderAccount master), SubEntity (Transaction master), SubEntity (TransactionAccount master), SubEntity (FrequentTransaction master), SubEntity (FrequentTransactionAccount master)) => PKCloudAccounts master where

  type Book master = b | b -> master
  pkBook :: AuthId master -> Text -> UTCTime -> Book master
  pkBookIdField :: EntityField (Book master) (Key (Book master))
  pkBookCreatedBy :: Book master -> AuthId master
  pkBookName :: Book master -> Text
  pkBookDateCreated :: Book master -> UTCTime

  type Account master = t | t -> master
  pkAccountIdField :: EntityField (Account master) (Key (Account master))
  pkAccountName :: Account master -> Text
  pkAccountDateCreated :: Account master -> UTCTime
  pkAccountParent :: Account master -> (FolderAccountId master)
  pkAccountParentField :: EntityField (Account master) (FolderAccountId master)
  pkAccountShadow :: Account master -> Maybe (AccountId master)
  pkAccountFeatured :: Account master -> Bool

  type BookFolderAccount master = t | t -> master
  pkBookFolderAccountIdField :: EntityField (BookFolderAccount master) (Key (BookFolderAccount master))
  pkBookFolderAccountBook :: BookFolderAccount master -> BookId master
  pkBookFolderAccountBookField :: EntityField (BookFolderAccount master) (BookId master)
  pkBookFolderAccountFolder :: BookFolderAccount master -> FolderAccountId master
  pkBookFolderAccountFolderField :: EntityField (BookFolderAccount master) (FolderAccountId master)
  pkBookFolderAccountIsDebit :: BookFolderAccount master -> Bool
  pkBookFolderAccountIsDebitField :: EntityField (BookFolderAccount master) Bool
  pkBookFolderAccountUniqueBookFolder :: BookId master -> FolderAccountId master -> Unique (BookFolderAccount master)
  -- UniqueBookFolder book folder

  type FolderAccount master = t | t -> master
  pkFolderAccountIdField :: EntityField (FolderAccount master) (Key (FolderAccount master))
  pkFolderAccountName :: FolderAccount master -> Text
  pkFolderAccountParent :: FolderAccount master -> Maybe (FolderAccountId master)
  pkFolderAccountParentField :: EntityField (FolderAccount master) (Maybe (FolderAccountId master))

  type Transaction master = t | t -> master
  pkTransactionIdField :: EntityField (Transaction master) (Key (Transaction master))
  pkTransactionDescription :: Transaction master -> Text
  pkTransactionDate :: Transaction master -> UTCTime
  pkTransactionCreatedBy :: Transaction master -> AuthId master
  pkTransactionEditedBy :: Transaction master -> Maybe (AuthId master)
  pkTransactionEditedDate :: Transaction master -> Maybe UTCTime
  -- TODO: Add a PeriodId reference? XXX

  type TransactionAccount master = t | t -> master
  pkTransactionAccountIdField :: EntityField (TransactionAccount master) (Key (TransactionAccount master))
  pkTransactionAccountTransaction :: TransactionAccount master -> TransactionId master
  pkTransactionAccountAccount :: TransactionAccount master -> AccountId master
  pkTransactionAccountAccountField :: EntityField (TransactionAccount master) (AccountId master)
  pkTransactionAccountAmount :: TransactionAccount master -> Nano
  pkTransactionAccountAmountField :: EntityField (TransactionAccount master) Nano

  type FrequentTransaction master = t | t -> master
  pkFrequentTransactionBook :: FrequentTransaction master -> BookId master
  pkFrequentTransactionDescription :: FrequentTransaction master -> Text
  pkFrequentTransactionCreatedBy :: FrequentTransaction master -> AuthId master

  type FrequentTransactionAccount master = t | t -> master
  pkFrequentTransactionAccountTransaction :: FrequentTransactionAccount master -> FrequentTransactionId master
  pkFrequentTransactionAccountAccount :: FrequentTransactionAccount master -> AccountId master
  pkFrequentTransactionAccountAmount :: FrequentTransactionAccount master -> Nano

-- Book json
--   createdBy UserId
--   name Text
--   dateCreated UTCTime
--   -- UniqueBook createdBy name
-- 
-- Account
--   name Text
--   dateCreated UTCTime
--   parent FolderAccountId
--   shadow AccountId Maybe
--   featured Bool
-- 
-- BookFolderAccount
--   book BookId
--   folder FolderAccountId
--   isDebit Bool
--   UniqueBookFolder book folder
-- 
-- FolderAccount -- JP: Rename to Folder?
--   name Text
--   parent FolderAccountId Maybe
-- 
-- Transaction
--   description Text
--   date UTCTime
--   createdBy UserId
--   editedBy UserId Maybe
--   editedDate UTCTime Maybe
--   -- TODO: Add a PeriodId reference? XXX
-- 
-- TransactionAccount
--   transaction TransactionId
--   account AccountId
--   amount Nano
-- 
-- FrequentTransaction
--   book BookId
--   description Text
--   createdBy UserId
-- 
-- FrequentTransactionAccount
--   transaction FrequentTransactionId
--   account AccountId
--   amount Nano

