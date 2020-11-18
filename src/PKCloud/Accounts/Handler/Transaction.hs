module PKCloud.Accounts.Handler.Transaction where

import qualified Database.Esqueleto as E

import qualified Account
import qualified Breadcrumb
import Import
import qualified Transaction

getTransactionR :: BookId -> TransactionId -> Handler Html
getTransactionR = Transaction.layout Breadcrumb.View $ \(Entity bookId _) (Entity transactionId transaction) entries accountTree -> do
    setTitle $ toHtml ("Transaction" :: Text)

    let ts = zip3 (repeat Nothing) entries (repeat (E.Value (Nothing :: (Maybe Nano))))

    [whamlet|
        <a class="btn btn-primary pull-right" href="@{TransactionEditR bookId transactionId}">
            Edit
        <a class="btn btn-danger pull-right" style="margin-right: 5px" href="@{TransactionDeleteR bookId transactionId}">
            Delete
        <h2>
            Transaction
        <form>
            <div .form-group>
                <label>
                    Description
                <div>
                    <p .form-control-static>
                        #{transactionDescription transaction}
            <div .form-group>
                <label>
                    Date
                <div>
                    <p .form-control-static>
                        #{shortDateTime (transactionDate transaction)}
            ^{createdByW transaction}
            ^{editedByW transaction}
            ^{editedDateW transaction}
            <div .form-group>
                <label>
                    Transactions
                <table .table .table-condensed .table-transactions>
                    <tr>
                        <th>
                            Account
                        <th>
                            Debit
                        <th>
                            Credit
                    ^{Account.displayTransactionRow accountTree bookId ts}
    |]

        -- TODO: Separate debits and credits. Lookup accounts from account tree? XXX
  
  where
    createdByW t = do
      user <- handlerToWidget $ runDB $ get404 $ transactionCreatedBy t
      
      [whamlet|
          <div .form-group>
              <label>
                  Created by
              <div>
                  <p .form-control-static>
                      #{userUsername user}
      |]
      -- JP: Link to user?

    editedByW t | Just userId <- transactionEditedBy t = do
      user <- handlerToWidget $ runDB $ get404 userId
        
      [whamlet|
          <div .form-group>
              <label>
                  Edited by
              <div>
                  <p .form-control-static>
                      #{userUsername user}
      |]
      -- JP: Link to user?
    editedByW _t | otherwise = mempty

    editedDateW t | Just date <- transactionEditedDate t = do
      [whamlet|
          <div .form-group>
              <label>
                  Edited on
              <div>
                  <p .form-control-static>
                      #{shortDateTime date}
      |]
    editedDateW _t | otherwise = mempty

