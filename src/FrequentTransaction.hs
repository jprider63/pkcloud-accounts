module FrequentTransaction where

import Import

layout :: (Entity Book -> Entity FrequentTransaction -> [Entity FrequentTransactionAccount] -> [AccountTree] -> Widget) -> BookId -> FrequentTransactionId -> Handler Html
layout = error "TODO"

