{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import qualified Book
import Import

getHomeR :: Handler Html
getHomeR = do
    -- Get last opened book.
    bookM <- Book.getLastOpened

    case bookM of
        Nothing ->
            redirect BooksR
        Just bookId ->
            redirect $ BookR bookId


