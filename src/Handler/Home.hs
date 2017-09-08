{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    maybeUserId <- maybeAuthId
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        setTitle "PKCloud Accounts"
        [whamlet|
            <nav .navbar .navbar-default>
                <div .container>
                    <div .navbar-header>
                        <a .navbar-brand href=@{HomeR}>Home
                    <ul .nav .navbar-nav .navbar-right>
                        <li>
                            $maybe _ <- maybeUserId
                                <a href=@{AuthR LogoutR}>Logout
                            $nothing
                                <a href=@{AuthR LoginR}>Login
        |]

