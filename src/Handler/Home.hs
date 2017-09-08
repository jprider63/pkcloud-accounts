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
    _ <- requireAuth
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
        setTitle "PKCloud Accounts"
        [whamlet|
            ^{alert}
            ^{navbar}
            <div .page-header>
                <h1>Welcome to PKCloud Accounts
        |]

alert :: Widget
alert = do
    mmsg <- getMessage
    [whamlet|
    $maybe msg <- mmsg
        <div .alert .alert-success>#{msg}
    |]
    toWidget [julius|window.setTimeout(function() {
        $(".alert-success").fadeTo(500, 0).slideUp(500, function(){
            $(this).remove();
        });
    }, 2000);
|]

navbar :: Widget
navbar = do
    [whamlet|
    <nav .navbar.navbar-default>
        <div .container>
            <div .navbar-header>
                <a .navbar-brand href=@{HomeR}>Home
            <ul .nav.navbar-nav.navbar-right>
                <li>
                    <a href=@{AuthR LogoutR}>Logout
|]