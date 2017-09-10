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
    userId <- requireAuthId
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
        setTitle "PKCloud Accounts"
        [whamlet|
            ^{alert}
            ^{navbar}
            <div .page-header>
                <h1>Welcome to PKCloud Accounts
            ^{books userId}
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

books :: UserId -> Widget 
books userId = do
    userBooks <- handlerToWidget $ runDB $ selectList [BookCreatedBy ==. userId] []
    [whamlet|
    <div .bs-docs-section>
        <div .row>
            <div .col-lg-12>
                <h2 #books>Books
            <div .col-lg-6>
                <div .bs-callout.bs-callout-info.well>
                    <ul #js-bookList>
                        $forall book <- userBooks
                            <li>#{bookName $ entityVal book}
                    <form #js-bookForm>
                        <div .field>
                            <input #js-bookName placeholder="New book name..." required>
                            <button .btn.btn-primary.btn-sm type=submit>Create book
    |]
    toWidget [julius|
        $("#js-bookForm").submit(function(event) {
            event.preventDefault();

            var name = $("#js-bookName").val();
            if (!name) {
                alert("Please enter a book name");
                return;
            }

            $.ajax({
                url: '@{BookR}',
                type: 'POST',
                contentType: "application/json",
                data: JSON.stringify({
                  name: name,
                  createdBy: 1,                 // Dummy values for FromJson
                  dateCreated: new Date()       // Is there another way?
                }),
                success: function (data) {
                    console.log(data);
                  var newNode = $("<li></li>");
                  newNode.text(data.name);
                  console.log(data);
                  $("#js-bookList").append(newNode);
                },
                error: function (data) {
                  console.log("Error creating book: " + JSON.stringify(data));
                },
            });
        });
    |]