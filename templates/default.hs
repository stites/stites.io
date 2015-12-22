{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

numbers :: Int -> Html
numbers n = docTypeHtml $ do
  H.head $ do
    H.title "Natural numbers"
  body $ do
    p "A list of natural numbers:"
    ul $ forM_ [1..n] (li.toHtml)

simpleImage :: Html
simpleImage = img ! src "foo.png" ! alt "a foo image."

parentAttributes :: Html
parentAttributes = p ! class_ "styled" $ em "Context Here"

altParentAttributes :: Html
altParentAttributes = (p $ em "Context Here") ! class_ "styled"

data User = User { getUserName :: String, getPoints :: Int }

userInfo :: Maybe User -> Html
userInfo u = H.div ! A.id "user-info" $ case u of
  Nothing ->
    a ! href "/login" $ "Please login."
  Just user -> do
    "Logged in as "
    toHtml $ getUserName user
    ". Your points: "
    toHtml $ getPoints user




