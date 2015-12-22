{-# LANGUAGE OverloadedStrings #-}
import Clay
import Data.Text
import Prelude hiding (div)

main :: IO ()
main = putCss $ do
  bodyStyle
  headerStyle
  logoAnchorStyle
  headerNavigationStyle
  headerNavigationAnchorStyle
  footerStyle
  h1Style
  h2Style
  infoStyle

bodyStyle :: Css
bodyStyle = body ? do
  color    (black)
  fontSize (em 1.8)
  margin   (em 0) auto (em 0) auto
  width    (px 600)

headerStyle :: Css
headerStyle = div # header ? do
  borderBottom (px 2) solid black
  marginBottom (px 30)
  padding      (px 12) (px 0) (px 12) (px 0)

logoAnchorStyle :: Css
logoAnchorStyle = div # "logo" # a ? do
  color = black
  float = left
  fontSize = (px 18)
  fontWeight = bold
  textDecoration = none

headerNavigationStyle :: Css
headerNavigationStyle = div # header # "navigation" ? do
  textAlign = right

headerNavigationAnchorStyle :: Css
headerNavigationAnchorStyle = div # header # "navigation" # a ? do
  color = black
  fontSize = (px 18)
  fontWeight = bold
  marginLeft = (px 12)
  textDecoration = none
  textTransform = uppercase

footerStyle :: Css
footerStyle = div # footer ? do
  borderTop = solid (px 2) black
  color = #555
  fontSize = (px 12)
  marginTop = (px 30)
  padding = (px 12) (px 0) (px 12) (px 0)
  textAlign = right

h1Style :: Css
h1Style = h1 ? do
  fontSize = (px 24)

h2Style :: Css
h2Style = h2 ? do
  fontSize = (px 20)

infoStyle :: Css
infoStyle = div # "info" ? do
  color = #555
  fontSize = (px 14)
  fontStyle = italic

