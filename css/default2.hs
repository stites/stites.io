{-# LANGUAGE OverloadedStrings #-}
import Clay
import Data.Text
import Prelude hiding (div, (!))

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
  fontFamily  ["Helvetica Neue"] [sansSerif]

siteHeader :: Selector
siteHeader = div # "#header"

headerStyle :: Css
headerStyle = siteHeader ? do
  borderBottom solid (px 2) black
  marginBottom (px 30)
  padding      (px 12) (px 0) (px 12) (px 0)

logoAnchor :: Selector
logoAnchor = div # "#logo" # "a"

logoAnchorStyle :: Css
logoAnchorStyle = logoAnchor ? do
  color            black
  float            floatLeft
  fontSize         (px 18)
  fontWeight       bold
  textDecoration   none

headerNavigation :: Selector
headerNavigation = siteHeader # "navigation"

headerNavigationStyle :: Css
headerNavigationStyle = headerNavigation ? do
  textAlign        (alignString 'r')

headerNavigationAnchor :: Selector
headerNavigationAnchor = headerNavigation # "a"

headerNavigationAnchorStyle :: Css
headerNavigationAnchorStyle = headerNavigationAnchor ? do
  color           black
  fontSize        (px 18)
  fontWeight      bold
  marginLeft      (px 12)
  textDecoration  none
  textTransform   uppercase

footerStyle :: Css
footerStyle = div # "#footer" ? do
  borderTop  solid (px 2) black
  color      "#555"
  fontSize   (px 12)
  marginTop  (px 30)
  padding    (px 12) (px 0) (px 12) (px 0)
  textAlign  (alignString 'l')

h1Style :: Css
h1Style = h1 ? do
  fontSize  (px 24)

h2Style :: Css
h2Style = h2 ? do
  fontSize  (px 20)

info :: Selector
info = div # ".info"

infoStyle :: Css
infoStyle = info ? do
  color  "#555"
  fontSize  (px 14)
  fontStyle  italic

