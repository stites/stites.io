--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid     (mappend, (<>))
import Hakyll
import System.FilePath ( (</>) , (<.>)
                       , splitExtension
                       , splitFileName
                       , takeDirectory )

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- static content
    match "images/*"        $ imagesRoutes
    match "css/*.hs"        $ cssRoutes
    match "posts/*"         $ postsRoutes
    match "index.html"      $ indexPage

    -- generated
    match (fromList ["about.md", "contact.md"]) $ metaRoutes
    create ["archive.html"] $ archivePage
    match "templates/*"     $ compile templateCompiler
    create ["atom.xml"]     $ rssFeed

--------------------------------------------------------------------------------
cssRoutes :: Rules ()
cssRoutes = do
  route $ setExtension "css"
  compile $ getResourceString >>= withItemBody (unixFilter "stack" ["runghc"])

imagesRoutes :: Rules ()
imagesRoutes = do
  route   idRoute
  compile copyFileCompiler

metaRoutes :: Rules ()
metaRoutes = do
  route $ setExtension "html" `composeRoutes` appendIndex
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/default.html" defaultContext
    >>= relativizeUrls

postsRoutes :: Rules ()
postsRoutes = do
  route $ setExtension "html" `composeRoutes` appendIndex
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/post.html"    postCtx
    >>= saveSnapshot "content"
    >>= loadAndApplyTemplate "templates/default.html" postCtx
    >>= relativizeUrls

archivePage :: Rules ()
archivePage = do
  route $ setExtension "html" `composeRoutes` appendIndex
  compile $ do
    posts <- recentFirst =<< loadAll "posts/*"
    let siteCtx = dropIndexHtml "url" <> defaultContext
    let archiveCtx = listField "posts" postCtx (return posts) <>
                     constField "title" "Archives"            <>
                     defaultContext
    makeItem "" >>= loadAndApplyTemplate "templates/archive.html" siteCtx
                >>= loadAndApplyTemplate "templates/default.html" siteCtx
                >>= relativizeUrls

indexPage :: Rules ()
indexPage = do
  route idRoute
  compile $ do
    posts <- return.take 10 =<< recentFirst =<< loadAll "posts/*"
    let indexCtx = listField "posts" postCtx (return posts) <>
                   constField "title" "Home"                <>
                   defaultContext
    getResourceBody
      >>= applyAsTemplate indexCtx
      >>= loadAndApplyTemplate "templates/default.html" indexCtx
      >>= relativizeUrls

rssFeed :: Rules ()
rssFeed = do
  route idRoute
  compile $ do
    let feedCtx = postCtx `mappend` bodyField "description"
    posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
    renderAtom feedConfig feedCtx posts
  where feedConfig = FeedConfiguration {
          feedTitle       = "Programming Notes"
        , feedDescription = "Programming Notes"
        , feedAuthorName  = "Sam Stites"
        , feedAuthorEmail = "sam@stites.io"
        , feedRoot        = "http://www.stites.io"
        }

--------------------------------------------------------------------------------
extensionless :: Routes
extensionless = customRoute $ takeWhile ((/=) '.') . toFilePath

appendIndex :: Routes
appendIndex = customRoute $ (\(p, e)-> p </> "index" <.> e).splitExtension.toFilePath

dropIndexHtml :: String -> Context a
dropIndexHtml key = mapContext transform (urlField key)
  where transform url = case splitFileName url of
          (p, "index.html") -> takeDirectory p
          _                 -> url
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    dropIndexHtml "url"          <>
    defaultContext


