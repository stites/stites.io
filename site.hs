--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath ((</>), (<.>), splitExtension)

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
cssRoutes = do
  route $ setExtension "css"
  compile $ getResourceString >>= withItemBody (unixFilter "stack" ["runghc"])

imagesRoutes = do
  route   idRoute
  compile copyFileCompiler

metaRoutes = do
  route $ setExtension "html" `composeRoutes` appendIndex
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/default.html" defaultContext
    >>= relativizeUrls

appendIndex
postsRoutes = do
  route $ setExtension "html" `composeRoutes` appendIndex
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/post.html"    postCtx
    >>= saveSnapshot "content"
    >>= loadAndApplyTemplate "templates/default.html" postCtx
    >>= relativizeUrls

archivePage = do
  route idRoute
  compile $ do
    posts <- recentFirst =<< loadAll "posts/*"
    let archiveCtx = listField "posts" postCtx (return posts) `mappend`
                     constField "title" "Archives"            `mappend`
                     defaultContext
    makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

indexPage = do
  route idRoute
  compile $ do
    posts <- return.take 10 =<< recentFirst =<< loadAll "posts/*"
    let indexCtx = listField "posts" postCtx (return posts) `mappend`
                   constField "title" "Home"                `mappend`
                   defaultContext
    getResourceBody
      >>= applyAsTemplate indexCtx
      >>= loadAndApplyTemplate "templates/default.html" indexCtx
      >>= relativizeUrls

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
extensionless = customRoute $ takeWhile ((/=) '.') . toFilePath

appendIndex = customRoute $ (\(p, e)-> p </> "index" <.> e).splitExtension.toFilePath

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
