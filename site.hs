{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid ((<>), mappend)
import Hakyll
import Data.Binary
import Data.Typeable
import Control.Monad

main :: IO ()
main = hakyll $ do
  staticRules
  metaRules
  postRules
  archiveRules
  indexRules
  draftRules
  draftListRules
  match "templates/*" $ compile templateBodyCompiler

  where
    staticRules :: Rules ()
    staticRules = do
      "images/*" `matchWith` copyFileCompiler
      "css/*"    `matchWith` compressCssCompiler

    metaRules :: Rules ()
    metaRules =
      fromList ["about.md", "contact.md"] `match2htmlWith`
        (loadAndApplyTemplate' "default" defaultContext >=> relativizeUrls)


    postRules :: Rules ()
    postRules = "posts/*" `match2htmlWith`
      (   loadAndApplyTemplate' "post"    postCtx
      >=> loadAndApplyTemplate' "default" postCtx
      >=> relativizeUrls)

    archiveRules :: Rules ()
    archiveRules = create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        makeItem ""
          >>= loadAndApplyTemplate' "archive" (posts `listCtxCalled` "Archives")
          >>= loadAndApplyTemplate' "default" (posts `listCtxCalled` "Archives")
          >>= relativizeUrls


    indexRules :: Rules ()
    indexRules = "index.html" `matchWith` do
      posts <- recentFirst =<< loadAll "posts/*"
      getResourceBody
        >>= applyAsTemplate                 (posts `listCtxCalled` "Home")
        >>= loadAndApplyTemplate' "default" (posts `listCtxCalled` "Home")
        >>= relativizeUrls


    draftRules :: Rules ()
    draftRules = "drafts/*" `match2htmlWith`
      (   loadAndApplyTemplate' "post"    (dateField "date" "" <> defaultContext)
      >=> loadAndApplyTemplate' "default" (dateField "date" "" <> defaultContext)
      >=> relativizeUrls)

    draftListRules :: Rules ()
    draftListRules = create ["drafts.html"] $ do
      route idRoute
      compile $ do
        drafts <- recentFirst =<< loadAll "drafts/*"
        makeItem ""
          >>= loadAndApplyTemplate' "archive" (draftListCtx drafts)
          >>= loadAndApplyTemplate' "default" (draftListCtx drafts)
          >>= relativizeUrls
      where
        draftListCtx :: [Item String] -> Context String
        draftListCtx posts =
          listField "posts" (dateField "date" "" <> defaultContext) (pure posts)
          <> constField "title" "Drafts"
          <> defaultContext


matchWith :: (Writable a, Typeable a, Binary a) => Pattern -> Compiler (Item a) -> Rules ()
matchWith patt comp =
  match patt $ do
    route   idRoute
    compile comp

match2htmlWith :: (Writable a, Typeable a, Binary a) => Pattern -> (Item String -> Compiler (Item a)) -> Rules ()
match2htmlWith patt comp =
  match patt $ do
    route $ setExtension "html"
    compile $ pandocCompiler >>= comp



loadAndApplyTemplate' :: String -> Context a -> Item a -> Compiler (Item String)
loadAndApplyTemplate' t = loadAndApplyTemplate (fromFilePath $ "templates/"<> t <>".html")


--------------------------------------------------------------------------------


listCtxCalled :: [Item String] -> String -> Context String
listCtxCalled posts title =
  listField "posts" listItem (pure posts)
  <> constField "title" title
  <> defaultContext
  where
    listItem :: Context String
    listItem = dateField "date" "%D" <> defaultContext


postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
  <> defaultContext
