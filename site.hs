{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (filterM)
import           Data.Maybe
import           Data.Monoid ((<>))
import           Debug.Trace
import           Hakyll
import           Text.Pandoc.Options
import           Data.Binary (Binary)
import           Data.Typeable (Typeable)

siteConfig :: Configuration
siteConfig = defaultConfiguration {
                deployCommand = "bash deploy.sh"
             }

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration {
                feedTitle       = "Simon - all articles"
              , feedDescription = "A personnal blog"
              , feedAuthorName  = "Simon Guillot"
              , feedRoot        = "http://notsimon.github.io"
              , feedAuthorEmail = "sgr.[last name]@gmail.com"
             }

main :: IO ()
main = hakyllWith siteConfig $ do
    match "media/*.dot" $ do
        route   $ setExtension "svg"
        compile dotCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["cv.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/article.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "articles/*" $ do
        route $ setExtension "html"

        let writerOptions = defaultHakyllWriterOptions {
                                writerHtml5 = True
                              , writerHTMLMathMethod = KaTeX "" ""
                            }
        compile $ pandocCompilerWith defaultHakyllReaderOptions writerOptions
            >>= loadAndApplyTemplate "templates/article.html"    postContext
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postContext
            >>= relativizeUrls

    match "talks/*" $ do
        route $ setExtension "html"

        let writerOptions = defaultHakyllWriterOptions {
                                writerSlideVariant = DZSlides
                              , writerHtml5 = True
                              , writerHTMLMathMethod = KaTeX "" ""
                            }

        compile $ pandocCompilerWith defaultHakyllReaderOptions writerOptions
            >>= loadAndApplyTemplate "templates/talk.html" postContext
            >>= relativizeUrls

    match "archive.html" $ do
        route idRoute
        compile $ do
            items <- loadPublishedPosts ("articles/*" .||. "talks/*")
            let archiveContext =
                    listField "items" postContext (return items)
                    <> defaultContext

            getResourceBody
                >>= applyAsTemplate archiveContext
                >>= loadAndApplyTemplate "templates/default.html" archiveContext
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            articles <- fmap (take 10) . recentFirst =<< loadAllSnapshots "articles/*" "content"
            let feedContext = postContext `mappend` bodyField "description"

            renderAtom feedConfig feedContext articles

    match "index.html" $ do
        route idRoute
        compile $ do
            articles <- fmap (take 3) $ loadPublishedPosts "articles/*"
            talks <- fmap (take 3) $ loadPublishedPosts "talks/*"
            let indexContext =
                    listField "articles" postContext (return articles)
                    <> listField "talks" postContext (return talks)
                    <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    match (foldl1 (.||.) ["media/**", "scripts/*", "robots.txt", "articles/**", "css/**"]) $ do
        route   idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateBodyCompiler

isDraft :: Item a -> Compiler Bool
isDraft item = do
    draft <- getMetadataField (itemIdentifier item) "draft"
    return $ fromMaybe False (draft >>= return . (== "true"))


loadPublishedPosts :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
loadPublishedPosts p = do
    posts <- recentFirst =<< loadAll p
    filterM (\item -> isDraft item >>= return . not) posts

postContext :: Context String
postContext =
    dateField "date" "%B %e, %Y" <> defaultContext

dotCompiler :: Compiler (Item String)
dotCompiler = getResourceString >>= withItemBody (unixFilter "dot" ["-Tsvg"])
