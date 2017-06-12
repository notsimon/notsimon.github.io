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
                feedTitle       = "Simon - all posts"
              , feedDescription = "A personnal blog"
              , feedAuthorName  = "Simon Guillot"
              , feedRoot        = "http://notsimon.github.io"
              , feedAuthorEmail = "sgr.[last name]@gmail.com"
             }

main :: IO ()
main = hakyllWith siteConfig $ do
    match "images/*.dot" $ do
        route   $ setExtension "svg"
        compile dotCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["cv.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"

        let writerOptions = defaultHakyllWriterOptions {
                                writerHtml5 = True
                              , writerHTMLMathMethod = KaTeX "" ""
                            }
        compile $ pandocCompilerWith defaultHakyllReaderOptions writerOptions
            >>= loadAndApplyTemplate "templates/post.html"    postContext
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

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- loadPublishedPosts
            let archiveContext =
                    listField "posts" postContext (return posts) <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveContext
                >>= loadAndApplyTemplate "templates/default.html" archiveContext
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            let feedContext = postContext `mappend` bodyField "description"

            renderAtom feedConfig feedContext posts

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 3) loadPublishedPosts
            let indexContext =
                    listField "posts" postContext (return posts) <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    match (foldl1 (.||.) ["images/*", "scripts/*", "robots.txt", "posts/**", "css/**"]) $ do
        route   idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateBodyCompiler

isDraft :: Item a -> Compiler Bool
isDraft item = do
    draft <- getMetadataField (itemIdentifier item) "draft"
    return $ fromMaybe False (draft >>= return . (== "true"))


loadPublishedPosts :: (Binary a, Typeable a) => Compiler [Item a]
loadPublishedPosts = do
    posts <- recentFirst =<< loadAll ("posts/*" .||. "talks/*")
    filterM (\item -> isDraft item >>= return . not) posts

postContext :: Context String
postContext =
    dateField "date" "%B %e, %Y" <> defaultContext

dotCompiler :: Compiler (Item String)
dotCompiler = getResourceString >>= withItemBody (unixFilter "dot" ["-Tsvg"])
