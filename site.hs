{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (filterM)
import           Data.Maybe
import           Data.Monoid ((<>))
import           Debug.Trace
import           Hakyll
import           Text.Pandoc.Options

siteConfig :: Configuration
siteConfig = defaultConfiguration {
                deployCommand = "bash deploy.sh"
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
            posts <- recentFirst =<< loadAll ("posts/*" .||. "talks/*")
            let archiveCtx =
                    listField "posts" postContext (return posts) <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 3) . recentFirst =<< loadAll ("posts/*" .||. "talks/*")
            publishedPosts <- filterM (\item -> isDraft item >>= return . not) posts
            let indexCtx =
                    listField "posts" postContext (return publishedPosts) <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match (foldl1 (.||.) ["images/*", "scripts/*", "robots.txt", "posts/**", "css/**"]) $ do
        route   idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateBodyCompiler

isDraft :: Item a -> Compiler Bool
isDraft item = do
    draft <- getMetadataField (itemIdentifier item) "draft"
    return $ fromMaybe False (draft >>= return . (== "true"))

postContext :: Context String
postContext =
    dateField "date" "%B %e, %Y" <> defaultContext

dotCompiler :: Compiler (Item String)
dotCompiler = getResourceString >>= withItemBody (unixFilter "dot" ["-Tsvg"])
