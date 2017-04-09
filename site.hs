{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid (mappend)
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

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "scripts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
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
                              writerHTMLMathMethod = MathJax ""
                            }
        compile $ pandocCompilerWith defaultHakyllReaderOptions writerOptions
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "talks/*" $ do
        route $ setExtension "html"

        let writerOptions = defaultHakyllWriterOptions {
                                writerSlideVariant = DZSlides
                              , writerHtml5 = True
                              , writerHTMLMathMethod = MathJax ""
                            }

        compile $ pandocCompilerWith defaultHakyllReaderOptions writerOptions
            >>= loadAndApplyTemplate "templates/talk.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 3) . recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

dotCompiler :: Compiler (Item String)
dotCompiler = getResourceString >>= withItemBody (unixFilter "dot" ["-Tsvg"])
