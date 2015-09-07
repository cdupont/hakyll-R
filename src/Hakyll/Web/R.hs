{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Hakyll.Web.R where

import Hakyll
import Text.Printf
import System.Process
import Control.Applicative
import System.FilePath
import System.Exit
import Text.Pandoc
import System.Directory as SD
import Text.Pandoc.SelfContained
import Text.Pandoc.R


buildRmd :: Rules ()
buildRmd = do
    match "*.md" $ do
      route idRoute
      compile $ pandocCompilerR

-- | Read a page render using pandoc
pandocCompilerR :: Compiler (Item String)
pandocCompilerR = pandocCompilerRWith defaultHakyllReaderOptions defaultHakyllWriterOptions

pandocCompilerRWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)
pandocCompilerRWith ropt wopt = pandocCompilerWithTransformM ropt wopt rTransformer

rTransformer :: Pandoc -> Compiler Pandoc
rTransformer pandoc = unsafeCompiler $ renderRPandoc pandoc


-- -- Compile the underlying Rmd file and returns its content as HTML
-- pandocRmdCompilerWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)
-- pandocRmdCompilerWith ropt wopt = do
--    item <- getResourceBody
--    if isRmd item
--       then cached cacheName $ do
--          fp <- getResourceFilePath
--          unsafeCompiler $ saveDir $ do
--             abfp <- canonicalizePath fp
--             setCurrentDirectory (dropFileName abfp)
--             -- convert Rmd to md
--             mdContent <- convertRmdTomd (takeFileName abfp)
--             -- get html file from md
--             let (Item _ epandoc) = readMarkdown ropt <$> item {itemBody = mdContent}
--             case epandoc of
--                Right pandoc -> do
--                   let html = writePandocWith wopt item {itemBody = pandoc}
--                   -- make the html self-contained (imgs are embedded as data URIs)
--                   html' <- makeSelfContained wopt (itemBody html)
--                   --clean
--                   SD.removeDirectoryRecursive "figure"
--                   return $ item {itemBody = html'}
--                Left e -> error $ show e
--    else pandocCompilerWith ropt wopt where
--             cacheName = "Rmd.pandocRmdCompilerWith"
--
-- pandocRmdCompiler :: Compiler (Item String)
-- pandocRmdCompiler = pandocRmdCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions
--
--
-- --get the markdown content from an R markdown file
-- convertRmdTomd :: FilePath -> IO (String)
-- convertRmdTomd fp = do
--    (e,_,_) <- readProcessWithExitCode "R" ["--no-save","--quiet"] $ printf "library(knitr); knit('%s')" fp
--    if (e==ExitSuccess)
--       then do
--          let nf = replaceExtension (takeFileName fp) "md"
--          content <- readFile nf
--          removeFile nf
--          return content
--       else error "Error while processing Rmd file"
--
--
-- isRmd :: Item a -> Bool
-- isRmd i = ex == ".Rmd"
--   where
--     ex = snd . splitExtension . toFilePath . itemIdentifier $ i
--
-- saveDir :: IO a -> IO a
-- saveDir m = do
--     origDir <- getCurrentDirectory
--     m <* setCurrentDirectory origDir
