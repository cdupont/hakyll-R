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

buildRmd :: Rules ()
buildRmd = do
    match "*.Rmd" $ do
      route idRoute
      compile $ pandocRmdCompiler

--Compile the underlying Rmd file and returns its content
pandocRmdCompilerWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)
pandocRmdCompilerWith ropt wopt = do
   item <- getResourceBody
   if isRmd item
      then cached cacheName $ do
         fp <- getResourceFilePath
         unsafeCompiler $ saveDir $ do
            abfp <- canonicalizePath fp
            setCurrentDirectory (dropFileName abfp)
            -- convert Rmd to md
            mdContent <- rMarkdown (takeFileName abfp)
            -- get html file from md
            let html = (writePandocWith wopt (readMarkdown ropt <$> item {itemBody = mdContent}))
            -- make the html self-contained (imgs are embedded as data URIs)
            html' <- makeSelfContained wopt (itemBody html)
            --clean
            SD.removeDirectoryRecursive "figure"
            return $ item {itemBody = html'}
   else pandocCompilerWith ropt wopt where
            cacheName = "Rmd.pandocRmdCompilerWith"

pandocRmdCompiler :: Compiler (Item String)
pandocRmdCompiler = pandocRmdCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions


--get the markdown content from an R markdown file
rMarkdown :: FilePath -> IO (String)
rMarkdown fp = do
   (e,_,_) <- readProcessWithExitCode "R" ["--no-save","--quiet"] $ printf "library(knitr); knit('%s')" fp
   if (e==ExitSuccess)
      then do
         let nf = replaceExtension (takeFileName fp) "md"
         content <- readFile nf
         removeFile nf
         return content
      else error "Error while processing Rmd file"


isRmd :: Item a -> Bool
isRmd i = ex == ".Rmd"
  where
    ex = snd . splitExtension . toFilePath . itemIdentifier $ i

saveDir :: IO a -> IO a
saveDir m = do
    origDir <- getCurrentDirectory
    m <* setCurrentDirectory origDir
