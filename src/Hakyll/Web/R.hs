{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Hakyll.Web.R where

import Hakyll
import Text.Printf
import System.Process
import Control.Applicative
import System.FilePath
import System.Exit
import Text.Pandoc
import System.Directory

buildRmd :: Rules ()
buildRmd = do
    match "*.Rmd" $ do
      route idRoute
      compile $ pandocRmdCompiler

--Compile the underlying Rmd file and returns its content
pandocRmdCompilerWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)
pandocRmdCompilerWith ropt wopt = do
   i <- getResourceBody
   if isRmd i
      then cached cacheName $ do
         fp <- getResourceFilePath
         unsafeCompiler $ saveDir $ do
            abfp <- canonicalizePath fp
            setCurrentDirectory (dropFileName abfp)
            s <- rMarkdown (takeFileName abfp)
            let i' = i {itemBody = s}
            return (writePandocWith wopt (readMarkdown ropt <$> i'))
   else pandocCompilerWith ropt wopt where
    cacheName = "Rmd.pandocRmdCompilerWith"


pandocRmdCompiler :: Compiler (Item String)
pandocRmdCompiler = pandocRmdCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions


--get the markdown content from an R markdown file
rMarkdown :: FilePath -> IO String
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
