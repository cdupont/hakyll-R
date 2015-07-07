{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Hakyll.Web.R where

import Hakyll
import Text.Printf
import System.Process
import Control.Applicative
import System.FilePath
import System.Exit
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Web.Pandoc
import Text.Pandoc


buildRmd :: Rules ()
buildRmd = do
    match "*.Rmd" $ do
      route idRoute
      compile $ pandocRmdCompiler 

--Compile the underlying Rmd file and returns its content
pandocRmdCompilerWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)
pandocRmdCompilerWith ropt wopt = do
   i <- getResourceBody
   if isRmd i then do
         fp <- toFilePath <$> getUnderlying
         content <- unsafeCompiler $ rMarkdown fp 
   --   let nf = replaceExtension fp "md"
         return $ Item (fromFilePath fp) content
         let i' = i {itemBody = content}
         return (writePandocWith wopt (readMarkdown ropt <$> i'))
   else pandocCompilerWith ropt wopt

pandocRmdCompiler :: Compiler (Item String)
pandocRmdCompiler = pandocRmdCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions
   
  
--get the markdown content from an R markdown file 
rMarkdown :: FilePath -> IO String
rMarkdown fp = do
   putStrLn $ "fp=" ++ fp
   (e,_,_) <- readProcessWithExitCode "R" ["--no-save","--quiet"] $ printf "library(knitr); knit('%s')" fp 
   if (e==ExitSuccess)
      then do 
         let nf = replaceExtension (takeFileName fp) "md"
         readFile nf
      else error "Error while processing Rmd file"


isRmd :: Item a -> Bool
isRmd i = ex == ".Rmd"
  where
    ex = snd . splitExtension . toFilePath . itemIdentifier $ i

   --case parseString parseTree mempty rt of
   --    Success a -> return $ hsClassify a
   --    Failure a -> do
   --       print a
   --       error "parse failure" 
