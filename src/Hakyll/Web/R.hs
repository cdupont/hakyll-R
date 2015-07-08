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
import System.Directory
import qualified Data.ByteString as BS
import Data.Binary
import GHC.Generics (Generic)
import Data.Typeable

buildRmd :: Rules ()
buildRmd = do
    match "*.Rmd" $ do
      route idRoute
      compile $ pandocRmdCompiler

data RmdData = RmdData String [BS.ByteString]
   deriving (Typeable, Generic)

instance Binary RmdData
instance Writable RmdData where
    write path item = do
       let (RmdData s fs) = itemBody item
       writeFile path s
       mapM_ writeFigure (zip fs [1..]) where
     writeFigure (f, i) =  BS.writeFile ((dropFileName path) ++ "plots-" ++ (show i) ++ ".png") f

--Compile the underlying Rmd file and returns its content
pandocRmdCompilerWith :: ReaderOptions -> WriterOptions -> Compiler (Item RmdData)
pandocRmdCompilerWith ropt wopt = do
   i <- getResourceBody
   if isRmd i
      --then cached cacheName $ do
      then do
         fp <- getResourceFilePath
         unsafeCompiler $ saveDir $ do
            abfp <- canonicalizePath fp
            setCurrentDirectory (dropFileName abfp)
            (s, fs) <- rMarkdown (takeFileName abfp)
            let i' = i {itemBody = s}
            let is = (writePandocWith wopt (readMarkdown ropt <$> i'))
            return $ fmap (\a -> RmdData a []) is
   else do
   is <- pandocCompilerWith ropt wopt

   return $ fmap (\a -> RmdData a []) is where
    cacheName = "Rmd.pandocRmdCompilerWith"


pandocRmdCompiler :: Compiler (Item RmdData)
pandocRmdCompiler = pandocRmdCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions


--get the markdown content from an R markdown file
rMarkdown :: FilePath -> IO (String, [FilePath])
rMarkdown fp = do
   (e,_,_) <- readProcessWithExitCode "R" ["--no-save","--quiet"] $ printf "library(knitr); knit('%s')" fp
   if (e==ExitSuccess)
      then do
         let nf = replaceExtension (takeFileName fp) "md"
         content <- readFile nf
         removeFile nf
         return (content, [])
      else error "Error while processing Rmd file"


isRmd :: Item a -> Bool
isRmd i = ex == ".Rmd"
  where
    ex = snd . splitExtension . toFilePath . itemIdentifier $ i

saveDir :: IO a -> IO a
saveDir m = do
    origDir <- getCurrentDirectory
    m <* setCurrentDirectory origDir
