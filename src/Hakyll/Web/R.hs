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
pandocCompilerRWith ropt wopt = pandocCompilerWithTransformM ropt wopt (rTransformer "images")

rTransformer :: FilePath -> Pandoc -> Compiler Pandoc
rTransformer plotsDir pandoc = unsafeCompiler $ renderRPandoc plotsDir pandoc
