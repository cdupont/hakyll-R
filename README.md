Hakyll-R
================

Hakyll-R is a library for Hakyll allowing to process blog posts written in R-markdown.

Usage
=====

Add a rule in your Hakyll blog to process Rmd files:

```
buildRmd :: Rules ()
buildRmd = do
    match "*.Rmd" $ do
      route idRoute
      compile $ pandocRmdCompiler
```


 
