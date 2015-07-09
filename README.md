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


Limitations
===========

Hakyll-R will embbed all images in your posts as Data-URIs directly inside the HTML, including R-generated images and normal images.
For that to work, all the images in your Rmd file should have paths relative to your post (i.e. ../../images/pic.png) instead of absolute paths (i.e. /images/pic.png).
 
