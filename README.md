Hakyll-R
========

Hakyll-R is a library for Hakyll allowing to process blog posts written in R-markdown.

Installation
============

You need to have R and knitr installed:

```
$ sudo apt-get install r-base-dev
$ R
> install.packages("knitr")
```

Install it with:

```
cabal install hakyll-R
```

Usage
=====

Add a rule in your Hakyll blog to process Rmd files:

```
import Hakyll.Web.R

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
 
