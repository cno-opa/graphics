# OPA graphics functions

These are some helpful functions and a ggplot theme for making charts in the OPA key. Use this repo as the authoritative source for these charting functions. Meaning, push any improvements you make to this repo so the charts the office makes are standardized.

Pull these functions into your script with:

```r
source_https <- function(u, unlink.tmp.certs = FALSE) {
    require(RCurl)

    if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
    script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
    if(unlink.tmp.certs) unlink("cacert.pem")

    eval(parse(text = script), envir= .GlobalEnv)
}
```
 Then:

 ```r
 source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/WHICHEVER-FILE-YOU-WANT")
 ```

 See how [the stat template](https://github.com/cno-opa/stat-template) incorporates this into `main.R`

Thanks to [Tony Breyal for the above function](https://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/).


For mappers.R, see this [tutorial](http://rpubs.com/djknaggs/mappers-tutorial).
