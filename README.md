WikipediR
=========

An R API wrapper for MediaWiki, optimised for the Wikimedia Foundation MediaWiki instances, such as Wikipedia.

__Author:__ Os Keyes<br/>
__License:__ [MIT](https://opensource.org/licenses/MIT)<br/>
__Status:__ Stable

![downloads](https://cranlogs.r-pkg.org/badges/grand-total/WikipediR)

Description
======
_WikipediR_ is a wrapper around the MediaWiki API, optimised for the Wikimedia Foundation's production sites, such as Wikipedia. It is written in and for R, a statistical environment and associated programming language in heavy use by HCI researchers that, somehow, escaped having an API wrapper until now.

Installation
======

For the most recent version, on CRAN:

    install.packages("WikipediR")
    
For the development version:

    library(devtools)
    devtools::install_github("ironholds/WikipediR")
    
Limitations
======
WikipediR currently lacks:
* Quite a few possible API calls;
* Direct authentication, through MediaWiki or OAuth.

These issues will be solved in time; if there is a particular feature you want, open an issue here on github: I can't prioritise if I don't know what people are trying to do :).

Dependencies
======
* R. Doy.
* [httr](https://cran.r-project.org/package=httr) and its dependencies.
