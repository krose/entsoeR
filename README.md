
<!-- README.md is generated from README.Rmd. Please edit that file -->
entsoeR
=======

USE THE [entsoeapi](https://github.com/krose/entsoeapi) PACKAGE INSTEAD OF THIS ONE. THIS PACKAGE WILL NOT GET UPDATED.

entsoeR wraps the [API](https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html) from Entso-e.

The API returns some very un-tidy xml and these are made as tidy as possible, but

Installation
------------

To get an API key, you must follow the instructions [here](https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html#_authentication_and_authorisation).

When you have got the API key, it's recommended to save it to your .Renviron file as shown below. The package will search for the key, so you don't have to add it to all your scripts.

    ENTSOE_PAT = "<ENSTO-E-API-KEY>"

You can install entsoeR from github with:

``` r
# install.packages("devtools")
devtools::install_github("krose/entsoeR")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```
