# neuroc.sysreq
[![Travis build status](https://travis-ci.org/adigherman/neuroc.sysreq.svg?branch=master)](https://travis-ci.org/adigherman/neuroc.sysreq)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/adigherman/neuroc.sysreq?branch=master&svg=true)](https://ci.appveyor.com/project/adigherman/neuroc.sysreq)
[![Coverage Status](https://coveralls.io/repos/github/adigherman/neuroc.sysreq/badge.svg)](https://coveralls.io/github/adigherman/neuroc.sysreq)

The purpose of the `neuroc.sysreq` package is to provide a list of formatted system requirements for a package using a recursive dependency tree.

## Installation

You can install neuroc.sysreqs from github with:

``` r
# install.packages("devtools")
devtools::install_github("adigherman/neuroc.sysreqs")
```

## Usage

``` r
neuroc.sysreq::get_all_sysreqs('neuroconductor/fslr')
```


