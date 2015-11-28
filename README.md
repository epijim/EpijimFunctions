# EpijimFunctions
My R functions, based off advice from here http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/


## How to install the package

Load from github with `install_github('epijim/EpiJimFunctions')`.

## Updating (for me)

#### Get required packages

```r
install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
```
#### Update
At the top of the R script with the function, add in the variables for `roxygen2` so that it will 
auto-generate the documentation for the function (see head of existing functions).

```r
directory <- "~/funwithcode_local/code_snippets/EpijimFunctions"
setwd(directory)
```

Put function (r script) in `/R/` with name of file as name of function.

```r
library(roxygen2)

roxygen2::roxygenise()
# devtools::document() # alternative way to fire up roxy
```

Package is now updated! Push to github then install as per below.

```r
# Install locally (if starting inside package dir)
  setwd("..")
  install("EpijimFunctions")
# Install from github
  devtools::install_github("epijim/EpijimFunctions") # Install package
```
