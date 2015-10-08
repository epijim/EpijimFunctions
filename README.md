# EpijimFunctions
My R functions, based off advice from here http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/


## How to install the package

Load from github with `install_github('epijim/EpiJimFunctions')`.

## Updating (for me)

#### Packages
```
install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)
```
#### Update
At the top of the R script with the function, add in the variables for `roxygen2` so that it will 
auto-generate the documentation for the function (see head of existing functions).

```
directory <- "~/funwithcode_local/code_snippets/EpijimFunctions"
setwd(directory)
```

Put function (r script) in `/R/` with name of file as name of function.

```
document() # Make documentation
setwd("..") # Go back one
install("EpijimFunctions") # Install package
```
