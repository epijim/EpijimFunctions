#' Automatically install missing packages
#'
#' Packager loader that will automatically install missing packages
#' @param name The name of the package.
#' @keywords packages
#' @export
#' @examples
#' jb_load(ggplot2)

jb_load <- function(package) { 
  package <- as.character(substitute(package)) 
  if(isTRUE(package %in% .packages(all.available=TRUE))) { 
    eval(parse(text=paste("require(", package, ")", sep="")))
    print(paste0(
      "Loaded ",
      package)
    )
  } else { 
    update.packages() 
    eval(parse(text=paste("install.packages('", package, "')", sep=""))) 
    print(paste0(
      "Uhoh, ",package, "was missing so it has been installed")
    )
  } 
} 