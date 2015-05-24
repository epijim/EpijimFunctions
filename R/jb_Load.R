#' Automatically install missing packages
#'
#' Automatically install missing packages
#' @param name The name of the package.
#' @keywords packages
#' @export
#' @examples
#' jb_load()

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