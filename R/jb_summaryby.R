#' Automatically install missing packages
#'
#' Packager loader that will automatically install missing packages
#' @param name The name of the package.
#' @keywords packages
#' @export
#' @examples
#' data <- iris
#' jb_summaryby(Sepal.Width,Species)

jb_summaryby <- function (variable,by) {
  means <- tapply(variable, by, mean, na.rm=TRUE)
  sds  <- tapply(variable,by, sd, na.rm = TRUE)
  valid <- function (variable) {return(sum(!is.na(variable)) )}
  ns <- tapply(variable,by, valid )
  se=sds/sqrt(ns)
  median <- tapply(variable, by, median, na.rm=TRUE)
  LQ <- tapply(variable, by, quantile, na.rm=TRUE, probs=0.25)
  UQ <- tapply(variable, by, quantile, na.rm=TRUE, probs=0.75)
  answer <-data.frame(n=ns,
                      mean=means,sd=sds,se=se,
                      median=median,LQ=LQ,UQ=UQ)
  answer
}