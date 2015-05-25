#' Automatically install missing packages
#'
#' Packager loader that will automatically install missing packages
#' @param variable Vector of interest.
#' @param by The name of the package.
#' @keywords summary
#' @export
#' @examples
#' data <- iris
#' jb_summaryby(data$Sepal.Width,data$Species)

jb_summaryby <- function (variable,by) {
  means <- tapply(variable, by, mean, na.rm=TRUE)
  sds  <- tapply(variable,by, sd, na.rm = TRUE)
  valid <- function (variable) {return(sum(!is.na(variable)) )}
  ns <- tapply(variable,by, valid )
  se=sds/sqrt(ns)
  median <- tapply(variable, by, median, na.rm=TRUE)
  LQ <- tapply(variable, by, quantile, na.rm=TRUE, probs=0.25)
  UQ <- tapply(variable, by, quantile, na.rm=TRUE, probs=0.75)
  max <- tapply(variable,by, max, na.rm = TRUE)
  min <- tapply(variable,by, min, na.rm = TRUE)
  answer <-data.frame(n=ns,
                      mean=means,sd=sds,se=se,
                      median=median,LQ=LQ,UQ=UQ,
                      min=min,max=max)
  answer
}

#' Quickly histograph a bunch of variables
#'
#' Packager loader that will automatically install missing packages
#' @param data Dataframe, will automatically filter to numeric. But limited to max that fit on one page.
#' @keywords summary
#' @export
#' @examples
#' data <- iris
#' jb_multihists(data)
#' 

jb_multihists <- function(x) {
  # get number of variables
  classes <- as.character(lapply(x,class))
  nvar <- length(grep("\\<numeric\\>", classes))  #number of variables
  nsize=trunc(sqrt(nvar))+1   #size of graphic
  old.par <- par(no.readonly = TRUE) # all par settings which can be changed
  par(mfrow=c(nsize,nsize))       #set new graphic parameters
  for (i in 1:nvar) {
    # Only run if numeric
    if(class(x[,1])=="numeric"){
      name=names(x)[i]                #get the names for the variables
      hist(x[,i],main=name,xlab=name) }  #draw the histograms for each variable
  }
  on.exit(par(old.par))   #set the graphic parameters back to the original
}
