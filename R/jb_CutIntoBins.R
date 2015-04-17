#' Make sparklines from a variable
#'
#' Sparkline histograms form an R variable
#' @param bins Number of bins or bars in the histogram. Defaults to 10.
#' @param width The width of the histogram. Defaults to 6.
#' @param time This is the x axis. If only one variable is given, this isn't needed.
#' @param name The name to give this sparkline. I define as a command to keep the main text where you insert it tidy.
#' @keywords latex
#' @export
#' @examples
#' jb_CutIntoBins()

jb_CutIntoBins <- function(variable,time="potato",name="line",bins=10,width=6){
  # Make histograph ###################### if clause ###########################
  if(length(time)==1){
    temp_ranges <- range(variable, na.rm=T) # Get range
    temp_breaks <- seq(temp_ranges[1],temp_ranges[2], # From first to last
                       length=(bins+1)) # Number of bins
    temp_cut <- as.data.frame(cut(variable,
                                  breaks=temp_breaks,labels=F))
    # Set up dataset
    temp_data <- data.frame(bin=1:bins,
                            value=c(NA)
    )
    temp_cut <- as.data.frame(table(temp_cut),stringsAsFactors=F) #
    temp_cut$temp_cut <- as.numeric(temp_cut$temp_cut)
    # Fill dataset
    for(i in 1:bins){
      tryCatch(
        if(i==temp_data[i,1]){
          temp_data[i,2] <- temp_cut[temp_cut$temp_cut==i,2]
        },error=function(e){})
    }
    # Make sparkline
    binlocations <- seq(0,1,length=bins)
    middle<-NA
    for(i in 1:nrow(temp_data)){
      ifelse(is.na(temp_data[i,2]),
             middle[i] <- paste0(" \\sparkspike ",binlocations[i]," ",0),
             middle[i] <- paste0(" \\sparkspike ",binlocations[i]," ",
                                 temp_data[i,2]/max(temp_data$value,na.rm=T))
      )
    }
  }
  # Make line ###################### if clause ###########################
  if(length(time)>1){
    temp_ranges <- range(time, na.rm=T) # Get range
    temp_breaks <- seq(temp_ranges[1],temp_ranges[2], # From first to last
                       length=(bins+1)) # Number of bins
    temp_cut <- as.data.frame(cut(time,
                                  breaks=temp_breaks,labels=F))
    temp_data <- data.frame(temp_cut,time=time,value=variable)
    names(temp_data)[1] <- "group" # messy as temp_cut is a df
    temp_data <- aggregate(temp_data$value,by=list(temp_data$group),FUN=median,na.rm=T)
    names(temp_data)[1] <- "group"
    temp_data$x <- temp_data$x/max(temp_data$x)
    temp_output <- data.frame(group=1:bins,value=c(0))
    for(i in 1:bins){
      tryCatch(
        if(i==temp_data[temp_data$group==i,1]){
          temp_output[i,2] <- temp_data[temp_data$group==i,2]
        },error=function(e){})
    }
    temp_output$datalocations <- seq(0,1,length=bins)
    middle <- c(" \\spark",
                paste0("   ",temp_output$datalocations,"   ",temp_output$value)
    )
  }
################### OUTPUT
  opening <- paste0("\\newcommand{\\spark",
                    name,"}{\\begin{sparkline}{",
                    width,"}")
  close <- "/ \\end{sparkline}}"
  output <- c(opening,middle,close)
  cat(output,sep="\n")
}