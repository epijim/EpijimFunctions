#'  jb_geteq5dvalues: Get summary stats from EQ-5D
#'  
#'  @author James Black - a ggplot take on the bullet plots by Simon Müller
#'  
#'  @param barorder Numeric 1 to whatever. For which bar (or timepoint)  this set of EQ-5D measurements will be on the plot.
#'                     
#'  @param eq_index The main EQ-5D index score. 
#'      
#'  @param domain_one Any of the five eq domains - order doesn't matter
#'  
#'  @param domain_two Any of the five eq domains - order doesn't matter
#' 
#'  @param domain_three Any of the five eq domains - order doesn't matter
#'   
#'  @param domain_four Any of the five eq domains - order doesn't matter
#' 
#'  @param domain_five Any of the five eq domains - order doesn't matter
#'    
#'  jb_geteq5dvalues()                                                                                                                                              

jb_geteq5dvalues <- function(barorder,
                             eq_index,
                             domain_one,
                             domain_two,
                             domain_three,
                             domain_four,
                             domain_five){
  # check range of index score
  if(2<max(eq_index, na.rm=T)) {
    warning("Score has been transformed from 0-100 to 0-1 scale")
    eq_index <- eq_index/100
  }
  if(0>min(eq_index, na.rm=T)) {
    below_zero <- NA
    below_zero <- ifelse(eq_index<0,below_zero <- 1,0)
    below_zero <- sum(below_zero, na.rm=T)
    warning(paste0(below_zero,
                   " people scored below zero, which is not well represented in this plot")
    )
  }
  # Create variable counting impairment
  impaired_domains <- 0
  impaired_domains <- ifelse(domain_one > 1, impaired_domains+1, impaired_domains)
  impaired_domains <- ifelse(domain_two > 1, impaired_domains+1, impaired_domains)
  impaired_domains <- ifelse(domain_three > 1, impaired_domains+1, impaired_domains)
  impaired_domains <- ifelse(domain_four > 1, impaired_domains+1, impaired_domains)
  impaired_domains <- ifelse(domain_five > 1, impaired_domains+1, impaired_domains)
  impaired_domains <- ifelse(is.na(eq_index),NA,impaired_domains)
  data <- data.frame(bar=barorder,eq_index,impaired_domains)
  return(data)
}

#'  bulleteq5d: Vertical Bullet Graph
#'  
#'  @author James Black - a ggplot take on the bullet plots by Simon Müller
#'  
#'  @param data:  data.frame with
#'                     measure  : label of what's being measured
#'                     unit     : label of units of the measure
#'                     high     : the high value for the measure
#'                     second_cut     : the mean value for the measure
#'                     first_cut  : the first_cut value for the measure
#'                     target   : the target value for the measure
#'                     value    : the actual value of the measure
#'                     
#'  @param bullet TRUE for bullet style plot with mean and median inspired by Muller's work, or FALSE for violin plots.
#'      
#' jb_ploteq5d()

jb_ploteq5d <- function(data,bullet=F){
  data <- data
  # Plot it!
  require(ggplot2)
  require(plyr)
  require(Hmisc)
  # colour blind safe pallete
  cbPalette <- c("#D55E00","#CC79A7","#E69F00","#F0E442","#56B4E9","#009E73")
  # reorder impaired domains
  data$order <- factor(data$impaired)
  data$order <- with(data, factor(order, levels = rev(levels(order))))
  # give pretty names
  levels(data$order) <- c("5 domains","4 domains","3 domains",
                          "2 domains","1 domain","No domains")
  # timepoints
  data$timepoints <- factor(data$bar)
  timepoint_label <- NULL
  for(i in unique(data$bar)){
    timepoint_label <- c(timepoint_label,paste0("Timepoint ",i))
  }
  levels(data$timepoints) <- timepoint_label
  
  data_means <- ddply(data, "timepoints", summarise, 
                      mean = mean(eq_index),
                      median = median(eq_index))
  
  ifelse(isTRUE(bullet),
         plot <-   ggplot(data) + 
           geom_bar(data=data,
                    aes(x=timepoints,
                        fill=factor(order)),
                    position="fill")+
           geom_bar(data=data_means,
                    aes(x=timepoints,
                        y=mean),
                    width=.1,
                    stat="identity")+
           geom_point(data=data_means,
                      aes(x=timepoints,
                          y=median),
                      shape="-",
                      size=30,
                      color="red")+
           scale_fill_manual(values=cbPalette,
                             name="Proportion with an \nimpairment in each \nEQ-5D domain")+
           scale_y_continuous(name="Euroqol index score / Proportion")+
           scale_x_discrete(name="")+
           theme_bw()
         ,
         
         plot <- ggplot(data) + 
           geom_bar(aes(x=timepoints,
                        fill=factor(order)),
                    position="fill")+
           geom_violin(
             aes(timepoints,eq_index),
             trim=T)+
           geom_boxplot(
             aes(timepoints,eq_index),
             colour="darkred",
             width=0.1,
             outlier.shape = NA)+
           #stat_summary(
           #  aes(x=timepoints,y=eq_index,
           #      fill=factor(order)),
           #  fun.data="data_summary")+
           scale_fill_manual(values=cbPalette,
                             name="Proportion with an \nimpairment in each \nEQ-5D domain")+
           scale_y_continuous(name="Euroqol index score / Proportion",
                              breaks=seq(-0.25, 1, 0.25))+
           scale_x_discrete(name="")+
           theme_bw()
  )
  print(plot)}