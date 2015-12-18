#'  Vertical Bullet Graph
#'
#'  Plot the EQ5D.
#'  @param data:  data.frame that is produced by rbinding runs of jb_geteq5dvalues
#'                     bar  : number of bar values are from
#'                     eq_index     : eq5d index score
#'                     impaired_domains     : number of domains listed as impaired
#'  @param bullet TRUE for bullet style plot with mean and median inspired by Muller's work, or FALSE for violin plots.
#' @examples
#' jb_eq5dPlot()

jb_eq5dPlot <- function(data,bullet=F){
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
