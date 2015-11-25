#'  jb_eq5dGetValues: Get summary stats from EQ-5D
#'  
#'  @author James Black
#'  @param barorder Numeric 1 to whatever. For which bar (or timepoint)  this set of EQ-5D measurements will be on the plot.
#'  @param eq_index The main EQ-5D index score. 
#'  @param domain_one Any of the five eq domains - order doesn't matter
#'  @param domain_two Any of the five eq domains - order doesn't matter
#'  @param domain_three Any of the five eq domains - order doesn't matter
#'  @param domain_four Any of the five eq domains - order doesn't matter
#'  @param domain_five Any of the five eq domains - order doesn't matter
#'  @examples
#'  "See epijim.uk"                                                                                                                                             

jb_eq5dGetValues <- function(barorder,
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