#----------------------------------------- Functions ---------------------------

NA.analysis <- function(df)
{
  # prints the number of NA's and the % of overall values for all df columns containing at least one
  NA.count <- sapply(df, function(x) sum(is.na(x)))
  NA.percentage <- sapply(df, function(x) 100*sum(is.na(x))/length(x))
  
  #Create result matrix
  NA.summary = cbind(NA.count[NA.count>0], NA.percentage[NA.count>0])
  colnames(NA.summary) <- c("Count", "Percent")
  
  return(NA.summary[order(desc(NA.summary[,"Percent"])),])
}
