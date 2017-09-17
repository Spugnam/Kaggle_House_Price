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


generate.HouseAgeNewFeature <- function(house.train)
{
  # Transforms the YearBuilt feature into HouseAge: years since the house was built
  # Define new feature
  HouseAgeNewFeature.df <- data.frame(house.train$YearBuilt)
  colnames(HouseAgeNewFeature.df) <- c("HouseAgeNewFeature")
  # Insert after YearBuilt
  spot <- which(names(house.train)=="YearBuilt")
  house.train <- cbind(house.train[,1:spot],HouseAgeNewFeature.df,house.train[,(spot+1):ncol(house.train)])
  
  # Get current year
  current.year <- as.numeric(format(Sys.Date(),'%Y'))
  
  # Assign value
  house.train$HouseAgeNewFeature <- current.year - house.train$YearBuilt

  return(house.train)
}

generate.RemodNewFeature <- function(house.train)
{
  # Define new remodelling feature
  Remod.new.feature <- data.frame(house.train$YearRemodAdd)
  colnames(Remod.new.feature) <- c("RemodNewFeature")
  Remod.new.feature$RemodNewFeature <- 'No remodelling'
  # Insert after YearRemodAdd
  spot <- which(names(house.train)=="YearRemodAdd")
  house.train <- cbind(house.train[,1:spot],Remod.new.feature,house.train[,(spot+1):ncol(house.train)])
  
  # Get current year
  current.year <- as.numeric(format(Sys.Date(),'%Y'))
  
  # Create Levels/ make categorical
  house.train$RemodNewFeature[0<(current.year - house.train$YearRemodAdd) & (current.year - house.train$YearRemodAdd)<=1] <- 'Last year'
  house.train$RemodNewFeature[1<(current.year - house.train$YearRemodAdd) & (current.year - house.train$YearRemodAdd)<=2] <- 'Last 2 years'
  house.train$RemodNewFeature[2<(current.year - house.train$YearRemodAdd) & (current.year - house.train$YearRemodAdd)<=5] <- 'Last 5 years'
  house.train$RemodNewFeature[5<(current.year - house.train$YearRemodAdd) & (current.year - house.train$YearRemodAdd)<=10] <- 'Last 10 years'
  house.train$RemodNewFeature[10<(current.year - house.train$YearRemodAdd) & (current.year - house.train$YearRemodAdd)<=20] <- 'Last 20 years'
  house.train$RemodNewFeature[20<(current.year - house.train$YearRemodAdd) & (current.year - house.train$YearRemodAdd)<=30] <- 'Last 30 years'
  house.train$RemodNewFeature[30<(current.year - house.train$YearRemodAdd) & (current.year - house.train$YearRemodAdd)<=50] <- 'Last 50 years'
  house.train$RemodNewFeature <- as.factor(house.train$RemodNewFeature)
  
  return(house.train)
}

generate.NewAreaFeature <- function(df, factor1, numeric1, NewFeature)
{
  # Combines quality and square footage into a new factor. SF is made a factor by cutting it in 50
  # Define new feature
  NewFeature.df <- data.frame(factor1)
  colnames(NewFeature.df) <- NewFeature
  
  # Insert after the numeric column
  spot <- which(names(df)==numeric1)
  df <- cbind(df[,1:spot],NewFeature.df,df[,(spot+1):ncol(df)])

  # Assign value
  df[[NewFeature]] <- with(df, interaction(df[[factor1]], cut(df[[numeric1]], 7, labels=c(1:7))))
  
  # Remove original columns
  drops <- c(factor1, numeric1)
  return(df[,!(names(df)) %in% drops])
}

main.value.print <- function(f) {
  # Takes a factor column as input
  # Indicates which value represents highest % of occurences
  t <-table(as.factor(f), exclude=NULL)
  cat("Most represented value: ", names(which.max(t)), "\nPercentage: ", max(prop.table(t)))
}

replace.NA.most.frequent.value <- function(df, column) {
  # Finds the most frequent value and replaces all NA's with it
  t <-table(as.factor(df[,column]), exclude=NULL)
  df[[column]][is.na(df[[column]])] <- names(which.max(t))
  return(df[[column]])
}

replace.NA.value <- function(df, column, value) {
  levels(df[[column]]) <- c(levels(df[[column]]), value)
  df[[column]][is.na(df[[column]])] <- value
  return(df[[column]])
}




