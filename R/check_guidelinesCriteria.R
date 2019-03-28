#'Check and select appropriate WMO SC guidelines criteria
#'
#'@title Check and select WMO SC Guidelines
#'@description Check and select WMO SC Guidelines
#'@param df dataframe to select the criteria
#'@param criteria_columnName String. Name of column name from df
#'@author Jelle Stuurman
#'create_spatialPoint(X = , Y = , LAT LON = TRUE)
#'@return data frame with values for each Class for the shading, heat sources and vegetation height criteria

check_criteria <- function(df, criteria_columnName){
  if(missing(criteria_columnName)){
    criteria_columnName <- "Criteria_Value"
  }
  for(a in 1:nrow(df)){
    str <- df[a,criteria_columnName]
    tryCatch({  
      if(grepl(",", str) == TRUE){
        str_adj <- gsub(",", ".", str, fixed = TRUE)
        nr <- as.numeric(str_adj)
        df[a,criteria_columnName] <- nr
      }
    }, error=function(e){
      message(e)
      stop("No correct Criteria value column name is selected. Change it or leave it out for default WMO Crteria gudeline values.")
    })
  }
  return (df)
}