#' AWS coordinates 
#'
#' Dataset contains lat lon of all the aws stations. 
#'
#' @format A data frame with 46 rows and 9 variables:
#' \describe{
#'   \item{IT_DATETIME}{price, in US dollars}
#'   \item{DS_CODE}{weight of the diamond, in carats}
#'   \item{DS_NAME}{weight of the diamond, in carats}
#'   \item{DS_DESC}{weight of the diamond, in carats}
#'   \item{DS_LAT}{weight of the diamond, in carats}
#'   \item{DS_LON}{weight of the diamond, in carats}
#'   \item{DS_ALT}{weight of the diamond, in carats}
#'   \item{REH1.TG}{weight of the diamond, in carats}
#'   \item{REH1.Q_TG}{weight of the diamond, in carats}
#' }
#' @examples 
#' data(AWS.df)
#' str(AWS.df)
#' summary(AWS.df)
#' 
"AWS.df"