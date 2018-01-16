#' Finding zero variance
#'
#' This function takes a data table and finds both numeric and character predictors that have only one value (zero variance predictors)
#'
#' @param  a data table
#' @return a logical vector indicating which columns have one value for all
#' @export
zeroVar <- function(DT) {
  # This function takes a data table and finds both numeric and character predictors
  # that have only one value (zero variance predictors)
  # Input : Data table
  # Output : logical vector indicating which columnn(s) has one value for all
  require(dplyr);require(data.table)
  res <- sapply(DT, function(x) {
    length(unique(x)) == 1
  }
  )
  return(res)
}
