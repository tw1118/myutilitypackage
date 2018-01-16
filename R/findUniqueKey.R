
#' Finding unique key
#'
#' This function takes a data table and returns a logical vectors telling which column has a unique identifier
#' @param a data table
#' @return a logical vector
#' @export
findUniqueKey <- function(DT) {
  # this function takes in data table and return boolean values of
  # predictor(s) that contains unique key.
  # ex) customer id in customer data table.
  require(dplyr);require(data.table)
  res <- sapply(DT, function(x) {
    length(unique(x)) == nrow(DT)
  }
  )
  return(res)
}
