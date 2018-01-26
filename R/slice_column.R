#' Slicing a data table with column names
#'
#' This function does column slicing from a data table
#' @param a data table
#' @param a string or a string vector with column name(s)
#' @return a data table
#' @export
slice_column <- function(DT, columnNames) {
  columnNames <- as.vector(columnNames)
  #columnNames <- as.vector(names(DT))
  
  DT <- DT[, columnNames, with = FALSE]
  return(DT)
}
