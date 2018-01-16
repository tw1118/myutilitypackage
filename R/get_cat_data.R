#' Getting categorical data
#'
#' This function takes a datatable and slices only categorical data
#'
#' @param  a data table
#' @return a data table with categorical features
#' @export
get_cat_data <- function(DT) {
  if (!is.data.table(DT)) {
    DT <- as.data.table(DT)
  }
  bool <- sapply(DT[sample(1:nrow(DT), 100)], class) %in% c("integer", "numeric")
  DT <- DT[, !bool, with = FALSE]
  return(DT)
}
