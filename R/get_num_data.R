#' Getting numeric data
#'
#' this function takes a datatable and filters only numeric data
#' @param a data table
#' @return sliced data table with only numerical features
#' @export
get_num_data <- function(DT) {
  if (!is.data.table(DT)) {
    DT <- as.data.table(DT)
  }
  bool <- sapply(DT[sample(1:nrow(DT), 100)], class) %in% c("integer", "numeric")
  DT <- DT[, bool, with = FALSE]
  return(DT)
}
