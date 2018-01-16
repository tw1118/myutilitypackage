
#' frequency encoding
#'
#' This function takes a data table and target column and gives frequency encoding for the target column
#'
#' @param a data table and a string of a column
#' @return a data frame with frequency encoded values
#' @export
freq_encoding <- function(data, column) {
  data[[column]]
  tmp <- sapply(unique(data[[column]]), function(x) {sum(data[[column]] == x)}) / nrow(data)
  tmp_df <- as.data.frame(tmp)
  tmp_df[[column]] <- rownames(tmp_df)

  names(tmp_df) <- c(paste0("freq_fe_", column), column)
  rownames(tmp_df) <- NULL

  return(tmp_df)
}
