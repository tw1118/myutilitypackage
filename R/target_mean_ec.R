#' Target mean encoding
#'
#' this function takes a data table and a column and a target predictor to calculate target mean encoding.
#'
#' @param a data table, column string, target string
#' @return a data frame with resulted values
#' @export
target_mean_enc <- function(data, column, target) {
  if (class(data[[target]]) %in% c("numeric", "integer")) {
    data[[target]] <- as.numeric(data[[target]])
    print(unique(data[[target]]))
  }
  tmp <- sapply(unique(train[[column]]), function(x) {
    index <- data[[target]][data[[column]] == x]
    prop.target <- sum(index) / length(index)
  })
  tmp_df <- as.data.frame(tmp)
  tmp_df[[column]] <- rownames(tmp_df)

  names(tmp_df) <- c(paste0("target_mean", column), column)
  rownames(tmp_df) <- NULL
  return (tmp_df)
}
