#' Preprocessing data
#'
#' This function will eliminate zero variance column and unique identifier column
#' This is a preliminary step for fe_cluster
#' @param a data table
#' @return a data table
#' @export
preprocess_dt <- function(df) {
  # Removing zero variance column

  zeroCol <- zeroVar(df)
  df <- df[, c(!zeroCol), with = F]

  # Removing unique identifier
  uniqKey <- findUniqueKey(df)
  df <- df[, c(!uniqKey), with = FALSE]
  return(df)
}

