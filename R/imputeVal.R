#' Impute Values
#'
#' Objective is to find NA values and imput using median. for categorical values, "None" factor will be added in place for 
#' missing values. 
#' @import dplyr
#' @import data.table
#' @param a Data Table with missing values
#' @return A finished data table with filled missing data table
#' @export
imputeVal <- function(dt, method = "median") {
  # get numeric
  # apply each column with median
  #train <-as.data.table(fread("../Downloads/train.csv", stringsAsFactors = F))
  num_dt <- get_num_data(dt)
  cat_dt <- get_cat_data(dt)

  
  
  
  #numeric Values
  med_val <- apply(num_dt, 2, median, na.rm = TRUE)
  
  if (any(is.na(med_val))) {
    warning(
      paste(
        "These variables are never filled:",
        paste(names(med_val)[is.na(med_val)], collapse = ", ")),
      immediate. = TRUE)
    med_val[is.na(med_val)] <- 0
    
  }
  med_val_df <- as.data.frame(med_val, row.names = names(med_val))
  
  sapply(names(num_dt), function(x) {
    #invisible(
    set(num_dt, i = which(is.na(num_dt[[x]])), j = x, value = med_val_df[x,])
    #)
  })
  
  # Categorical values
  sapply(names(cat_dt), function(x) {
    #invisible(
    set(cat_dt, i = which(is.na(cat_dt[[x]])), j = x, value = "None")
    #)
  })
  
  dt <- cbind(num_dt, cat_dt)
  
  return(dt) 
}

#train <-as.data.table(fread("../Downloads/train.csv", stringsAsFactors = F))
#a <- imputeVal(train)
