#' Feature Engineering PCA vector addition
#'
#' This function will take in data and add pca feature engineering (PC components) to the original table. For categorical variables,
#' MCA will be applied for feature reduction.
#' @import dplyr
#' @import data.table
#' @importFrom stats prcomp
#' @param a train data table
#' @param a test data table
#' @param ratio of variance explained (pca)
#' @param a number of components to be used (pca)
#' @param ratio of variace explained (mca)
#' @param a number of components to be used (mca)
#' @return a list with finished two data tables
#' @export
fe_pca <- function(train, test, pca_var, pca_comp, mca_var, mca_comp) {
  # eraze zero variance and unique id
  train <- as.data.table(fread("C://Users/taeha/Downloads/train.csv", stringsAsFactors = F))
  test <- as.data.table(fread("C://Users/taeha/Downloads/test.csv", stringsAsFactors = F))
  train_y <- train$SalePrice; train$SalePrice <- NULL

  train <- preprocess_dt(train)
  test <- slice_column(test, names(train))

  # divide into categorical and numerical varaible
  train_num <- get_num_data(train)
  train_cat <- get_cat_data(train)
  test_num <- get_num_data(test)
  test_cat <- get_cat_data(test)

  # do pca
  md.pca <- prcomp(train_num, center = T, scale. = T)

  # select variance
  # predict to test
  # add to varaible

  # same for Mca
}
