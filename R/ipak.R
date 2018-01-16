#' loading necessary libraries
#'
#' This function takes in a vector of libraries and load them to the environment
#'
#' @param a character vector of library names
#' @return boolean values of whether libraries loaded successfully
#' @export

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    suppressMessages(install.packages(new.pkg, dependencies = TRUE))
  suppressMessages(sapply(pkg, require, character.only = TRUE))
}
