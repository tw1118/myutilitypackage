#' PCA variance explanined plot
#'
#' This function takes a model pca object and plots a pca variance explained plot
#'
#' @param a prcomp object
#' @return a plot of variance explained
#' @export
pca_varExp_plot <- function (md.pca){
  pc_var <- (md.pca$sdev^2)/sum(md.pca$sdev^2)
  plot(pc_var, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b")

}
