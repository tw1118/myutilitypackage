#' Getting distance between two data points 
#'
#' This function calculates euclidean distance between two points
#' @param a point 
#' @param a different point 
#' @return distance value
#' @export
getDistance <- function(p1, p2) {
  return(sqrt(sum((p1 - p2)^2)))
}
