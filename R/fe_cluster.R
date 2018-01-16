#' Feature Engineering Clustering
#'
#' This function takes in a data table and a desired number of features in a cluster. This function will filter only
#' numerical features first and cluster them with randomly picked numerical features. the final data table will have added
#' clustered id's and distance to centroids added
#' @param a data table
#' @return a data table with added features
#' @export
fe_cluster <- function(DT, predInClust = 5) {
  # This function takes in a DT and filters only numeric variable.
  # performs k means with prdInClust in one data table
  # returns data table with added features. cluster, and distance from each cluster.

  # Declare variables
  colNameList <- list()
  # Check whether DT is a data.table
  if (!is.data.table(DT)) {
    print("The input is not a data.table")
    stop()
  }

  # import libraries
  pkgs <- c("dplyr", "data.table", "fpc")
  ipak(pkgs)

  # Define distance function for getting distance between points
  getDistance <- function(p1,p2) {
    return(sqrt(sum((p1 - p2)^2)))
  }

  # Getting only numeric values
  numCol <- sapply(DT[sample(1:nrow(DT), 100)], is.numeric)

  #subset(DT, ,numCol)
  DT_cat <- data.table::copy(DT[, !numCol, with = FALSE])
  DT <- data.table::copy(DT[, numCol, with = FALSE])

  # preprocess on the numeric values
  zeroCol <- zeroVar(DT) # zero variance removal
  DT <- DT[, names(zeroCol[!zeroCol]), with = FALSE]
  #DT <- DT[, !zeroCol, with = FALSE]
  uniqCol <- findUniqueKey(DT) # Unique Key removal
  DT <- DT[, names(uniqCol[!uniqCol]), with = FALSE]
  #DT <- DT[, !uniqCol, with = FALSE]
  # Unique Indexing
  # DT[, "index" := 1:nrow(DT)] # Creating index for future indexing

  # Iteratively go through sampled column
  colName <- colnames(DT)

  for (i in 1:ceiling(length(colName) / predInClust)) {
    # sampling randomly to pick variables defined by parameter
    if (length(colName) < predInClust) {
      predInClust <- length(colName)
    }
    tmpCol <- sample(x = colName, size = predInClust, replace = FALSE)
    colName <- colName[!colName %in% tmpCol]

    # filtering only the few columns
    dt <- data.table::copy(DT[, tmpCol, with = FALSE])
    sc.dt <- data.table::copy(scale(dt))

    #colNameList[[i]] <- tmpCol
    #clustMeanSDList <- list()
    #clustMeanList[[i]] <- apply(sc.dt, 2, mean)
    #class(clustMeanSDList[1])
    #colNameList

    # testing how many cluster is needed for the dataset
    pamk.best <- pamk(sc.dt, krange = 2:10, usepam = TRUE) # Getting number of k based on Shillouette Width
    numclust <- pamk.best$nc

    # K-means clustering algorithm
    md.kmeans <- kmeans(sc.dt, center = numclust)

    # Declaring clustering info
    numClust <- pamk.best$nc
    center <- md.kmeans$centers
    cluster <- md.kmeans$cluster

    # Adding column name to the original data table
    clustColName <- paste0(paste(tmpCol, collapse = "_"), "_cluster", collapse = "")
    clustDistColName <- paste(paste(tmpCol, collapse = "_"), "cluster_Distance", collapse = "_")
    DT[[clustColName]] <- cluster
    DT[[clustDistColName]] <- -9999

    # center <- pamk.best$pamobject$medoids
    # cluster <- pamk.best$pamobject$clustering

    # Iterating through number of clusters to get distance from the centroid
    for (i in 1:numClust) {
      # filtering only cluster i data
      tmp <- data.table::copy(sc.dt[cluster == i,])

      d <- sapply(1:nrow(tmp), function(x) {
        getDistance(tmp[x,], center[i,])
      })
      #cluster
      DT[[clustDistColName]][cluster == i] <- d
    }
    DT[[clustColName]] <- as.factor(DT[[clustColName]])

  }

  # Binding catagorical data table with transformed data table
  DT <- cbind(DT_cat, DT)
  return(DT)
  # things to do
  # 1 get numeric data
  # 2 sample with replacement
  # for each cluster data table
  # perform k means, mclust, hclust
  # add cluster and distance to center
  # combine all data frames
  # result
}
