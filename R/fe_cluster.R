
# Declaring Helper function
preprocess_dt <- function(df) {
  # Removing zero variance column
  zeroCol <- zeroVar(df)
  df <- df[, !zeroCol, with = FALSE]

  # Removing unique identifier
  uniqKey <- findUniqueKey(df)
  df <- df[, !uniqKey, with = FALSE]
  return(df)
}

slice_column <- function(DT, columnNames) {
  columnNames <- as.vector(columnNames)
  #columnNames <- as.vector(names(DT))

  DT <- DT[, columnNames, with = FALSE]
  return(DT)
}

getDistance <- function(p1, p2) {
  return(sqrt(sum((p1 - p2)^2)))
}

#' Feature Engineering Clustering
#'
#' This function takes in a data table and a desired number of features in a cluster. This function will filter only
#' numerical features first and cluster them with randomly picked numerical features. the final data table will have added
#' clustered id's and distance to centroids added
#' @import dplyr
#' @import data.table
#' @import fpc
#' @importFrom FNN get.knnx unbox
#' @param a train data table
#' @param a test data table
#' @param number of k in kmeans clustering
#' @return a data table with added features
#' @export
fe_cluster <- function(DT, test = "None", predInClust = 5) {
  # This function takes in a DT and filters only numeric variable.
  # performs k means with prdInClust in one data table
  # returns data table with added features. cluster, and distance from each cluster.


  # Checking if DT is a data table
  if (!is.data.table(DT) | test == "None") {
    print("The input is not a data.table or test dataset is not given")
    stop()
  }

  # preprocess data (eraze zero varaiance predictor, and unique identifier)
  DT <- preprocess_dt(DT)

  # splitting numeric and categorical data table
  DT_cat <- get_cat_data(DT)
  DT <- get_num_data(DT)

  # apply same pipeline to test data table
  class(test)
  if (is.data.table(test)) {
    test_cat <- slice_column(test, names(DT_cat))
    test <- slice_column(test, names(DT))
  }

  # declaring column names to the original table
  colName <- colnames(DT)

  print("Initiating clustering algorithm")

  sapply(1:ceiling(length(colName)/predInClust), function(x) {

    # if column length does not match predInClust parameter
    if (length(colName) < predInClust) {
      predInClust <- length(colName)
    }

    # select columns to cluster
    tmpCol <- sample(x = colName, size = predInClust, replace = FALSE)
    # remove columns from colName
    colName <<- colName[!colName %in% tmpCol]
    # slicing data table with only selected column
    dt <- data.table::copy(DT[, tmpCol, with = FALSE])

    #scaling numerical variables (for distance dissimilarity matrix)
    sc.dt <- scale(dt)
    cent <- attr(sc.dt, "scaled:center")
    std <- attr(sc.dt,"scaled:scale")
    sc.dt <- data.table::copy(as.data.table(sc.dt))

    # getting optimal number of clusters
    pamk.best <- pamk(sc.dt, krange = 2:10, usepam = TRUE)
    # store k number
    numclust <- pamk.best$nc
    # getting info of kmeans
    numClust <- pamk.best$nc

    # apply k means
    md.kmeans <- kmeans(sc.dt, center = numclust)
    #store kmeans values
    center <- md.kmeans$centers
    cluster <- md.kmeans$cluster

    # making column names for selected columns
    clustColName <- paste0(paste(tmpCol, collapse = "_"),
                           "_cluster", collapse = "")
    clustDistColName <- paste(paste(tmpCol, collapse = "_"),
                              "cluster_Distance", collapse = "_")

    DT[[clustColName]] <<- cluster
    DT[[clustDistColName]] <<- -9999

    # apply to test data table (Same pipeline as above)
    if (is.data.table(test)) {
      # scaling data
      test_dt <- data.table::copy(test[, tmpCol, with = FALSE])
      sc.dt.test <- data.table::copy(as.data.table(scale(test_dt, cent, std)))

      #apply predefined kmeans in train
      test_clust_id <- FNN::get.knnx(md.kmeans$centers, sc.dt.test, 1)$nn.index[,1]
      test[[clustColName]] <<- test_clust_id
      test[[clustDistColName]] <<- -9999
    }

    # removing unnecessary variables
    rm(cent, std)

    # declaring a varaible for slicing
    i <- 0
    # Calculating distance from a datapoint to centroids in kmeans clustering
    sapply(1:numClust-1, function(x) {
      i <<- i + 1

      train_tmp <- data.table::copy(sc.dt[cluster == i])

      train_distance <- sapply(1:nrow(train_tmp), function (x) {
        getDistance(train_tmp[x, ], center[i, ])
      })

      if (is.data.table(test)) {
        test_tmp <- data.table::copy(sc.dt.test[test_clust_id == i])

        test_distance <- sapply(1:nrow(test_tmp), function (x) {
          getDistance(test_tmp[x, ], center[i, ])
        })
      }

      DT[[clustDistColName]][cluster == i] <<- train_distance
      test[[clustDistColName]][test_clust_id == i] <<- test_distance
    })

    # Adding distance to original data table
    DT[[clustColName]] <- as.factor(DT[[clustColName]])
    test[[clustColName]] <- as.factor(test[[clustColName]])
  })

  print("Finished Algorithm, preparing to return")
  # Concatnating categorical data table with numerical (finished) data table
  DT <- cbind(DT_cat, DT)
  test <- cbind(test, test_cat)
  return(list(train = DT, test = test))
}
