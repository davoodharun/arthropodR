aggregation1 <- function (dataFileName) {
  data <- read.csv(dataFileName, header = TRUE);
  data
}

aggregation1("dataSet.csv")


