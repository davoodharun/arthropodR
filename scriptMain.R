ag1 <- function(fileName){
  # read csv file
  data <- read.csv(fileName, header = TRUE);
  
  # get all POST samples
  Post_data <- data[grepl('POST', data$Sample),]
  
  H2O <- Post_data[grepl('H2O', Post_data$Sample),]
  MET52 <- Post_data[grepl('Met52', Post_data$Sample),]
  
  H2O_ag <- aggregate(H2O$number, by=list(Sample=H2O$Sample), FUN=sum)
  MET52_ag <- aggregate(MET52$number, by=list(Sample=MET52$Sample), FUN=sum)
  

  # print all POST samples
  graph2 <- matrix(c(sum(MET52_ag$x), sum(H2O_ag$x)), ncol=2,byrow=TRUE)
  colnames(graph2) <- c("MET52","H2O")
  rownames(graph2) <- c("Number")
  graph2 <- as.table(graph2)
  graph2
  
  # print graph
  barplot(graph2)
}

ag1("dataSet.csv")