ag1 <- function(fileName){
  # read csv file
  data <- read.csv(fileName, header = TRUE);
  
  # get all POST samples
  Post_data <- data[grepl('POST', data$Sample),]
  
  # separate POST samples in H2O and MET52
  H2O <- Post_data[grepl('H2O', Post_data$Sample),]
  MET52 <- Post_data[grepl('Met52', Post_data$Sample),]
  
  # sum up number of invertebrates per trap
  H2O_ag <- aggregate(H2O$number, by=list(Sample=H2O$Sample), FUN=sum)
  MET52_ag <- aggregate(MET52$number, by=list(Sample=MET52$Sample), FUN=sum)
  
  H2O_ag_Lila <- H2O_ag[grepl('LiLa', H2O_ag$Sample),]
  H2O_total_Lila <- sum(H2O_ag_Lila$x)
  
  H2O_ag_AL <- H2O_ag[grepl('AL', H2O_ag$Sample),]
  H2O_total_AL <- sum(H2O_ag_AL$x)
  
  H2O_ag_BF <- H2O_ag[grepl('BF', H2O_ag$Sample),]
  H2O_total_BF <- sum(H2O_ag_BF$x)
  
  H2O_ag_FS <- H2O_ag[grepl('FS', H2O_ag$Sample),]
  H2O_total_FS <- sum(H2O_ag_FS$x)
  
  H2O_ag_GT <- H2O_ag[grepl('GT', H2O_ag$Sample),]
  H2O_total_GT <- sum(H2O_ag_GT$x)
  
  H2O_ag_HGA <- H2O_ag[grepl('HGA', H2O_ag$Sample),]
  H2O_total_HGA <- sum(H2O_ag_HGA$x)
  
  H2O_ag_HFO <- H2O_ag[grepl('HFO', H2O_ag$Sample),]
  H2O_total_HFO <- sum(H2O_ag_HFO$x)
  
  H2O_ag_KH <- H2O_ag[grepl('KH', H2O_ag$Sample),]
  H2O_total_KH <- sum(H2O_ag_KH$x)
  
  H2O_ag_MOP <- H2O_ag[grepl('MOP', H2O_ag$Sample),]
  H2O_total_MOP <- sum(H2O_ag_MOP$x)
  
  H2O_ag_FGL <- H2O_ag[grepl('FGL', H2O_ag$Sample),]
  H2O_total_FGL <- sum(H2O_ag_FGL$x)
  
  H2O_ag_AUD <- H2O_ag[grepl('AUD', H2O_ag$Sample),]
  H2O_total_AUD <- sum(H2O_ag_AUD$x)
  
  H2O_ag_STKE <- H2O_ag[grepl('STKE', H2O_ag$Sample),]
  H2O_total_STKE <- sum(H2O_ag_STKE$x)
  
  H2O_ag_STKW <- H2O_ag[grepl('STKW', H2O_ag$Sample),]
  H2O_total_STKW <- sum(H2O_ag_STKW$x)
  
  MET52_ag_Lila <- MET52_ag[grepl('LiLa', MET52_ag$Sample),]
  MET52_total_Lila <- sum(MET52_ag_Lila$x)
  
  MET52_ag_AL <- MET52_ag[grepl('AL', MET52_ag$Sample),]
  MET52_total_AL <- sum(MET52_ag_AL$x)
  
  MET52_ag_BF <- MET52_ag[grepl('BF', MET52_ag$Sample),]
  MET52_total_BF <- sum(MET52_ag_BF$x)
  
  MET52_ag_FS <- MET52_ag[grepl('FS', MET52_ag$Sample),]
  MET52_total_FS <- sum(MET52_ag_FS$x)
  
  MET52_ag_GT <- MET52_ag[grepl('GT', MET52_ag$Sample),]
  MET52_total_GT <- sum(MET52_ag_GT$x)
  
  MET52_ag_HGA <- MET52_ag[grepl('HGA', MET52_ag$Sample),]
  MET52_total_HGA <- sum(MET52_ag_HGA$x)
  
  MET52_ag_HFO <- MET52_ag[grepl('HFO', MET52_ag$Sample),]
  MET52_total_HFO <- sum(MET52_ag_HFO$x)
  
  MET52_ag_KH <- MET52_ag[grepl('KH', MET52_ag$Sample),]
  MET52_total_KH <- sum(MET52_ag_KH$x)
  
  MET52_ag_MOP <- MET52_ag[grepl('MOP', MET52_ag$Sample),]
  MET52_total_MOP <- sum(MET52_ag_MOP$x)
  
  MET52_ag_FGL <- MET52_ag[grepl('FGL', MET52_ag$Sample),]
  MET52_total_FGL <- sum(MET52_ag_FGL$x)
  
  MET52_ag_AUD <- MET52_ag[grepl('AUD', MET52_ag$Sample),]
  MET52_total_AUD <- sum(MET52_ag_AUD$x)
  
  MET52_ag_STKE <- MET52_ag[grepl('STKE', MET52_ag$Sample),]
  MET52_total_STKE <- sum(MET52_ag_STKE$x)
  
  MET52_ag_STKW <- MET52_ag[grepl('STKW', MET52_ag$Sample),]
  MET52_total_STKW <- sum(MET52_ag_STKW$x)
  
  
  graph <- matrix(c(MET52_total_STKW, H2O_total_STKW, MET52_total_STKE, H2O_total_STKE, MET52_total_AUD, H2O_total_AUD, MET52_total_FGL, H2O_total_FGL, MET52_total_MOP, H2O_total_MOP, MET52_total_Lila, H2O_total_Lila, MET52_total_KH, H2O_total_KH, MET52_total_HFO, H2O_total_HFO, MET52_total_HGA, H2O_total_HGA, MET52_total_GT, H2O_total_GT, MET52_total_FS, H2O_total_FS, MET52_total_BF, H2O_total_BF, MET52_total_AL, H2O_total_AL),ncol=26,byrow=TRUE)
  colnames(graph) <- c("MET52_STKW", "H2O_STKW", "MET52_STKE", "H2O_STKE", "MET52_AUD", "H2O_AUD", "MET52_FGL", "H2O_FGL", "MET52_MOP", "H2O_MOP", "MET52_Lila", "H2O_Lila", "MET52_KH", "H2O_KH", "MET52_HFO", "H2O_HFO", "MET52_HGA", "H2O_HGA", "MET52_GT", "H2O_GT", "MET52_FS", "H2O_FS", "MET52_BF", "H2O_BF", "MET52_AL", "H2O_AL")
  rownames(graph) <- c("Number")
  graph <- as.table(graph)
  graph
  
  barplot(graph)
  
  
  
}

ag1("dataSet.csv")