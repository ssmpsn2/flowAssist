#' Import Packages
#' 
#' Simple function for downloading and installing necessary packages for flow analysis.
#' @export
Import_Packages<- function(){
  source("https://bioconductor.org/biocLite.R")
  biocLite("flowCore")
  biocLite("flowClust") ## Only compatible with <R 3.3.x
  biocLite("flowViz")
  biocLite("flowTrans")
  biocLite("ggcyto")
  biocLite("flowWorkspace") ## Only compatible with >R 3.4.x
  
  install.packages("ggplot2")
  install.packages("tidyr")
  install.packages("plyr")
  install.packages("RColorBrewer")
  install.packages("cluster")
  install.packages("NbClust")
  install.packages("dplyr")
  
  if(!require(devtools)) install.packages("devtools")
  devtools::install_github("kassambara/factoextra")
}