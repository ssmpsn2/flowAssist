#' Optimal Number of Clusters for Flow Data
#'
#' Function implementing various statistical methods to determine the optimal number of clusters for flow cytometry data,
#'   using various clustering algorithms. ***WARNING*** extremely computationally intensive-- very large data sets may not
#'   function and/or may take significant time to complete
#' @param flowObj A data frame or flowFrame to be analyzed
#' @param algorithm The clustering algorithm to utilize and attempt to generate an optimal cluster number for
#'   Options (no quotations): kmeans, pam, clara, hcut
#' @param kMax Integer representing max number of possible clusters to consider
#' @param nBoot Integer representing number of times to repeat the analysis from a random starting position, must be >1 for gap statistic analysis
#' @param plot Logical indicating whether or not to generate graphical plots for wss, silhouette, and gap statistic methods
#' @import NbClust
#' @import flowViz
#' @import factoextra
#' @import cluster
#' @import ggplot2
#' @export
flow_OptimalClust<-function(flowObj, algorithm, kMax, nBoot, plot = TRUE) {

  alg<-deparse(substitute(algorithm))

  genOptimalClust<-function(flowDataFrame, algorithm, kMax, nBoot, plot){
    set.seed(1L)
    wss<-factoextra::fviz_nbclust(flowDataFrame, algorithm, method = "wss", k.max = kMax, nboot = nBoot)
    x<-wss$data$y
    for (i in 1:kMax){
      # instSlope<-(1-(x[i+1]/x[i]))

      percentVarExplained<-(x[i]-x[1])/(x[kMax]-x[1])
      nextI<-(x[i+1]-x[1])/(x[kMax]-x[1])
      totalVarExplainedROC<-1-(percentVarExplained/nextI)


      if(totalVarExplainedROC < 0.05){
        wssNum<-i
        print(paste("wss method suggested cluster number:",wssNum))
        break()
      }
    }

    if(plot){
      ggplot2::plot(wss + ggplot2::geom_vline(xintercept = wssNum, linetype = 2))
    }

    sil<-factoextra::fviz_nbclust(flowDataFrame, algorithm, method = "silhouette", k.max = kMax, nboot = nBoot)
    silNum<-which.max(sil$data$y)
    print(paste("Silhouette method suggested cluster number:", silNum))

    if(plot){
      plot(sil)
    }

    if(nBoot > 1) {
      gap<-cluster::clusGap(flowDataFrame, algorithm, K.max = kMax, B = nBoot, d.power = 2, verbose = FALSE)
      y<-as.data.frame(gap$Tab)
      gapNum<-cluster::maxSE(y$gap, SE.f = y$SE.sim, method = "firstSEmax", SE.factor = 1)
      print(paste("Gap statistic method suggest cluster number:", gapNum))

      if(plot){
        plot(factoextra::fviz_gap_stat(gap, linecolor = "steelblue", maxSE = list(method = "firstSEmax", SE.factor = 1)))
      }
    }

    else{
      print("Gap statistic not performed when nBoot = 1!")
    }

    nb<-NbClust::NbClust(flowDataFrame, distance = "euclidean", min.nc = 2, max.nc = kMax, method = alg)

  }

  if(class(flowObj) == "data.frame" | class(flowObj) == "matrix"){
    return(genOptimalClust(flowObj, algorithm, kMax, nBoot, plot))
  }

  if(class(flowObj) == "flowFrame"){
    return(genOptimalClust(FFtoDF(flowObj), algorithm, kMax, nBoot, plot))
  }

  else(
    stop("Object type not supported.")
  )

}
