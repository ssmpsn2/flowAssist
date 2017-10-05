#' Visually Inspect Compensation Samples
#' 
#' A function for building flowJo V10-like plots that allow for visual inspection of
#' the quality of acquired compensation matrices
#' @param compList A list of flowFrames containing compensation control data. Samples may need
#'   pre-processing (to remove doublets, outliers) to generate nice-looking plots.
#' @param parameter Optional character string defining a single compensation sample (e.g., PE control) to build a plot for. 
#'   If not provided, defaults to building plots for all items in compList.
#' @param compMatrix Optional data frame containing compensation values. By default, the compensation matrix which is applied to
#'   compList should be the matrix that was built during acquisition to compensate the actual experimental samples.
#'   This can be easily obtained by calling the get_CompMatrix() function on a flowFrame containing experimental data, but
#'   only if the flowFrame has never been converted to a data frame, as this drops all experimental metadata associated with
#'   the flowFrame class.
#' @import ggplot2
#' @import flowCore
#' @import ggcyto
#' @import grid
#' @import gridExtra
#' @export

## Compensation samples require some amount of pre-processing to make nice plots!
flow_CompPlot<-function(compList, parameter, compMatrix) {
  
  if(missing(compMatrix)){
    warning("No compensation matrix provided-- Plotted data is likely uncompensated!")
  }
  
  ## Pre-process the compList to drop unneeded channels
  compList<-flow_Prune(compList, c("FSC", "SSC", "FSCH", "SSCH", "FSCW", "SSCW", "TIME"))
  
  sampleNames<-toupper(names(compList))
  sampleNames<-as.vector(sapply(sampleNames, sub, pattern = "^COMPENSATION CONTROLS_", replacement = ""))
  sampleNames<-as.vector(sapply(sampleNames, sub, pattern = " STAINED CONTROL_...", replacement = ""))
  sampleNames<-as.vector(sapply(sampleNames, gsub, pattern = "[^[:alnum:]]", replacement = ""))
  names(compList)<-sampleNames
  
 
  
  ## The plotting function that will be called to generate all the plots we want,
  ## where DF is the flow data itself, xAes is what we want on the x-axis (the fluorophore with which the sample was stained),
  ## and yAes is what we want on the y-axis (the various channels into which the signal may be spilling over)
  compPlot<-function(DF, xAes, yAes) {
    return(ggplot(DF, aes_string(x=xAes, y=yAes)) + geom_point(alpha = 0.05, show.legend = FALSE) + scale_x_flowJo_biexp() + scale_y_flowJo_biexp(pos = 4.5, neg = 0.5) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")))
  }
  
  compPlotGen<-function(compList, parameter, compMatrix) {
    ## Define the sample
    sampleOfInterest<-compList[[parameter]]
    
    ## Apply the provided compensation matrix to the sample, if it exists
    if(!missing(compMatrix)){
      sampleOfInterest<-compensate(sampleOfInterest, compMatrix)
    }
    
    ## If the designated sample name is also a fluorescence channel in the sample...
    if(parameter %in% colnames(sampleOfInterest)){
      
      ## Define a vector of channel names that excludes the fluorescence channel of the sample (we don't
      ## care to look at PE vs. PE, for example)
      iVec<-colnames(sampleOfInterest) != parameter
      yAesVec<-colnames(sampleOfInterest)[iVec]
      
      ## Call the compPlot function over the compensated sample of interest data frame, using
      ## each element of the yAesVec vector as an argument 
      ## The data frame itself needs to be part of MoreArgs and wrapped in a list, otherwise
      ## mapply() thinks you want to iterate sequentially over the columns of the data frame
      plotList<-mapply(compPlot, xAes = parameter, yAes = yAesVec, MoreArgs = list(sampleOfInterest), SIMPLIFY = FALSE)
      
      ## Uses the list of plots to generate a grid plot
      do.call("grid.arrange", c(plotList, ncol(plotList)))
    }
  }
  
  ## Make an empty list to hold the created plots
  plotList<-list()
  
  ## If a parameter argument was provided, and that string is in the vector of sample names...
  if(!missing(parameter)){
    if(parameter %in% sampleNames){
      compPlotGen(compList, parameter, compMatrix)
    }
  }
  
  if(missing(parameter)){
    mapply(compPlotGen, parameter = sampleNames, MoreArgs = list(compList, compMatrix), SIMPLIFY = FALSE)
  }
}
