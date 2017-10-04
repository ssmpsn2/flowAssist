
#' Retrive Compensation Matrix
#'
#' Reads the metadata of a flowFrame object and returns the compensation matrix which was applied to it.
#' @param flowFrame An object of the flowFrame class which possess compensation metadata.
#' @return A labeled data frame of compensation values.
#' @import flowCore
#' @export

get_CompMatrix<-function(flowFrame){
  
  if(class(flowFrame) != "flowFrame"){
    stop("Object is not of class 'flowFrame'")
  }
  
  if(is.null(keyword(flowFrame, "SPILL"))){
    stop("No compensation metadata found")
  }
  
  ## This returns the spillover matrix used to compensate a flow file as a data frame
  ## (default as a list)
  compDF<-as.data.frame(keyword(flowFrame, "SPILL"))
  
  ## Data frame structure: Column denotes what the detector is intended to detect,
  ## row indicates how much light from the row label fluorophore is spilling into that
  ## detector
  
  ## Parse names
  CDFcLabels<- colnames(compDF)
  CDFcLabels<- as.vector(sapply(CDFcLabels, FUN = sub, pattern = "SPILL.", replacement = ""))
  CDFcLabels<- as.vector(sapply(CDFcLabels, FUN = sub, pattern = ".A", replacement = ""))
  CDFcLabels<- as.vector(sapply(CDFcLabels, FUN = gsub, pattern = "[^[:alnum:]]", replacement = ""))
  cLabels<- as.vector(sapply(CDFcLabels, FUN = paste, "detector", sep = "-"))
  rLabels<- as.vector(sapply(CDFcLabels, FUN = paste, "spillover"))
  
  colnames(compDF)<-cLabels
  row.names(compDF)<-rLabels
  
  return(compDF)
}

## Potential added functionality-- if called on a list, quickly determine how many unique
## comp matrices there are within the list, then return a list of all unique matrices.

## Name each element of the comp matrix list in a way that describes which flowFrames
## of the originally passed list each matrix applies to?