#' Data frame to flowFrame
#'
#' Utility function for coercing data frames to the 'flowFrame' S4 class, which many flow
#'   packages are designed to work with.
#'
#' @param DF A data frame or list of data frames to be converted
#' @return A flowFrame or list of flowFrames
DFtoFF<-function(DF){
  if(class(DF) == "data.frame"){
    return(flowFrame(as.matrix(DF)))
  }
  if(class(DF) == "list"){
    frameList<-as.list(NULL)
    length(frameList)<-length(DF)
    for(i in 1:length(DF)){
      if(class(DF[[i]]) == "data.frame"){
        frameList[[i]]<-flowFrame(as.matrix(DF[[i]]))
        names(frameList)[[i]]<-names(DF)[[i]]
      }
      else{
        warning(paste("Object at index",i,"not of type data.frame"))
      }
    }
    return(frameList)
  }
  else {
    stop(" Object is not of type data.frame")
  }

}
