#' Convert a flowFrame to a data frame
#'
#' Utility function for coercing S4-type flowFrame objects to data frames
#'
#' @param FF A flowFrame or list of flowFrames to be converted
#' @return A data frame or list of data frames
#' @import flowCore
#' @export
FFtoDF<-function(FF){

  if(class(FF) == "flowFrame"){
    return(as.data.frame(exprs(FF)))
  }

  if(class(FF) == "list"){
    frameList<-list()
    length(frameList)<-length(FF)
    for(i in 1:length(FF)){
      if(class(FF[[i]]) == "flowFrame"){
        frameList[[i]]<-as.data.frame(flowCore::exprs(FF[[i]]))
        names(frameList)[[i]]<-names(FF)[[i]]
      }
      else{
        warning(paste("Object at index",i,"not of type flowFrame"))
      }
    }
    return(frameList)
  }
  else {
    stop("Object is not of type flowFrame")
  }
}
