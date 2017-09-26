#' Subset Flow Data
#'
#' Simple function which returns a random subset of values from the flow object it is passed.
#' @param flowObj A data frame, flowFrame, or a list containing these data types
#' @param subsetPercent Numeric from 0-1 denoting the percentage of the original data set to return
#' @return A subsetted flow object of the type initially passed to the function
#' @export
flow_Subset<-function(flowObj, subsetPercent){

  if(subsetPercent >= 1 | subsetPercent <= 0){
    stop("Enter a subset value between 0 and 1")
  }

  subsetter<-function(flowDataFrame, subsetPercent){
    eventNumber<-nrow(flowDataFrame)*subsetPercent
    return(flowDataFrame[sample(nrow(flowDataFrame), eventNumber),])
  }

  if(class(flowObj) == "data.frame") {
    return(subsetter(flowObj, subsetPercent))
  }

  if(class(flowObj) == "list") {
    flowList<-as.list(NULL)
    length(flowList)<-length(flowObj)

    for(i in 1:length(flowObj)) {
      if(class(flowObj[[i]]) == "data.frame" ) {
        flowList[[i]]<-subsetter(flowObj[[i]], subsetPercent)
        names(flowList)[[i]]<-names(flowObj)[[i]]
      }
      if(class(flowObj[[i]]) == "flowFrame") {
        flowList[[i]]<-as.data.frame(exprs(flowObj[[i]]))
        flowList[[i]]<-subsetter(flowObj[[i]], subsetPercent)
        flowList[[i]]<-flowFrame(as.matrix(flowList[[i]]))
        names(flowList)[[i]]<-names(flowObj)[[i]]
      }
    }
    return(flowList)
  }

  if(class(flowObj) == "flowFrame") {
    flowObj<-as.data.frame(exprs(flowObj))
    flowObj<-subsetter(flowObj, subsetPercent)
    return(flowFrame(as.matrix(flowObj)))
  }

}
