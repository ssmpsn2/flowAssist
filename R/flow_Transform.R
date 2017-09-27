#' Transform Flow Data
#'
#' Simplified and more flexible wrapper for the flowTrans() function supplied by the
#' flowTrans package.
#' @param flowObj A data frame, flowFrame, or a list containing these data types
#' @param transformation The transformation to be performed, case insensitive, quotations allowed but not required.
#'   Transformation options: arcsin OR arcsinh (Multivariate arc-sinh transform), box-cox OR boxcox (Multivariate box-cox transform),
#'   biexponential OR biexp (Multivariate biexponential transform), linlog OR lin-log (Multivariate lin-log transform).
#' @return A transformed flow object of the type initially passed to the function
#' @import flowTrans
#' @import flowClust
#' @export
flow_Transform<-function(flowObj, transformation) {

  ## Check for valid arguments
  if(is.null(flowObj)){
    stop("Invalid argument, NULL flow object")
  }

  if(missing(transformation)){
    stop("No transformation specified")
  }

  ## Parse the kind of transformation the user would like to perform
  transformation<-toupper(deparse(substitute(transformation)))
  transformation<-gsub("\"", "", transformation)

  if(transformation == "ARCSIN" | transformation == "ARCSINH"){
    transformation<-"mclMultivArcSinh"
  }

  else if(transformation == "BOX-COX" | transformation == "BOXCOX"){
    transformation<-"mclMultivBoxCox"
  }

  else if(transformation == "BIEXPONENTIAL" | transformation == "BIEXP"){
    transformation<-"mclMultivBiexp"
  }

  else if(transformation == "LINLOG" | transformation == "LIN-LOG"){
    transformation<-"mclMultivLinLog"
  }

  else{
    stop("Transformation not recognized")
  }

  ## The actual transformation function, built on the flowTrans() function
  flowFrame.Transform<-function(flowObj, transformation) {
    flowFrameTrans<-flowTrans::flowTrans(dat=flowObj, fun=transformation, colnames(flowObj), n2f=FALSE, parameters.only = FALSE)
    return(flowFrameTrans$result)
  }

  ## How to call the flowTrans() function based on the type of data passed to flow.Transform()
  if(class(flowObj) == "data.frame"){
    asFlowFrame<-DFtoFF(flowObj)
    flowResults<-flowFrame.Transform(asFlowFrame, transformation)
    flowResults<-FFtoDF(flowResults)
    return(flowResults)
  }

  if(class(flowObj) == "flowFrame"){
    return(flowFrame.Transform(flowObj, transformation))
  }

  if(class(flowObj) == "list"){
    flowList<-as.list(NULL)
    length(flowList)<-length(flowObj)

    for(i in 1:length(flowObj)){
      if(class(flowObj[[i]]) == "data.frame"){
        asFlowFrame<-DFtoFF(flowObj[[i]])
        flowResults<-flowFrame.Transform(asFlowFrame, transformation)
        flowList[[i]]<-FFtoDF(flowResults)
        names(flowList)[[i]]<-names(flowObj)[[i]]
      }

      if(class(flowObj[[i]]) == "flowFrame"){
        flowList[[i]]<-flowFrame.Transform(flowObj[[i]], transformation)
        names(flowList)[[i]]<-names(flowObj)[[i]]
      }
    }
    return(flowList)
  }
}
