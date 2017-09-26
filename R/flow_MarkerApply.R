#' Apply informative marker labels to imported flow data
#'
#' Using the parameter table called by the get_Parameters() function, applies
#'   marker labels to flow data frames.
#'@param keepFluoro (Optional) Logical, if TRUE appends the marker label to the fluorescence label
#'@param sepChar (Optional, dependent on keepFluoro = TRUE) String defining the character separating
#'  the marker and fluorescene labels. Default = "-"
flow_MarkerApply<- function(flowObj, keepFluoro = FALSE, sepChar = "-") {
  flowFrame.MarkerApply<- function(flowDataFrame = flowObj, keepFluoro, sepChar) {
    if (exists("paramInfo") == FALSE) {
      stop("No parameter table found")
    }

    if (is.null(paramInfo) == TRUE) {
      stop("Parameter table is null")
    }

    if (is.null(flowDataFrame) == TRUE) {
      stop("Flow object is null")
    }

    ## For each column in data frame containing flow data:
    for(i in 1:ncol(flowDataFrame)) {

      ## Read column name
      flow_column_name<-colnames(flowDataFrame[i])

      ## If column name = FSC, SSC, or Time, skip it
      paramsToIgnore<-c("FSC", "FSCH", "SSC", "SSCH", "TIME")
      if(flow_column_name %in% paramsToIgnore) {
        next()
      }

      else {

        ## Compare the column name to the fluorescence parameters in the
        ## provided label file
        ##
        ## If matched, rename the column of the flow data frame to marker label

        Switch<-FALSE

        if(flow_column_name %in% rownames(paramInfo)){


          if(keepFluoro){
            if(sepChar == "-") {
              colnames(flowDataFrame)[i]<-paste(paramInfo[flow_column_name,1], flow_column_name, sep = "-")
              Switch<-TRUE
            }
            else {
              colnames(flowDataFrame)[i]<-paste(paramInfo[flow_column_name,1], flow_column_name, sep = sepChar)
              Switch<-TRUE
            }

          }
          if(!keepFluoro) {
            colnames(flowDataFrame)[i]<-paramInfo[flow_column_name,1]
            Switch<-TRUE
          }
        }

        if(Switch == FALSE) {
          warning(paste("Unable to assign", flow_column_name, "to a marker, check parameter table"))
        }

      }

    }
    return(flowDataFrame)
  }

  if(class(flowObj) == "data.frame") {
    return(flowFrame.MarkerApply(flowObj, keepFluoro, sepChar))
  }

  if(class(flowObj) == "list") {
    flowList<-as.list(NULL)
    for(i in 1:length(flowObj)) {
      if(class(flowObj[[i]]) == "data.frame"){
        flowList[[i]]<-flowFrame.MarkerApply(flowObj[[i]], keepFluoro, sepChar)
        names(flowList)[[i]]<-names(flowObj)[[i]]
      }
      if(class(flowObj[[i]]) == "flowFrame"){
        flowList[[i]]<-FFtoDF(flowObj[[i]])
        flowList[[i]]<-flowFrame.MarkerApply(flowList[[i]], keepFluoro, sepChar)
        flowList[[i]]<-DFtoFF(flowList[[i]])
        names(flowList)[[i]]<-names(flowObj)[[i]]
      }
    }
    return(flowList)
  }

  if(class(flowObj) == "flowFrame") {
    flowObj<-FFtoDF(flowObj)
    flowObj<-flowFrame.MarkerApply(flowObj, keepFluoro, sepChar)
    return(DFtoFF(flowObj))
  }

}
