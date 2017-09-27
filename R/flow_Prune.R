#' Prune Flow Data
#'
#' Function to remove columns or events that meet specific conditions from a flow data frame.
#' @param flowObj Data frame, flowFrame, or a list containing either class of object to be manipulated
#' @param parameter String or vector of strings matching a column label or set of labels in the flow data frame.
#'   If a parameter argument is passed but no upper or lower bounds are specified, all designated parameters will
#'   be dropped from the flow data frame.
#' @param upperBound (Optional) Numeric defining the allowed maximum value for a specific parameter (if designated by the parameter argument)
#'   or for any parameter (if no parameter argument provided). If a value falls outside this bound, the entire event
#'     (row) will be removed from the flow data frame.
#' @param lowerBound See upperBound
#' @param fromTable (Optional, dependent on calling get_Parameters() function) Logical indicating whether
#'   or not the function should look in the parameter table for upper/lower bound arguments. If true,
#'   ignores provided parameter, upperBound, and lowerBound arguments (all the logic is in the table).
#'   Default = FALSE
#' @return A pruned flow object of the type initially passed to the function
#' @export
flow_Prune<-function(flowObj, parameter, upperBound = NULL, lowerBound = NULL, fromTable = FALSE) {

  ## flowFrame.Prune() is defined here so it can be called by flow.Prune() in a way that makes sense given
  ## the data type the function is passed
  flowFrame.Prune<-function(flowDataFrame = flowObj, parameter, upperBound, lowerBound, fromTable) {

    ## Function for handling pruning when the parent function is called without a parameter table
    pruneIterateNoTable<-function(flowDataFrame, parameter, upperBound, lowerBound){
      Switch<-FALSE
      colNum<-ncol(flowDataFrame)
      columnNames<-toupper(colnames(flowDataFrame))

      ## If a parameter argument was supplied...
      if(!missing(parameter)){

        parameter<-toupper(parameter)

        ## If "parameter" has multiple elements and there are no upper or lower bounds, remove all
        ## columns of the flow frame that have a matching label to any contained in "parameter"
        if(length(parameter) > 1 & is.null(upperBound) & is.null(lowerBound)){
          for(i in 1:length(parameter)){
            for(colI in 1:colNum){
              if(columnNames[colI] == parameter[i]){
                columnsToDelete<-c(columnNames[colI])
              }
            }
            flowDataFrame<-flowDataFrame[, !names(flowDataFrame) %in% columnsToDelete]
          }
          return(flowDataFrame)
        }

        for(n in 1:length(parameter)){
          for(colI in 1:colNum) {

            ## If a match is found
            if (columnNames[colI] == parameter[n]) {

              Switch<-TRUE

              ## and if no upper/lower bound arguments were supplied
              if(is.null(upperBound)& is.null(lowerBound)) {

                ## remove that column
                flowDataFrame<-flowDataFrame[-colI]
                break()
              }

              else {

                rowCount<-nrow(flowDataFrame[colI])
                colValues<-as.vector(flowDataFrame[,colI])
                rowIndexVector<-NULL

                ## If an upperbound and lowerbound argument were passed
                if(!is.null(upperBound) & !is.null(lowerBound)) {

                  cond<-c(colValues > upperBound | colValues < lowerBound)

                  ## For each element in that column of the flow frame
                  for(rowI in 1:rowCount){

                    ## if the element value is above the upper or below the lower bounds,
                    ## add the row index of that value (cell) to a vector
                    if(cond[rowI]){
                      rowIndexVector<-c(rowIndexVector, rowI)
                    }
                  }

                  ## Remove the cells matching the row indices from the flow frame
                  if(!is.null(rowIndexVector)) {
                    flowDataFrame<-flowDataFrame[-rowIndexVector,]
                    break()
                  }
                }

                else {

                  if(is.null(upperBound) == FALSE) {

                    cond<-c(colValues > upperBound)

                    for(rowI in 1:rowCount){
                      if(cond[rowI]){
                        rowIndexVector<-c(rowIndexVector, rowI)
                      }
                    }
                    if(!is.null(rowIndexVector)) {
                      flowDataFrame<-flowDataFrame[-rowIndexVector,]
                      break()
                    }
                  }

                  if(is.null(lowerBound) == FALSE) {

                    cond<-c(colValues < lowerBound)

                    for(rowI in 1:rowCount){
                      if(cond[rowI]){
                        rowIndexVector<-c(rowIndexVector, rowI)
                      }
                    }
                    if(!is.null(rowIndexVector)) {
                      flowDataFrame<-flowDataFrame[-rowIndexVector,]
                      break()
                    }
                  }
                }
              }
            }
          }
        }
      }

      ## If a parameter argument was NOT supplied but upper or lower bound arguments WERE...
      if(missing(parameter) & (!is.null(upperBound) | !is.null(lowerBound))){

        Switch<-TRUE
        condUpper<-NULL
        condLower<-NULL
        bothConds<-FALSE
        onlyUpper<-FALSE
        onlyLower<-FALSE

        if(!is.null(upperBound) & !is.null(lowerBound)){
          bothConds<-TRUE
        }
        if(!is.null(upperBound) & is.null(lowerBound)){
          onlyUpper<-TRUE
        }
        if(is.null(upperBound) & !is.null(lowerBound)){
          onlyLower<-TRUE
        }

        for(colI in 1:colNum){

          rowIndexVector<-NULL
          rowCount<-nrow(flowDataFrame)

          ## Exit the function with an error message if no events fall within the range specified by
          ## upper and or lower bounds
          if(rowCount == 0){
            stop("No events within specified bounds.")
          }

          colValues<-as.vector(flowDataFrame[,colI])

          if(onlyUpper){
            condUpper<-c(colValues > upperBound)
            for(rowI in 1:rowCount){
              if(condUpper[rowI]){
                rowIndexVector<-c(rowIndexVector, rowI)
              }
            }
            if(!is.null(rowIndexVector)) {
              flowDataFrame<-flowDataFrame[-rowIndexVector,]
            }
          }
          else if(onlyLower){
            condLower<-c(colValues < lowerBound)
            for(rowI in 1:rowCount){
              if(condLower[rowI]){
                rowIndexVector<-c(rowIndexVector, rowI)
              }
            }
            if(!is.null(rowIndexVector)) {
              flowDataFrame<-flowDataFrame[-rowIndexVector,]
            }
          }
          else if(bothConds){
            cond<-c(colValues > upperBound | colValues < lowerBound)
            for(rowI in 1:rowCount){
              if(cond[rowI]){
                rowIndexVector<-c(rowIndexVector, rowI)
              }
            }
            if(!is.null(rowIndexVector)) {
              flowDataFrame<-flowDataFrame[-rowIndexVector,]
            }
          }
        }
      }

      if (Switch == FALSE) {
        stop(paste(as.character(parameter), "parameter not found"))
      }

      return(flowDataFrame)
    }

    ## If fromTable argument is true...
    if (fromTable) {

      ## Sanity checks on the parameter table
      if(!exists("paramInfo")) {
        stop("Parameter table not found-- call get.Parameters()")
      }
      if(is.null(paramInfo)) {
        stop("Parameter table is null")
      }
      if(!"UPPERBOUND" %in% colnames(paramInfo) & !"LOWERBOUND" %in% colnames(paramInfo)){
        stop("No upper or lowerbounds found in parameter table")
      }

      ## For each parameter in the table...
      for (i in 1:nrow(paramInfo)) {

        Switch<-FALSE

        ## Iterate through each column of the flow data frame..
        for (k in 1:ncol(flowDataFrame)) {

          fluoroLabel<-rownames(paramInfo[i,])
          markerLabel<-paramInfo$MARKER[i]
          Upper<-paramInfo$UPPERBOUND[i]
          Lower<-paramInfo$LOWERBOUND[i]

          ## Until a label match is found
          if (fluoroLabel == colnames(flowDataFrame[k]) | (!is.na(markerLabel) & markerLabel == colnames(flowDataFrame[k])) | (!is.na(markerLabel) & paste(markerLabel,"-",fluoroLabel, sep = "") == colnames(flowDataFrame[k]))){
            Switch<-TRUE

            rowCount<-nrow(flowDataFrame[k])
            colValues<-as.vector(flowDataFrame[,k])
            rowIndexVector<-NULL

            ## If 0,0 for upper/lower bound, delete column
            if(!is.na(Upper) & !is.na(Lower) & Upper == 0 & Lower == 0) {
              flowDataFrame<-flowDataFrame[-k]
              break()
            }

            ## If both upper and lower bound provided..
            if(!is.na(Upper) & !is.na(Lower)) {

              ## Generate a logical vector, based on provided arguments,
              ## for each value in flow frame
              cond<-c(colValues > Upper | colValues < Lower)

              ## For each element in that column of the flow frame
              for(rowI in 1:rowCount){

                ## if the element value is above the upper or below the lower bounds,
                ## add the row index of that value (cell) to a vector
                if(cond[rowI]){
                  rowIndexVector<-c(rowIndexVector, rowI)
                }
              }

              ## Use the index vector to delete cells from flow frame
              ## that fall out of described bounds
              if(!is.null(rowIndexVector)) {
                flowDataFrame<-flowDataFrame[-rowIndexVector,]
                break()
              }
            }

            if(!is.na(Upper) & is.na(Lower)) {

              cond<-c(colValues > Upper)

              for(rowI in 1:rowCount){
                if(cond[rowI]){
                  rowIndexVector<-c(rowIndexVector, rowI)
                }
              }
              if(!is.null(rowIndexVector)) {
                flowDataFrame<-flowDataFrame[-rowIndexVector,]
                break()
              }
            }

            if(is.na(Upper) & !is.na(Lower)) {

              cond<-c(colValues < Lower)
              for(rowI in 1:rowCount){
                if(cond[rowI]){
                  rowIndexVector<-c(rowIndexVector, rowI)
                }
              }

              if(!is.null(rowIndexVector)) {
                flowDataFrame<-flowDataFrame[-rowIndexVector,]
                break()
              }
            }
          }
        }
        if (Switch == FALSE) {
          if (!is.na(paramInfo$MARKER[i])){
            warning(paste("Unable to find",paramInfo$MARKER[i],"or",rownames(paramInfo[i,]),"in flow frame"))
          }
          else {
            warning(paste("Unable to find",rownames(paramInfo[i,]),"in flow frame"))
          }
        }
      }
      return(flowDataFrame)
    }

    ## If fromTable argument is false...
    if (!fromTable){
      return(flowDataFrame<-pruneIterateNoTable(flowDataFrame, parameter, upperBound, lowerBound))
    }
  }

  ## Data type manipulation for feeding into the flowFrame.Prune() function
  if(class(flowObj) == "data.frame") {
    return(flowFrame.Prune(flowObj, parameter, upperBound, lowerBound, fromTable))
  }

  if(class(flowObj) == "list") {
    flowList<-as.list(NULL)
    length(flowList)<-length(flowObj)

    for(i in 1:length(flowObj)) {
      if(class(flowObj[[i]]) == "data.frame" ) {
        flowList[[i]]<-flowFrame.Prune(flowObj[[i]], parameter, upperBound, lowerBound, fromTable)
        names(flowList)[[i]]<-names(flowObj)[[i]]
      }
      if(class(flowObj[[i]]) == "flowFrame") {
        flowList[[i]]<-FFtoDF(flowList[[i]])
        flowList[[i]]<-flowFrame.Prune(flowList[[i]], parameter, upperBound, lowerBound, fromTable)
        flowList[[i]]<-DFtoFF(flowList[[i]])
        names(flowList)[[i]]<-names(flowObj)[[i]]
      }
    }
    return(flowList)
  }

  if(class(flowObj) == "flowFrame") {
    flowObj<-FFtoDF(flowObj)
    flowObj<-flowFrame.Prune(flowObj, parameter, upperBound, lowerBound, fromTable)
    return(DFtoFF(flowObj))
  }
}
