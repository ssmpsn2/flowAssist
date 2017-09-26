#' Parameter Table Importer
#'
#' Function for reading in a tab-delimited file from the working directory containing
#'   labels for fluorophores and markers used in a flow experiment. The table is used
#'   by the flow_MarkerApply() function to apply more descriptive and informative labels
#'   to flow data frames. Optionally allows for description of upper and lower parameter
#'   bounds for removing unwanted data from flow frames using the flow_Prune() function.
#'   Table should have a header with the following labels:
#'   Column 1) "Fluorophore"
#'   Column 2) "Marker"
#'   Optional Column 3) "UpperBound"
#'   Optional Column 4) "LowerBound"
#'   Fill empty cells (e.g., marker value for FSC, or no argument for upper/lower bound) with "NA".
#'   The parameter labels themselves require no specific order/punctuation/casing for
#'   fluorophores or markers, but the fluorophore names should match those applied to
#'   the FCS files being worked with. Entering an Upper and LowerBound value of "0" for
#'   a parameter indicates that that parameter should be dropped when the flow_Prune() function
#'   is called.
#' @param parameterTableFileName Character string representing file name of parameter table in working directory.
#' @export
get_Parameters<-function(parameterTableFileName) {

  if(missing(parameterTableFileName)){
    stop("Parameter table file name missing")
  }

  if(length(list.files(pattern = parameterTableFileName)) == 0){
    stop("Could not find parameter table in current working directory")
  }

  parameters<-as.data.frame(read.table(parameterTableFileName, header = TRUE, stringsAsFactors = FALSE, na.strings = "NA", fill = TRUE))

  # Parse all column titles to standardized format
  for (i in 1:ncol(parameters)) {
    colnames(parameters)[i]<-gsub("[^[:alnum:]]", "", colnames(parameters)[i])
    colnames(parameters)[i]<-toupper(colnames(parameters)[i])
  }

  # Remove punctuation from parameter labels and convert to uppercase
  for (i in 1:nrow(parameters)) {
    parameters$FLUOROPHORE[i]<-gsub("[^[:alnum:]]", "", as.character(parameters$FLUOROPHORE[i]))
    parameters$FLUOROPHORE[i]<-toupper(parameters$FLUOROPHORE[i])
    parameters$MARKER[i]<-gsub("[^[:alnum:]]", "", as.character(parameters$MARKER[i]))
    parameters$MARKER[i]<-toupper(parameters$MARKER[i])
  }

  ## Rename the rows to the fluorophore labels and drop the fluorophore column
  rownames(parameters)<-parameters$FLUOROPHORE
  parameters<-parameters[,-1]

  assign("paramInfo", parameters, envir = globalenv())
}
