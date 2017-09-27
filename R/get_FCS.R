#' FCS File Importer
#'
#' Generic function for pulling in FCS files and returning a list of flow frames.
#'   Default behavior is to import all .fcs files in the working directory. Names each element of the list using the associated filename, minus the ".fcs" file extension.
#' @param fileName (Optional) Character string designating a file name pattern.
#'   Only files that match the pattern will be imported.
#'
#' @param path (Optional) Character string designating the file path in which to
#'   look for FCS files
#'
#' @param asDF Logical value representing whether or not to convert the incoming
#'   FCS files to dataframes, instead of flow frames. Defaults to FALSE.
#' @return A list of data frames or flow frames.
#' @import flowCore
#' @export
get_FCS<-function(fileName, path, asDF = FALSE) {

  flowFrameList<-list()
  currentWD<-getwd()

  if(missing(path)){

    if (missing(fileName)) {
      fileNames<-list.files(pattern = ".fcs$", ignore.case = TRUE)
      
      if(length(fileNames) == 0L){
        stop("No FCS files found in current working directory.")
      }
      
    }

    else {
      fileNames<-list.files(pattern = fileName)
      if (length(fileNames) == 0L) {
        stop(paste("File name not found in", getwd()))
      }
    }
  }

  else {
    if (missing(fileName)) {
      fileNames<-list.files(path = path, pattern = ".fcs$", ignore.case = TRUE)
      if (length(fileNames) == 0L) {
        stop(paste("File name not found in", filePath))
      }
    }
    else {
      fileNames<-list.files(path = path, pattern = fileName)
      if (length(fileNames) == 0L) {
        stop("File name not found, or unable to path correctly")
      }
    }

    setwd(path)
    if (getwd() == currentWD) {
      stop("Unable to path correctly")
    }
  }

  for (i in 1:length(fileNames)) {
    flowFrameList[[i]]<-flowCore::read.FCS(fileNames[i], transformation = FALSE)
  }

  setwd(currentWD)

  if(asDF){
    flowFrameList<-FFtoDF(flowFrameList)
  }

  names(flowFrameList)<-gsub(".fcs","", fileNames)

  # Parse column names of all incoming FCS files to a standardized,
  # no punctuation format
  for(i in 1:length(flowFrameList)) {
    flowColumnNames<-colnames(flowFrameList[[i]])
    flowColumnNames<-sub("^<","",flowColumnNames)
    flowColumnNames<-sub("-(A|A>)","",flowColumnNames)
    flowColumnNames<-gsub("[^[:alnum:]]", "", flowColumnNames)
    flowColumnNames<-toupper(flowColumnNames)
    colnames(flowFrameList[[i]])<-flowColumnNames
  }

  return(flowFrameList)

}
