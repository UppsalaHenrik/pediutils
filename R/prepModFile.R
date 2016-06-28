

prepModFile <- function(modFilePath, mceta){

  # Check that the file exists
  if(!file.exists(modFilePath)){
    stop("File ", modFilePath, " not found.")
  }
  
  # Print a message
  print(paste("Preparing model file", modFilePath, "by removing comments",
              ", setting MAXEVALS and unfixing any parameters"))
  modFileOrig <- readLines(modFilePath)
  
  # Removing comments
  modFile <- gsub("[[:space:]];.+", "", modFile)
  
  # Unfix all parameters. We control the exact values anyway. This may 
  modFile <- gsub("[[:space:]]FIXED", "", modFile)
  modFile <- gsub("[[:space:]]FIX", "", modFile)

  # Get the $ statement rows. 
  dollarAndLastRows <- c(grep("^\\$", modFile), length(modFile)+1)

  # Get $EST, $COV and $TABLE rows for deletion
  estRows <- grep("^\\$EST", modFile[dollarAndLastRows])
  covRows <- grep("^\\$COV", modFile[dollarAndLastRows])
  tableRows <- grep("^\\$TAB", modFile[dollarAndLastRows])
  
  dollarRowsToDelIndex <- c(estRows,covRows,tableRows)
  
  # Picking out all rows from each $-statement to delete up to 
  # the next $-statement
  rowsToDelList <- lapply(dollarRowsToDelIndex, function(x){
    rows <- dollarAndLastRows[x]:(dollarAndLastRows[x+1]-1)
  })
  
  rowsToDel <- unlist(rowsToDelList)

  modFile <- modFile[-rowsToDel]
  
  
  # So far I'm hardcoding the estimation statement. There could be benefit
  # to opening this up to user specified as well.
  newEstRow <- "$EST METHOD=COND INTER MAXEVALS=0 PRINT=9999"
  modFile <- c(modFile, newEstRow)
  
  newModFileName <- paste0("new_", basename(modFilePath))
  writeLines(modFile, newModFileName)
  
  return(newModFileName)
}