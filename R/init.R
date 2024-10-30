#Load liabraries << if not available, then install >>
if (!require(pacman)) install.packages("pacman")
pacman::p_load("data.table", "rhdf5", "ggplot2", 
               "scales", "lubridate", "stringr", 
               "bit64", "dplyr", "stats", "zoo", 
               "caret", "tuneRanger", "mlr", 
               "ranger", "xgboost")
Sys.setlocale("LC_TIME", "en_US")

#Function to source project folders
sourceFolder <- function(dirPath) {
  if (!dir.exists(dirPath)) {
    cat(paste0("Directory '", dirPath, "' does not exist. Skipping...\n"))
  } else {
    files <- list.files(dirPath)
    files <- files[grepl(".R$", files) | grepl(".r$", files)]
    len <- length(files)
    if (len > 0) {
      cat(paste0("Sourcing R files in folder '", dirPath, "'...\n"))
      for (i in 1:len) {
        curFile <- file.path(dirPath, files[i])
        source(curFile)
      }
    }
  }
}

sourceFolder("R/functions")
sourceFolder("R/misc")

basedata_path <- file.path(dirname(getwd()), "Data")