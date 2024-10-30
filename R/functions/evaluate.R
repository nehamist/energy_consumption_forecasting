#Split data into train and test
train_test_split <- function(dt, ratio = 0.8) {
  lst <- list()
  offset_index <- round(NROW(dt)*ratio)
  lst$dt_train <- dt[1:offset_index]
  lst$dt_test <- dt[(offset_index+1):NROW(dt)]
  return(lst)
}

####### MODEL LOAD & SAVE #######

#Save Model
saveModel <- function(model, type, freq, subpath = NULL, path = "C:/Users/vgupta/Documents/Neha Thesis Ws/Model") {
  if (!is.null(subpath))
    path <- file.path(path, subpath)
  saveRDS(model, file = file.path(path,paste0(type, "_", freq, ".rds")))
}

#Load Model
loadModel <- function(type, freq, subpath = NULL) {
  path = "C:/Users/vgupta/Documents/Neha Thesis Ws/Model"
  if (!is.null(subpath))
    path <- file.path(path, subpath)
  model <- readRDS(file.path(path,paste0(type, "_", freq, ".rds")))
  return(model)
}



