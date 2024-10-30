source("R/init.R")

#To get data
dt <- getAnalysisData(start_year = 2018, end_year = 2020, freq = 60,
                      data_type = "NO_PV_HP", holiday = F, weather = F)

#get additional features
dt <- getAdditionalDateFeatures(dt, onehotenc = F)

# lag columns
dt[, consupmtion_lag_1day := lag(P_TOT_SUM), by = c("time_hr", "time_min")]
dt[, consupmtion_lag_7day := lag(P_TOT_SUM), by = c("weekday", "time_hr", "time_min")]
dt <- dt[, -c("weekday", "time_hr", "time_min")]

# final data cleansing
#dt <- dt[673:NROW(dt)]  #15 min
dt <- dt[169:NROW(dt)]  # hourly

#Train and test data
lst <- train_test_split(dt, ratio = 0.8)
timestampVec <- lst$dt_test[["timestamp"]]
actuals <- lst$dt_test$P_TOT_SUM

######### MODEL FITTING #########
freq <- 60
subpath <- "hh"

# linear regression model
# Set seed for reproducibility
set.seed(123)

# Set up repeated k-fold cross-validation
remVars <- c("timestamp")
train.control <- caret::trainControl(method = "cv", number = 5)

# Train the model in forward direction feature selection
flr_model <- caret::train(P_TOT_SUM ~., 
                          data = lst$dt_train[, -c(remVars), with = F],
                          method = "glmStepAIC",
                          direction="forward", # Backward selection # Forward selection
                          #tuneGrid = data.frame(nvmax = 1:20),
                          trControl = train.control
)
saveModel(flr_model, subpath = subpath, type = "flr", freq)

# Test
flr_preds <- predict(flr_model, newdata = lst$dt_test[, -c(remVars), with = F])
flr_preds[flr_preds<0] <- 0


# Train the model in backward direction feature selection
blr_model <- caret::train(P_TOT_SUM ~., 
                          data = lst$dt_train[, -c(remVars), with = F],
                          method = "glmStepAIC",
                          direction="backward", 
                          #tuneGrid = data.frame(nvmax = 1:20),
                          trControl = train.control
)
saveModel(blr_model, subpath = subpath, type ="blr", freq)
blr_preds <- predict(blr_model, newdata = lst$dt_train[, -c(remVars), with = F])
blr_preds[blr_preds<0] <- 0

# Random Forset
rf <- ranger(P_TOT_SUM~., data=lst$dt_train[, -c(remVars), with = F])
saveModel(rf, subpath = subpath, type = "rf", freq)
rf_preds <- predict(rf, data = lst$dt_test[, -c(remVars), with = F])$predictions
#sqrt(mean((actuals - rf_preds)^2))

##R square
rf_total <- sum((actuals - mean(rf_preds))^2)
rf_res <- sum((actuals - rf_preds)^2)
r_squared <- 1 - (rf_res / rf_total)

#auto hyperparameters tuning for RF with mlr
#setnames(lst$dt_train, "mnth_März", "mnth_March")
#setnames(lst$dt_test, "mnth_März", "mnth_March")

task <- makeRegrTask(data = lst$dt_train[, -c(remVars), with = F], target = "P_TOT_SUM")
res <- tuneRanger(task, save.file.path = NULL)
saveModel(res$model, subpath = subpath, type = "atrf", freq)
trf_preds <- predict(res$model, newdata = lst$dt_test)
trf_preds <- trf_preds$data$response

#XGBoost model
#define final training and testing sets
remVars <- c("timestamp", "P_TOT_SUM")
xtrain <- data.matrix(lst$dt_train[, -c(remVars), with = F])
train_label <- data.matrix(lst$dt_train[["P_TOT_SUM"]])
xtest <- data.matrix(lst$dt_test[, -c(remVars), with = F])
test_label <- data.matrix(lst$dt_test[["P_TOT_SUM"]])
xgb_train = xgb.DMatrix(data = xtrain, label = train_label)
xgb_test = xgb.DMatrix(data = xtest, label = test_label)

#defining a watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each iteartion
xgboost <- xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)
saveModel(xgboost, subpath = subpath, type = "xgboost", freq)
xgboost_preds <- predict(xgboost, xgb_test)
xgboost_preds[xgboost_preds<0] <- 0

#R squared computation
d <- tibble(pred = predict(xgboost_model, newdata = xgb_train)
            , obs = train_label) %>% 
  mutate(resid = pred - obs,
         resid_sq = resid^2)
sstot <- sum((d$pred - mean(d$obs))^2)
ssresid <- sum(d$resid_sq)
print(100*(1-(ssresid/sstot)))

# Plot Weekly Mean Squared Error
dt_Results <- data.table(timestamp = timestampVec, 
                         Ground_Truth = actuals, 
                         Forward_Linear_Regression = unname(flr_preds),
                         Backward_Linear_Regression = unname(blr_preds),
                         Random_Forest = rf_preds,
                         Autotuned_Random_Forest = trf_preds,
                         XGBoost = xgboost_preds)
fwrite(dt_Results, paste0("C:/Users/vgupta/Documents/Neha Thesis Ws/Results/results_70_21_date_wthr_holi_OhtF_SUM", "_", subpath, "_", freq, ".csv"))
#RF Onehotencoding TRUE Result 
dt_RES_XG <- data.table(timestamp = timestampVec,
                        Ground_Truth = actuals,
                        XGBoost = xgboost_preds)
dt_Error_XG <- copy(dt_RES_XG)
wape_xg_OHT <- (sum(abs(dt_Error_XG$Ground_Truth - dt_Error_XG$XGBoost))/sum(dt_Error_XG$Ground_Truth))*100


xtrain <- data.matrix(lst$dt_train[, -c(remVars), with = F])
train_label <- data.matrix(lst$dt_train[["P_TOT_SUM"]])
xgb_train = xgb.DMatrix(data = xtrain, label = train_label)
xgboost_preds_train <- predict(xgboost, xgb_train)
xgboost_preds_train[xgboost_preds<0] <- 0

actuals <- lst$dt_train$P_TOT_SUM
timestampVec_train <- lst$dt_train[["timestamp"]]


fwrite(dt_RF, paste0("C:/Users/vgupta/Documents/Neha Thesis Ws/RFOhtT", ".csv"))
fwrite(dt_Results, paste0("C:/Users/vgupta/Documents/Neha Thesis Ws/Results/HH_SUM_15", ".csv"))

#to add all results daily
dt_Results[, date := as.Date(timestamp)]
dt_Results <- dt_Results[, -c("timestamp")]
dt_Results <- dt_Results[, lapply(.SD,base::sum), by = c("date")]
###### EVALUATION ######
dt_Results <- fread("C:/Users/vgupta/Documents/Neha Thesis Ws/Results/results_70_21_date_wthr_holi_OhtF_hP_60.csv", dec = ",")
dt_Error <- copy(dt_Results)
dt_Error[, time := format(as.POSIXct(timestamp), format = "%H:%M")]
dt_Error[, miute := minute(timestamp)]
dt_Error[, hour := hour(timestamp)]
dt_ape_flr <- dt_Error[hour == 9]
dt_mape <- dt_ape_flr[, .(Date= timestamp, APE_FLR = (abs(Ground_Truth - Forward_Linear_Regression))/Ground_Truth)]
dt_mape <- dt_Error[, .(Date= timestamp, APE_FLR = (abs(Ground_Truth - Forward_Linear_Regression))/Ground_Truth)]
dt_Error[, FLR_Error := sqrt((Ground_Truth - Forward_Linear_Regression)^2)]
dt_Error[, BLR_Error := sqrt((Ground_Truth - Backward_Linear_Regression)^2)]
dt_Error[, RF_Error := sqrt((Ground_Truth - Random_Forest)^2)]
dt_Error[, ATRF_Error := sqrt((Ground_Truth - Autotuned_Random_Forest)^2)]
dt_Error[, XGB_Error := sqrt((Ground_Truth - XGBoost)^2)]
#dt_Error <- dt_Error[, -c(2:7)]

#Calculate Absolute Percent Error
dt_Error[, FLR_APE := (abs(Ground_Truth - Forward_Linear_Regression)/Ground_Truth)*100]
dt_Error[, RF_APE := (abs(Ground_Truth - Random_Forest)/Ground_Truth)*100]
dt_Error[, ATRF_APE := (abs(Ground_Truth - Autotuned_Random_Forest)/Ground_Truth)*100]
dt_Error[, XGB_APE := (abs(Ground_Truth - XGBoost)/Ground_Truth)*100]
drr <- copy(dt_Error)
drr <- drr[, -c(2:14)]
##plot
drr_long <- melt(drr, id.vars = "timestamp", 
                 measure.vars = c("FLR_APE", "RF_APE", "ATRF_APE", "XGB_APE"),
                 variable.name = "Model", value.name = "APE")

# Create the box plot using ggplot2
ggplot(drr_long, aes(x = Model, y = APE)) +
  geom_boxplot() +
  labs(title = "Absolute Percent Error (APE) Boxplot for 4 Models",
       x = "Model",
       y = "Absolute Percent Error") +
  theme_minimal()


###APE####
#APE_flr <- (abs(dt_Error$Ground_Truth - dt_Error$Forward_Linear_Regression)/dt_Error$Ground_Truth)*100
drr <- copy(dt_Error)
drr[, FLR_APE := (abs(Ground_Truth - Forward_Linear_Regression)/Ground_Truth)*100]

drr[, (abs(drr$Ground_Truth - drr$Forward_Linear_Regression)/drr$Ground_Truth)*100]

drr_Err[, (abs(drr$Ground_Truth - drr$Random_Forest)/drr$Ground_Truth)*100]

drr_Err <- data.table(actual = dt_Error$Ground_Truth, predict = dt_Error$Forward_Linear_Regression)
drr_Err[, absolute_error := abs(actual-predict)]
drr_Err[, absolute_percentage_error := (absolute_error/actual)*100]

### WAPE ###
wape_flr <- (sum(abs(dt_Error$Ground_Truth - dt_Error$Forward_Linear_Regression))/sum(dt_Error$Ground_Truth))*100
wape_blr <- (sum(abs(dt_Error$Ground_Truth - dt_Error$Backward_Linear_Regression))/sum(dt_Error$Ground_Truth))*100
wape_rf <- (sum(abs(dt_Error$Ground_Truth - dt_Error$Random_Forest))/sum(dt_Error$Ground_Truth))*100
wape_arf <- (sum(abs(dt_Error$Ground_Truth - dt_Error$Autotuned_Random_Forest))/sum(dt_Error$Ground_Truth))*100
wape_xg <- (sum(abs(dt_Error$Ground_Truth - dt_Error$XGBoost))/sum(dt_Error$Ground_Truth))*100
### END ###

### TRAIN ERROR ###
remVars <- "timestamp"
subpth <- "hh"
fq <- "60"
flr_model <- loadModel("flr", freq = fq, subpath = subpth)
blr_model <- loadModel("blr", freq = fq, subpath = subpth)
rf_model <- loadModel("rf", freq = fq, subpath = subpth)
arf_model <- loadModel("atrf", freq = fq, subpath = subpth)
xgboost_model <- loadModel("xgboost", freq = fq, subpath = subpth)

flr_preds_train <- predict(flr_model, newdata = lst$dt_train[, -c(remVars), with = F])
flr_preds_train[flr_preds_train < 0] <- 0

blr_preds_train <- predict(blr_model, newdata = lst$dt_train[, -c(remVars), with = F])
blr_preds_train[blr_preds_train < 0] <- 0

# the data has to be reload with one-hot encoding false

rf_preds_train <- predict(rf, data = lst$dt_train[, -c(remVars), with = F])$predictions
arf_preds_train <- predict(res$model, newdata = lst$dt_train)
arf_preds_train <- arf_predresarf_preds_train <- arf_preds_train$data$response

remVars <- c("timestamp", "P_TOT_SUM")
xtrain <- data.matrix(lst$dt_train[, -c(remVars), with = F])
train_label <- data.matrix(lst$dt_train[["P_TOT_SUM"]])
xgb_train = xgb.DMatrix(data = xtrain, label = train_label)
xgboost_preds_train <- predict(xgboost_model, xgb_train)
xgboost_preds_train[xgboost_preds_train<0] <- 0

actuals <- lst$dt_train$P_TOT_SUM
timestampVec_train <- lst$dt_train[["timestamp"]]
dt_Results_TRAIN <- data.table(timestamp = timestampVec_train, 
                               Ground_Truth = actuals, 
                               #Forward_Linear_Regression = unname(flr_preds_train),
                               #Backward_Linear_Regression = unname(blr_preds_train))
                               #Random_Forest = rf_preds_train,
                               #Autotuned_Random_Forest = arf_preds_train)
                               XGBoost = xgboost_preds_train)

fwrite(dt_Results_TRAIN, paste0("C:/Users/vgupta/Documents/Neha Thesis Ws/Results/results_70_21_date_wthr_holi_OhtF_SUM_TRAIN", "_", subpath, "_", freq, ".csv"))

### TRAIN ERROR END ###
dt_Error_TRAIN <- copy(dt_Results_TRAIN)
###WAPE for TRAIN#######
wape_flr <- (sum(abs(dt_Error_TRAIN$Ground_Truth - dt_Error_TRAIN$Forward_Linear_Regression))/sum(dt_Error_TRAIN$Ground_Truth))*100
wape_blr <- (sum(abs(dt_Error_TRAIN$Ground_Truth - dt_Error_TRAIN$Backward_Linear_Regression))/sum(dt_Error_TRAIN$Ground_Truth))*100
wape_rf <- (sum(abs(dt_Error_TRAIN$Ground_Truth - dt_Error_TRAIN$Random_Forest))/sum(dt_Error_TRAIN$Ground_Truth))*100
wape_arf <- (sum(abs(dt_Error_TRAIN$Ground_Truth - dt_Error_TRAIN$Autotuned_Random_Forest))/sum(dt_Error_TRAIN$Ground_Truth))*100
wape_xg <- (sum(abs(dt_Error_TRAIN$Ground_Truth - dt_Error_TRAIN$XGBoost))/sum(dt_Error_TRAIN$Ground_Truth))*100



melt_data <- melt(dt_Error, id = c("timestamp"))
p1 <- ggplot(melt_data, aes(x = variable, y = value)) +
  geom_boxplot(outlier.colour="red", fill = "skyblue", color = "blue") +
  #coord_trans(y = "sqrt") +
  labs(x = "Model", y = "RMSE") +
  scale_y_continuous(labels = comma)+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4) +
  ggtitle("Comparison of Regression Models HH (Time Res. 15 Minutes)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p1
ggsave(filename = paste0("C:/Users/vgupta/Documents/Neha Thesis Ws/Plot/boxplot_HH_15min",".png"), width = 16, height = 7, units = "cm", plot = p1)

#################################################################################
dtError <- copy(dt_Results)
dt_Error[, week:= week(timestamp)]
dt_Error[, rmse:= (actuals - predict)^2]
dt_Error[, rmse := sqrt(mean(rmse)), .(week)]
dt_res <- data.table(timestamp = timestampVec, actuals = actuals, preds = preds)
fwrite(dt_res, "C:/Users/vgupta/Documents/Neha Thesis Ws/EDForecast/res_glm_forward.csv")
min(timestampVec)
max(timestampVec)
dt_res[, week := week(timestamp)]
dt_res_agg <- dt_res[, c("week", "actuals", "preds")]
dt_res_agg <- dt_res_agg[, lapply(.SD,sum), by = c("week")]



#create holiday data
dt_holiday <- data.table(date=as.character(timestamp))
dt_holiday[date %in% c("2018-01-01", "2018-03-30", "2018-04-02", "2018-05-01", "2018-05-10", "2018-05-21", "2018-10-03", "2018-10-31", "2018-12-25", "2018-12-26"), holiday := 1]
dt_holiday[, lag1 := lead(holiday)]
dt_base_aggr[, date := as.character(date)]
dt_join <- merge(dt_base_aggr, dt_holiday, by="date")
#holidac_vec <- c("2018-01-01", "2018-03-30", "2018-04-02", "2018-05-01", "2018-05-10", "2018-05-21", "2018-10-03", "2018-10-31", "2018-12-25", "2018-12-26")
dt_join[holiday == 1, holiday := P_TOT_SUM]
dt_join[lag1 == 1, lag1 := P_TOT_SUM]
dt_join[, date := as.Date(date)]
dt_join <- dt_join[P_TOT_SUM > 0]


p <- ggplot(dt_join, aes(x = date, y = P_TOT_SUM)) +
  geom_line() +
  theme_classic() +
  geom_point(data=dt_join[!is.na(holiday), ], aes(x=date, y=P_TOT_SUM), colour="red", size=2)
p

