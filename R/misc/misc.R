# create holiday data
# data frame with timestamp
start_index_base <- as.POSIXct("2018-01-01 00:00:00", tz = "UTC")  # Starting date and time
end_index_base <- as.POSIXct("2020-12-31 23:45:00", tz = "UTC")    # Ending date and time
timestamp_base <- seq(from = start_index_base, to = end_index_base, by = "15 mins")
dt_base <- data.table(timestamp = timestamp_base)
dt_base[, date := date(timestamp)]

holiday_vec <- c("neujahr", "karfreitag", "ostermontag", "tag_der_arbeit", 
                 "christi_himmelfahrt", "pfingsmontag", "tag_der_deutschen_einheit",
                 "reformationstag", "weihnachten", "2_weihnachtstag")

for (holiday in holiday_vec) {
  dt_base[, (holiday) := 0]
  if (holiday == "neujahr") {
    dt_base[date %in% c("2018-01-01", "2019-01-01", "2020-01-01"), (holiday) := 1]
  } else if (holiday == "karfreitag") {
    dt_base[date %in% c("2018-03-30", "2019-04-19", "2020-04-10"), (holiday) := 1] 
  } else if (holiday == "ostermontag") {
    dt_base[date %in% c("2018-04-02", "2019-04-22", "2020-04-13"), (holiday) := 1]
  } else if (holiday == "tag_der_arbeit") {
    dt_base[date %in% c("2018-05-01", "2019-05-01", "2020-05-01"), (holiday) := 1]
  } else if (holiday == "christi_himmelfahrt") {
    dt_base[date %in% c("2018-05-10", "2019-05-30", "2020-05-21"), (holiday) := 1]
  } else if (holiday == "pfingsmontag") {
    dt_base[date %in% c("2018-05-21", "2019-06-10", "2020-06-01"), (holiday) := 1]
  } else if (holiday == "tag_der_deutschen_einheit") {
    dt_base[date %in% c("2018-10-03", "2019-10-03", "2020-10-03"), (holiday) := 1]
  } else if (holiday == "roformationstag") {
    dt_base[date %in% c("2018-10-31", "2019-10-31", "2020-10-31"), (holiday) := 1]
  } else if (holiday == "weihnachten") {
    dt_base[date %in% c("2018-12-25", "2019-12-25", "2020-12-25"), (holiday) := 1]
  } else if (holiday == "2_weihnachtstag") {
    dt_base[date %in% c("2018-12-26", "2019-12-26", "2020-12-26"), (holiday) := 1]
  }
}

#fwrite(dt_base, file = "C:/Users/vgupta/Documents/Neha Thesis Ws/Data/holiday_data_15.csv")

dt_base[, holiday_flag := rowSums(.SD), .SDcols = 2:NCOL(dt_base)]

# all holiday vs consumption plot

#data aggregate per day

holiday_vec <- c("neujahr", "karfreitag", "ostermontag", "tag_der_arbeit", 
                 "christi_himmelfahrt", "pfingsmontag", "tag_der_deutschen_einheit",
                 "reformationstag", "weihnachten", "2_weihnachtstag")
dt[, holiday := 0]
for (holiday in holiday_vec) {
  if (holiday == "neujahr") {
    dt[date %in% c("2018-01-01", "2019-01-01", "2020-01-01"), holiday := 1]
  } else if (holiday == "karfreitag") {
    dt[date %in% c("2018-03-30", "2019-04-19", "2020-04-10"), holiday := 1] 
  } else if (holiday == "ostermontag") {
    dt[date %in% c("2018-04-02", "2019-04-22", "2020-04-13"), holiday := 1]
  } else if (holiday == "tag_der_arbeit") {
    dt[date %in% c("2018-05-01", "2019-05-01", "2020-05-01"), holiday := 1]
  } else if (holiday == "christi_himmelfahrt") {
    dt[date %in% c("2018-05-10", "2019-05-30", "2020-05-21"), holiday := 1]
  } else if (holiday == "pfingsmontag") {
    dt[date %in% c("2018-05-21", "2019-06-10", "2020-06-01"), holiday := 1]
  } else if (holiday == "tag_der_deutschen_einheit") {
    dt[date %in% c("2018-10-03", "2019-10-03", "2020-10-03"), holiday := 1]
  } else if (holiday == "roformationstag") {
    dt[date %in% c("2018-10-31", "2019-10-31", "2020-10-31"), holiday := 1]
  } else if (holiday == "weihnachten") {
    dt[date %in% c("2018-12-25", "2019-12-25", "2020-12-25"), holiday := 1]
  } else if (holiday == "2_weihnachtstag") {
    dt[date %in% c("2018-12-26", "2019-12-26", "2020-12-26"), holiday := 1]
  }
}

dt[holiday == 1, holiday := P_TOT_SUM]
dt[, date := as.Date(date)]

p <- ggplot(dt, aes(x = date, y = P_TOT_SUM,)) +
  geom_line() +
  theme_classic() +
  geom_point(data=dt[!holiday==0, ], aes(x=date, y=P_TOT_SUM), colour="red", size=2)

p

dt <- getAnalysisData(start_year = 2020, end_year = 2020, aggr_by = "day", holiday = T)
dt[, holiday := 0]
dt[, holiday := as.integer(rowSums(.SD) > 0), .SDcols = 3:NCOL(dt)]
dt <- dt[ , holiday := as.double(holiday)]
dt[holiday == 1, holiday := P_TOT_SUM]


coeff <- 2000000
p <- ggplot(dt, aes(x=timestamp)) +
  xlab("Timestamp") +
  geom_line(aes(y=temperature), color="red") + 
  geom_line(aes(y=P_TOT_SUM), color ="blue") +
  scale_x_continuous(breaks = 122:105989) +
  
  scale_y_continuous(
    # Features of the first axis
    name = "Temperature",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="P_TOT (kWh)")
  ) +
  theme_classic() +
  labs(title = paste("Plot")) +
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.title.y.right = element_text(color = "blue"),
        axis.title.y.left = element_text(color = "red"),
        axis.text.y.right = element_text(color = "blue"),
        axis.text.y.left = element_text(color = "red")) 
p


#predict for 1 hour, 24*4, 

corr <- round(cor(dt), 1)
ggcorrplot(corr = dt)
corr <- round(P_TOT_SUM, temperature)
attach(dt)

#plot holiday data
dt[, holiday := 0]
dt[, holiday := as.integer(rowSums(.SD) > 0), .SDcols = 3:NCOL(dt)]
dt <- dt[ , holiday := as.double(holiday)]
dt[holiday == 1, holiday := P_TOT_SUM]
p <- ggplot(dt, aes(x = date, y = P_TOT_SUM,)) +
  geom_line() +
  xlab("Month")+
  ylab("P_TOT_SUM 2019 HP (in Wh)")+
  scale_y_continuous(labels = comma)+
  theme_classic() +
  geom_point(data=dt[!holiday==0, ], aes(x=date, y=P_TOT_SUM), colour="red", size=4)
p
ggsave(filename = paste0("C:/Users/vgupta/Documents/Neha Thesis Ws/New Plots/P_TOT_2019HP_Holiday",i,".svg"), height= 7, width=20, unit = "cm",plot = p)


#monthly consumption bar graph
dt_wday <- dt
dt_wday[, time := format(as.POSIXct(timestamp), format = "%H:%M")]
dt_wday[, date := as.Date(timestamp)]
dt_wday[, month := month(date)]
dt_wday[, week := week(timestamp)]
dt_wday <- dt_wday[, -c("timestamp", "date", "month", "time")]
dt_wday<- dt_wday[, lapply(.SD,stats::median), by = c("week")]
coeff <- 150
p <- ggplot(dt_wday[1:52], aes(x=week)) +
  xlab("calendar week 2019") +
  geom_line(aes(y=solar_irradiance), color="red")+
  geom_line(aes(y=P_TOT_SUM/coeff), color ="blue") +
  scale_x_continuous(breaks = 1:52) +
  scale_y_continuous(
    # Features of the first axis
    name = "Median of solar_irradiance",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Median of P_TOT(kWh)")
  ) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=90, hjust=0.5),
        axis.title.y.right = element_text(color = "blue"),
        axis.title.y.left = element_text(color = "red"),
        axis.text.y.right = element_text(color = "blue"),
        axis.text.y.left = element_text(color = "red"))
p
ggsave(filename = paste0("C:/Users/vgupta/Documents/Neha Thesis Ws/New Plots/HP_2019_si_NEW",".png"), height = 7, width = 20, unit = "cm", plot = p)


p <- ggplot(dt, aes(x=month, y=P_TOT_SUM)) +
  geom_bar(stat="identity") +      # Use geom_bar to create the bar chart
  theme_classic() +                # Apply classic theme
  scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = 1:12)+
  xlab("Month") +                  # Set x-axis label
  ylab("P_TOT_SUM_2020 HH (in Wh)")                # Set y-axis label
p
ggsave(filename = paste0("C:/Users/vgupta/Documents/Neha Thesis Ws/New Plots/HH_2020_MOnthly_NEW",i,".png"), height = 7, width = 20, unit = "cm", plot = p)

# median of consumption by hour and minute and plot
dt_wday <- dt
dt_wday[, date := as.Date(timestamp)]
dt_wday[, day_of_week := weekdays(date)]
dt <- dt[, -c("week_day", "month", "season", "time_hr", "time_min")]
dt <- dt[, lapply(.SD, mean), by = c("weekday")]
dt[, time_interval:= paste0(hour,":",minute)]
dt <- dt[order(time_interval)]
p <- ggplot(dt, aes(x=time_interval, y=P_TOT_SUM, group = 1)) +
  geom_line(color="blue") + 
  xlab("Timestamp (freq: 15 mins)") +
  ylab("P_tot (in watt)") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_x_discrete()
#ggsave(filename = paste0("C:/Users/vgupta/Documents/Neha Thesis Ws/Plot/P_TOT_timestamp2018-20",i,".png"), plot = p)
p

#summary
lm_model <- lm(Ground_Truth ~ Forward_Linear_Regression + Backward_Linear_Regression + Random_Forest + Autotuned_Random_Forest + XGBoost, data = dt_Results)
summary(lm_model)
#Adjusted R^2
1-((1-(0.8961571))*(18231-1)/(18231-61-1))

######wdays
dt_wday <- dt
dt_wday[, time := format(as.POSIXct(timestamp), format = "%H:%M")]
dt_wday[, date := as.Date(timestamp)]
dt_wday[, Weekdays := weekdays(date)]
dt_wday <- dt_wday[, -c("timestamp", "date")]
dt_wday<- dt_wday[, lapply(.SD,base::sum), by = c("Weekdays", "time")]
dt_wday[, Weekdays := factor(Weekdays, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))]
p <- ggplot(dt_wday, aes(x=time, y=P_TOT_SUM, color=Weekdays, group = Weekdays)) +
  geom_line() + 
  xlab("Time (freq. 60 min.)") +
  ylab("HeatPump Consumption (in Wh)") +
  scale_y_continuous(labels = function(x)format(x, scientific = FALSE)) +
  theme_classic() +
  theme(legend.position='top')+
  guides(color=guide_legend(nrow = 1))+
  theme(axis.text.x=element_text(angle=90, vjust=0.4, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, margin = margin(t = 16, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 16, b = 0, l = 0)),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))+
  
  scale_color_manual(values = c("Sunday" = "red", "Monday" = "green", "Tuesday" = "yellow", "Wednesday" = "pink", "Thursday" = "orange", "Friday" = "purple", "Saturday" = "brown"))+
  scale_x_discrete()
p
ggsave(filename = paste0("C:/Users/vgupta/Documents/Neha Thesis Ws/New Plots/HP_Allyear",".png"), height = 13, width = 27, unit = "cm", plot = p)
