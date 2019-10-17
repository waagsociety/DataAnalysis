server <- 'https://api.luchtmeetnet.nl/station/measurement_data?'
query <- "station_id=%i&start_date=%s&end_date=%s&daily_averages=%i&monthly_averages=%i"
station_ids <- c(187,190,189,188)
names(station_ids) <- c("Amsterdam_Van Diemenstraat", "Amsterdam_Stadhouderskade", "Amsterdam_Westerpark", "Amsterdam-Vondelpark")

hourly_avrg <- TRUE
daily_avrg <- !hourly_avrg
monthly_avrg <- !(hourly_avrg || daily_avrg)
first <- TRUE

for( i in 1:length(station_ids)){
  
  station_id <- station_ids[i]
  
  query_enc <- URLencode(sprintf(query, station_id,format(start,format="%Y/%m/%d"),format(end,format="%Y/%m/%d"),daily_avrg,monthly_avrg))
  
  request <- paste(server,query_enc,sep="")
  
  con <- curl(request)
  result <- readLines(con)
  close(con)
  
  jresult <- fromJSON(result)
  
  pm10 <- jresult$PM10$measurements
  browser()
  
  names(pm10)[1] <- "time"
  pm10$time <- as.POSIXct(pm10$time,format="%Y-%m-%d %H:%M",tz="CET")
  
  pm10 <- data.table(pm10,key="time")
  
  names(pm10)[names(pm10) != "time"] <- paste(names(pm10)[names(pm10) != "time"],
                                              "PM10",station_id,sep ="_")
  
  pm25 <- jresult$PM25$measurements
  
  names(pm25)[1] <- "time"
  pm25$time <- as.POSIXct(pm25$time,format="%Y-%m-%d %H:%M",tz="CET")
  
  pm25 <- data.table(pm25,key="time")
  
  names(pm25)[names(pm25) != "time"] <- paste(names(pm25)[names(pm25) != "time"],
                                              "PM25",station_id,sep ="_")
  
  if (first){
    ggd_hourly_avrg <- merge(pm10,pm25,all=TRUE)
    first <- FALSE
  }else{
    ggd_hourly_avrg <- merge(ggd_hourly_avrg,merge(pm10,pm25,all=TRUE),all=TRUE)
  }
}



slots <- hourlyTimeSlots(min(waag_val[,time]),max(waag_val[,time]))

len <- length(slots)

# allocate table
waag_hourly_avrg <- data.table(time=rep(start,len-1),
                               PM10.2183229=rep(0,len-1),
                               PM25.2183229=rep(0,len-1),
                               Temp.2183229=rep(0,len-1),
                               Hum.2183229=rep(0,len-1),
                               Pres.2183229=rep(0,len-1),
                               PM10.697435=rep(0,len-1),
                               PM25.697435=rep(0,len-1),
                               Temp.697435=rep(0,len-1),
                               Hum.697435=rep(0,len-1),
                               Pres.697435=rep(0,len-1)
)


# assign averages to intervals
for (i in 1:(len-1)) {
  waag_hourly_avrg$time[i] <- slots[i]
  
  waag_hourly_avrg$PM10.2183229[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(PM10.2183229), mean(PM10.2183229),]
  waag_hourly_avrg$PM25.2183229[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(PM25.2183229), mean(PM25.2183229),]
  waag_hourly_avrg$Temp.2183229[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(Temp.2183229), mean(Temp.2183229),]
  waag_hourly_avrg$Hum.2183229[i]  <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(Hum.2183229), mean(Hum.2183229),]
  waag_hourly_avrg$Pres.2183229[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(Pres.2183229), mean(Pres.2183229),]
  
  waag_hourly_avrg$PM10.697435[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(PM10.697435), mean(PM10.697435),]
  waag_hourly_avrg$PM25.697435[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(PM25.697435), mean(PM25.697435),]
  waag_hourly_avrg$Temp.697435[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(Temp.697435), mean(Temp.697435),]
  waag_hourly_avrg$Hum.697435[i]  <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(Hum.697435), mean(Hum.697435),]
  waag_hourly_avrg$Pres.697435[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(Pres.697435), mean(Pres.697435),]
  
}



slots <- dailyTimeSlots(min(waag_val[,time]),max(waag_val[,time]))

len <- length(slots)

# allocate table
waag_daily_avrg <- data.table(time=rep(start,len-1),
                              PM10.2183229=rep(0,len-1),
                              PM25.2183229=rep(0,len-1),
                              Temp.2183229=rep(0,len-1),
                              Hum.2183229=rep(0,len-1),
                              Pres.2183229=rep(0,len-1),
                              PM10.697435=rep(0,len-1),
                              PM25.697435=rep(0,len-1),
                              Temp.697435=rep(0,len-1),
                              Hum.697435=rep(0,len-1),
                              Pres.697435=rep(0,len-1)
)


# assign averages to intervals
for (i in 1:(len-1)) {
  waag_daily_avrg$time[i] <- slots[i]
  
  waag_daily_avrg$PM10.2183229[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(PM10.2183229), mean(PM10.2183229),]
  waag_daily_avrg$PM25.2183229[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(PM25.2183229), mean(PM25.2183229),]
  waag_daily_avrg$Temp.2183229[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(Temp.2183229), mean(Temp.2183229),]
  waag_daily_avrg$Hum.2183229[i]  <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(Hum.2183229), mean(Hum.2183229),]
  waag_daily_avrg$Pres.2183229[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(Pres.2183229), mean(Pres.2183229),]
  
  waag_daily_avrg$PM10.697435[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(PM10.697435), mean(PM10.697435),]
  waag_daily_avrg$PM25.697435[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(PM25.697435), mean(PM25.697435),]
  waag_daily_avrg$Temp.697435[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(Temp.697435), mean(Temp.697435),]
  waag_daily_avrg$Hum.697435[i]  <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(Hum.697435), mean(Hum.697435),]
  waag_daily_avrg$Pres.697435[i] <- waag_val[time >= slots[i] & time < slots[i+1] & !is.na(Pres.697435), mean(Pres.697435),]
  
}


slots <- dailyTimeSlots(min(ggd_hourly_avrg[,time]),max(ggd_hourly_avrg[,time]))

len <- length(slots)

# allocate table
ggd_daily_avrg <- data.table(time=rep(start,len-1),
                             value_PM10_187=rep(0,len-1),
                             value_PM25_187=rep(0,len-1),
                             value_PM10_190=rep(0,len-1),
                             value_PM25_190=rep(0,len-1),
                             value_PM10_189=rep(0,len-1),
                             value_PM25_189=rep(0,len-1),
                             value_PM10_188=rep(0,len-1),
                             value_PM25_188=rep(0,len-1)
)


# assign averages to intervals
for (i in 1:(len-1)) {
  ggd_daily_avrg$time[i] <- slots[i]
  
  ggd_daily_avrg$value_PM10_187[i] <- ggd_hourly_avrg[time >= slots[i] & time < slots[i+1] & !is.na(value_PM10_187), mean(value_PM10_187),]
  ggd_daily_avrg$value_PM25_187[i] <- ggd_hourly_avrg[time >= slots[i] & time < slots[i+1] & !is.na(value_PM25_187), mean(value_PM25_187),]
  
  ggd_daily_avrg$value_PM10_190[i] <- ggd_hourly_avrg[time >= slots[i] & time < slots[i+1] & !is.na(value_PM10_190), mean(value_PM10_190),]
  ggd_daily_avrg$value_PM25_190[i]  <- ggd_hourly_avrg[time >= slots[i] & time < slots[i+1] & !is.na(value_PM25_190), mean(value_PM25_190),]
  
  ggd_daily_avrg$value_PM10_189[i] <- ggd_hourly_avrg[time >= slots[i] & time < slots[i+1] & !is.na(value_PM10_189), mean(value_PM10_189),]
  ggd_daily_avrg$value_PM25_189[i] <- ggd_hourly_avrg[time >= slots[i] & time < slots[i+1] & !is.na(value_PM25_189), mean(value_PM25_189),]
  
  ggd_daily_avrg$value_PM10_188[i] <- ggd_hourly_avrg[time >= slots[i] & time < slots[i+1] & !is.na(value_PM10_188), mean(value_PM10_188),]
  ggd_daily_avrg$value_PM25_188[i] <- ggd_hourly_avrg[time >= slots[i] & time < slots[i+1] & !is.na(value_PM25_188), mean(value_PM25_188),]
  
}


# if(!exists("ggd_daily_avrg")){
#   
#   putMsg("Reading GGD stations DAILY data", doStop=FALSE)
#   start.time <- Sys.time()
#   
#   server <- 'https://api.luchtmeetnet.nl/station/measurement_data?'
#   query <- "station_id=%i&start_date=%s&end_date=%s&daily_averages=%i&monthly_averages=%i"
# 
#   hourly_avrg <- FALSE
#   daily_avrg <- !hourly_avrg
#   monthly_avrg <- !(hourly_avrg || daily_avrg)
#   first <- TRUE
#   
#   for( i in 1:length(station_ids)){
#     
#     station_id <- station_ids[i]
#     
#     query_enc <- URLencode(sprintf(query, station_id,format(start,format="%Y/%m/%d"),format(end,format="%Y/%m/%d"),daily_avrg,monthly_avrg))
#     
#     request <- paste(server,query_enc,sep="")
#     
#     con <- curl(request)
#     result <- readLines(con)
#     close(con)
#     
#     jresult <- fromJSON(result)
#     
#     pm10 <- jresult$PM10$measurements
#     
#     pm10$timestamp_measured <- as.POSIXct(pm10$timestamp_measured,format="%Y-%m-%d %H:%M",tz="CET")
#     
#     pm10 <- data.table(pm10,key="timestamp_measured")
#     
#     names(pm10)[names(pm10) != "timestamp_measured"] <- paste(names(pm10)[names(pm10) != "timestamp_measured"],
#                                                               "PM10",station_id,sep ="_")
#     
#     pm25 <- jresult$PM25$measurements
#     
#     pm25$timestamp_measured <- as.POSIXct(pm25$timestamp_measured,format="%Y-%m-%d %H:%M",tz="CET")
#     
#     pm25 <- data.table(pm25,key="timestamp_measured")
#     
#     names(pm25)[names(pm25) != "timestamp_measured"] <- paste(names(pm25)[names(pm25) != "timestamp_measured"],
#                                                               "PM25",station_id,sep ="_")
#     
#     if (first){
#       ggd_daily_avrg <- merge(pm10,pm25,all=TRUE)
#       first <- FALSE
#     }else{
#       ggd_daily_avrg <- merge(ggd_daily_avrg,merge(pm10,pm25,all=TRUE),all=TRUE)
#     }
#   }
#   
# }

# corr_cols <- c("PM10.2183229","PM25.2183229","Temp.2183229","Hum.2183229","Pres.2183229","PM10.697435","PM25.697435","Temp.697435","Hum.697435","Pres.697435","Temp.697435",
#                "PM10_187", "PM25_187", "PM10_190", "PM25_190", "PM10_189", "PM25_189",
#                "PM10_188", "PM25_188")

hourly_avrg$day <- as.factor(weekdays(hourly_avrg$time))
levels(hourly_avrg$day) <- as.factor(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
hourly_avrg$hour <- hour(hourly_avrg$time)

days_averages <- data.table(day=as.factor(rep(1:7,24)),hour=rep(-1,24*7),PM25.2183229=0.0,PM25.697435=0.0,NL49016.PM25=0.0,NL49012.PM25=0.0,key=c("day","hour"))

levels(days_averages$day) <- as.factor(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))


for( i in 1:length(levels(hourly_avrg$day))){
  for( j in unique(hour(hourly_avrg$time))){
    # offset for index
    ii <- (i-1) * 24
    # correct for hour starting at 0
    jj <- j+1
    
    idx <- ii + jj
    
    days_averages$day[idx] <- levels(days_averages$day)[i]
    days_averages$hour[idx] <- j
    days_averages$PM25.2183229[idx] <- hourly_avrg[day == levels(days_averages$day)[i] & hour(time) == j & !is.na(PM25.2183229),mean(PM25.2183229),]
    days_averages$PM25.697435[idx] <- hourly_avrg[day == levels(days_averages$day)[i] & hour(time) == j & !is.na(PM25.697435),mean(PM25.697435),]
    days_averages$NL49016.PM25[idx] <- hourly_avrg[day == levels(days_averages$day)[i] & hour(time) == j &!is.na(NL49016.PM25),mean(NL49016.PM25),]
    days_averages$NL49012.PM25[idx] <- hourly_avrg[day == levels(days_averages$day)[i] & hour(time) == j & !is.na(NL49012.PM25),mean(NL49012.PM25),]
  }
}

setkey(days_averages,day,hour)

#days_averages <- melt(days_averages,id.vars=c("day","hour"))

browser()


