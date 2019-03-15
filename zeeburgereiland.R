# curl -X GET 
# 'https://lkvis.rivm.nl/api/datasources/proxy/23/query?db=waag&q=SELECT%20%22PM10%22,%20%22PM25%22,%20%22Temp%22,%20%22Hum%22,%20%22Pres%22%20FROM%20%22autogen%22.%22vuurwerk%22%20WHERE%20time%20%3E=%201539280800000ms%20GROUP%20BY%20%22id%22&epoch=ms' \
# -H 'Postman-Token: c0bf2308-84dd-4976-a90e-a6c150d14cf7' \
# -H 'cache-control: no-cache'
# 
# curl -X GET 
# '"https://lkvis.rivm.nl/api/datasources/proxy/23/query?db=waag&q=SELECT PM10, PM25, Temp, Hum, Pres FROM autogen.vuurwerk WHERE time >= 1539367200 GROUP BY id &epoch=ms' \
# -H 'Postman-Token: c0bf2308-84dd-4976-a90e-a6c150d14cf7' \
# -H 'cache-control: no-cache'


#  https://api.luchtmeetnet.nl/station/measurement_data?station_id=190&start_date=2019%2F01%2F24&end_date=2019%2F01%2F31&daily_averages=0&monthly_averages=0


library(jsonlite)
library(curl)
library(data.table)
library(ggplot2)
library(Hmisc)
library(scales)
library(lubridate)
library(reshape2)

my_mean <-function(x){
  mean(x,na.rm=TRUE)
}

get_elapsed <- function(){
  elapsed <- (Sys.time()-start.time)
  units(elapsed) <- "secs"
  start.time <- Sys.time()
  return(elapsed)
}

putMsg <- function(msg,isEnd=TRUE,doStop=FALSE){
  
  if(!is.null(msg)){
    #browser()
    out <- paste(msg)
    my_stars <- paste(rep("*",nchar(out)+1),collapse="")
    writeLines("\n")
    writeLines(my_stars)
    writeLines(out)
    
    if (isEnd){
      writeLines(my_stars)
      #      writeLines("\n")
    }
  }
  if (doStop){
    readline(prompt="Press ENTER to continue")
  }
}

hourlyTimeSlots <- function(my_start,my_end){
  # Considering only full hours
  my_start <- ceiling_date(my_start,"hour")
  my_end <- floor_date(my_end,"hour")
  slots <- my_start
  pivot <- my_start
  while(pivot <= my_end){
    pivot <- pivot + hours(1)
    slots <- append(slots,pivot)
  }
 return(slots)
}

dailyTimeSlots <- function(my_start,my_end){
  # Considering only full days
  my_start <- ceiling_date(my_start,"day")
  my_end <- floor_date(my_end,"day")
  slots <- my_start
  pivot <- my_start
  while(pivot <= my_end){
    pivot <- pivot + days(1)
    slots <- append(slots,pivot)
  }
  return(slots)
}

humCorrectionPM25 <- function(rh){
  2.3 * (100 - rh)**(-0.38)
}

humCorrectionPM10 <- function(rh){
  2.3 * (100 - rh)**(-0.38)
}

# This function just works for this example, needs modifications if something changes
make_palette <- function(){

#  cbPalette <- c("#E69F00","#000000","#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#E69F00","#000000")
  cbPalette <- c("#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#E69F00", "#000000")
  measures <-sort(colnames(hourly_avrg)[grep("PM25|Hum",colnames(daily_avrg))])
  names(cbPalette) <- measures
  
  # ltPalette <- c("blank","solid","dashed","dotted","dotdash","longdash","twodash")
  ltPalette <- c("dashed","dashed","solid","solid","solid","solid","solid","solid","solid")
  names(ltPalette) <- measures
  return(list(cbPalette,ltPalette))
}

style_plot <- function(g,ttl,xlab,start_point){
  g <- g + 
    scale_colour_manual(values=cbPalette) +
    scale_fill_manual(values=cbPalette) +
    scale_linetype_manual(values=ltPalette) +
    theme(legend.title=element_blank()) +
    ggtitle(ttl) +
    theme(plot.title = element_text(size = 11, face = "bold", hjust=.5)) +
    xlab(xlab) + ylab(expression(paste(mu,"g","/",m^3))) +
    geom_hline(yintercept=threshold_standards[1],linetype="dashed", colour = "black",size=.2)
#   geom_text(aes(start_range[i],threshold_standards[1],label = names(threshold_standards)[1], vjust = -1), size = 3, family="mono", fontface = "italic")
#   annotate("text", x=start_range[i], y=threshold_standards[2], label=names(threshold_standards)[2],size=3,vjust=-1) +
#   annotate("text", x=start_range[i], y=threshold_standards[3], label=names(threshold_standards)[3],size=3,vjust=-1)
  if( start_point != 0 ){
    g <- g + annotate("text", x=start_point, y=threshold_standards[1], label=names(threshold_standards)[1],size=3,vjust=-1,hjust=0)
  }
  return(g)
  
}


# http://ec.europa.eu/environment/air/quality/standards.htm
threshold_standards <- c(25,40,50)
names(threshold_standards)<- c("PM2.5 yearly","PM10 yearly","PM10 daily")

putMsg("START!!", doStop=FALSE)
start.time <- Sys.time()

if(!exists("waag_hourly_avrg")){
  
  putMsg("Reading Waag sensors data", doStop=FALSE)
  
  #consider only full hours
  start <- ceiling_date(as.POSIXct("11/10/2018 20:00:00",format="%d/%m/%Y %H:%M:%S", tz="CET"),"hour")
  end <- floor_date(Sys.time(),"hour")
  
  server <- 'https://lkvis.rivm.nl/api/datasources/proxy/23/query?db=waag&q='
  query <- "SELECT PM10, PM25, Temp, Hum, Pres FROM autogen.vuurwerk WHERE time >= %is AND time <= %is AND %s GROUP BY id "
  suffix <- "&epoch=ms"
  sensor_ids <- c(2183229,697435)
  where <- paste("( id = '",paste(sensor_ids,collapse="' OR id = '"),"' )",sep="")
  # .POSIXct(start, tz="CET")
  
  query_enc <- URLencode(sprintf(query, as.integer(start),as.integer(end),where))
  
  request <- paste(server,query_enc,suffix,sep="")
  
  con <- curl(request)
  result <- readLines(con)
  close(con)
  
  my_msg <- paste("Reading Waag sensors data DONE, time elapsed:", get_elapsed(),"seconds")
  putMsg(my_msg, doStop=FALSE)
  putMsg("Processing Waag sensor data", doStop=FALSE)
  
  jresult <- fromJSON(result)
  
  index_1 <- which(jresult[1]$results[1]$series[[1]]$tags == sensor_ids[1])
  index_2 <- which(jresult[1]$results[1]$series[[1]]$tags == sensor_ids[2])
  
  col_1 <- jresult[1]$results[1]$series[[1]]$columns[index_1][[1]]
  col_2 <- jresult[1]$results[1]$series[[1]]$columns[index_2][[1]]
  
  val_1 <- data.table((jresult[1]$results[1]$series[[1]]$values[index_1])[[1]])
  val_2 <- data.table((jresult[1]$results[1]$series[[1]]$values[index_2])[[1]])
  
  colnames(val_1) <- col_1
  colnames(val_2) <- col_2
  
  val_1$time <- .POSIXct(val_1$time/1000)
  val_2$time <- .POSIXct(val_2$time/1000)
  
  setkey(val_1,time)
  setkey(val_2,time)
  
  # apply corrections for Humidity
  val_1$PM25 <- val_1$PM25/humCorrectionPM25(val_1$Hum)
  val_2$PM25 <- val_2$PM25/humCorrectionPM25(val_2$Hum)
  
  val_1$PM10 <- val_1$PM10/humCorrectionPM10(val_1$Hum)
  val_2$PM10 <- val_2$PM10/humCorrectionPM10(val_2$Hum)
  
  colnames(val_1)[-1] <- paste(colnames(val_1)[-1],sensor_ids[1],sep=".")
  colnames(val_2)[-1] <- paste(colnames(val_2)[-1],sensor_ids[2],sep=".")
  
  waag_val <- merge(val_1,val_2,all = TRUE)
  
  # we use this instead of cut(waag_val$time,"1 day",right=FALSE)
  # necessary to have POSIX.ct and not factors
  cuts <- list(time=floor_date(waag_val$time,unit="hour"))
  
  first <- TRUE
  for ( i in colnames(waag_val)[-1] ){
    running <- data.table(aggregate(waag_val[,get(i)],cuts,my_mean))
    colnames(running)[which(colnames(running) == "x")] <- i
    
    if (first){
      waag_hourly_avrg <- running
      first <- FALSE
    }else{
      waag_hourly_avrg <- merge(waag_hourly_avrg,running,by="time",all=TRUE)
    }
  }
  
  # we use this instead of cut(waag_val$time,"1 day",right=FALSE)
  # necessary to have POSIX.ct and not factors
  cuts <- list(time=floor_date(waag_val$time,unit="day"))
  
  
  first <- TRUE
  for ( i in colnames(waag_val)[-1] ){
    running <- data.table(aggregate(waag_val[,get(i)],cuts,my_mean))
    colnames(running)[which(colnames(running) == "x")] <- i
    
    if (first){
      waag_daily_avrg <- running
      first <- FALSE
    }else{
      waag_daily_avrg <- merge(waag_daily_avrg,running,by="time",all=TRUE)
    }
  }
  
  my_msg <- paste("Processing Waag sensors data DONE, time elapsed:", get_elapsed(),"seconds")
  putMsg(my_msg, doStop=FALSE)
  
}

if(!exists("ggd_hourly_avrg")){
  
  putMsg("Reading GGD stations HOURLY data", doStop=FALSE)

  server <- 'https://api.luchtmeetnet.nl/open_api'
  
  ## read the stations
  endpoint <-'/stations?page=1'
  
  request <- paste(server,endpoint,sep="")
  
  con <- curl(request)
  result <- readLines(con)
  close(con)
  
  jresult <- fromJSON(result)
  
  # select stations in Amsterdam
  ams_stations <- data.table(jresult$data[startsWith(jresult$data$location,"Amsterdam"),])
  
  # read measurements for the stations in Amsterdam
  endpoint <- '/measurements?'
  pollutants <- c("PM25","PM10")
  formulas <- paste("formula=",pollutants,sep="",collapse="&")
  query <- "start=%s&end=%s&station_number=%s&%s&page=&order_by=timestamp_measured&order_direction=desc"
  
  first <- TRUE
  
  for( i in 1:nrow(ams_stations)){
    
    station_id <- ams_stations$number[i]
    paging_end <- end
    
    station_first <- TRUE
    
    repeat{
      query_with_params <- sprintf(query, format(start,usetz = TRUE), format(paging_end,usetz = TRUE), station_id,formulas)
      
      query_enc <- URLencode(query_with_params)
      
      request <- paste(server,endpoint,query_enc,sep="")
      
      con <- curl(request)
      result <- readLines(con)
      close(con)
      
      #browser()
      jresult <- fromJSON(result)
      
      if ( length(jresult$data) == 0 ){
        if ( station_first ){
          my_msg <- paste("Station",ams_stations$location[i],"does not measure any of",paste(pollutants,collapse=","))
          putMsg(my_msg, doStop=FALSE)
        }
        break
      }else{
        if ( station_first ){
          my_msg <- paste("Station",ams_stations$location[i],"measures at least one of",paste(pollutants,collapse=","))
          putMsg(my_msg, doStop=FALSE)
        }
      }
        
      
      running <- data.table(reshape(jresult$data,v.names="value",idvar="timestamp_measured",timevar="formula",direction="wide",drop="station_number"))
      
      running$time <- with_tz(ymd_hms(running$timestamp_measured))
      
      running[,timestamp_measured:=NULL]
      
      setkey(running,"time")
      
      colnames(running) <- gsub("value",station_id,colnames(running))
      
      if (station_first){
        ggd_station_hourly_avrg <- running
        station_first <- FALSE
      }else{
        ggd_station_hourly_avrg <- rbind(ggd_station_hourly_avrg,running,use.names=TRUE,fill=TRUE)
      }
      
      paging_end <- min(ggd_station_hourly_avrg$time)
      
      if ( paging_end <= start ){
        break
      }else{
        #print(paste(ams_stations$location[i],paging_end))
      }
    }
    
    if ( station_first ){
      next
    }

    if (first){
      ggd_hourly_avrg <- ggd_station_hourly_avrg
      first <- FALSE
    }else{
      ggd_hourly_avrg <- merge(ggd_hourly_avrg,ggd_station_hourly_avrg,all=TRUE)
    }
  }
  
  my_msg <- paste("Reading GGD station HOURLY data DONE, time elapsed:", get_elapsed(),"seconds")
  putMsg(my_msg, doStop=FALSE)
  putMsg("Processing GGD station data", doStop=FALSE)
  
  # we use this instead of cuts <- list(time=cut(ggd_hourly_avrg$time,"1 day",right=FALSE))
  # necessary to have POSIX.ct and not factors
  cuts <- list(time=floor_date(ggd_hourly_avrg$time,unit="day"))
  
  first <- TRUE
  for ( i in colnames(ggd_hourly_avrg)[-1] ){
    running <- data.table(aggregate(ggd_hourly_avrg[,get(i)],cuts,my_mean))
    colnames(running)[which(colnames(running) == "x")] <- i
    
    if (first){
      ggd_daily_avrg <- running
      first <- FALSE
    }else{
      ggd_daily_avrg <- merge(ggd_daily_avrg,running,by="time",all=TRUE)
    }
  }
  
  my_msg <- paste("Processing GGD station data DONE, time elapsed:", get_elapsed(),"seconds")
  putMsg(my_msg, doStop=FALSE)
  
}

# Merging data structures
hourly_avrg <-merge(waag_hourly_avrg,ggd_hourly_avrg,by.x="time",by.y="time",all=TRUE)
colnames(hourly_avrg) <- gsub("value_PM","PM",colnames(hourly_avrg))

daily_avrg <-merge(waag_daily_avrg,ggd_daily_avrg,by.x="time",by.y="time",all=TRUE)
colnames(daily_avrg) <- gsub("value_PM","PM",colnames(daily_avrg))

putMsg("Reading and processing Data DONE!!", doStop=FALSE)
putMsg("Calculating correlations", doStop=FALSE)
# just to reinitialise the time
get_elapsed()

## Hourly Correlation PM25
putMsg("Hourly Correlation PM25", doStop=FALSE)

print(rcorr(as.matrix(hourly_avrg[,grep("PM25",colnames(hourly_avrg)),with=FALSE]),type="pearson"))

## Hourly Correlation PM10
putMsg("Hourly Correlation PM10", doStop=FALSE)

print(rcorr(as.matrix(hourly_avrg[,grep("PM10",colnames(hourly_avrg)),with=FALSE]),type="pearson"))

## Hourly Correlation Waag sensors
putMsg("Hourly Correlation Waag sensors", doStop=FALSE)

print(rcorr(as.matrix(hourly_avrg[,grep("^PM",colnames(hourly_avrg)),with=FALSE]),type="pearson"))

print(rcorr(as.matrix(hourly_avrg[,grep("PM|time",colnames(hourly_avrg),invert=TRUE),with=FALSE]),type="pearson"))

##Daily Correlation PM25
putMsg("Daily Correlation PM25", doStop=FALSE)

print(rcorr(as.matrix(daily_avrg[,grep("PM25",colnames(daily_avrg)),with=FALSE]),type="pearson"))

##Daily Correlation PM10
putMsg("Daily Correlation PM10", doStop=FALSE)

print(rcorr(as.matrix(daily_avrg[,grep("PM10",colnames(daily_avrg)),with=FALSE]),type="pearson"))

##Daily Correlation Waag sensors
putMsg("Daily Correlation Waag sensors", doStop=FALSE)

print(rcorr(as.matrix(daily_avrg[,grep("^PM",colnames(daily_avrg)),with=FALSE]),type="pearson"))

print(rcorr(as.matrix(daily_avrg[,grep("PM|time",colnames(daily_avrg),invert=TRUE),with=FALSE]),type="pearson"))

my_msg <- paste("Correlation DONE, time elapsed:", get_elapsed(),"seconds")
putMsg(my_msg, doStop=FALSE)

#############################
# Plots
#############################
# Sets color and line type palettes
palettes <- make_palette()
cbPalette <- palettes[[1]]
ltPalette <- palettes[[2]]

waag_daily_avrg_mlt <- melt(waag_daily_avrg,id.vars="time",measure.vars=colnames(waag_daily_avrg)[grep("PM25|Hum",colnames(waag_daily_avrg))])

ttl <- paste(paste(sensor_ids,collapse = ","),"daily average", paste(date(start),date(end),sep=" / "),sep=" - ")

g <- ggplot(data=waag_daily_avrg_mlt,aes(x=time)) + 
        geom_line(aes(y = value, colour = variable, linetype = variable)) +
        scale_x_datetime(date_breaks = "7 day", labels = date_format("%d-%m")) +
#        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +
        theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

g <- style_plot(g,ttl,"time",start)

print(g)
putMsg(ttl, doStop=TRUE)

labels_months <- c("November","December","January","February")
start_months <- c("01/11/2018 00:00:00","01/12/2018 00:00:00","01/01/2019 00:00:00","01/02/2019 00:00:00")
end_months <- c("30/11/2018 23:59:59","31/12/2018 23:59:59","31/01/2019 23:59:59","28/02/2019 23:59:59")

start_range <- as.POSIXct(start_months,format="%d/%m/%Y %H:%M:%S", tz="CET")
end_range <- as.POSIXct(end_months,format="%d/%m/%Y %H:%M:%S", tz="CET")

daily_avrg_mlt <- melt(daily_avrg,id.vars="time",measure.vars=colnames(daily_avrg)[grep("PM25|Hum",colnames(daily_avrg))])


for( i in 1:length(labels_months)){
    
    ttl <- paste(paste(sensor_ids,collapse=","),labels_months[i],"daily average",sep=" - ")
    
    g <- ggplot(data=daily_avrg_mlt[time>= start_range[i]& time <=end_range[i],],aes(x=time)) +
      geom_line(aes(y = value, colour = variable,linetype=variable)) +
      scale_x_datetime(date_breaks = "1 day", labels = date_format("%d-%m")) +
      theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))
    
    g <- style_plot(g,ttl,"time",start_range[i])
    
    print(g)
    putMsg(ttl, doStop=TRUE)
  
}

# Plot week days averages

cuts <- list(time=as.factor(weekdays(daily_avrg$time)))

first <- TRUE
for ( i in colnames(daily_avrg)[grep("PM25",colnames(daily_avrg))] ){
  running <- data.table(aggregate(daily_avrg[,get(i)],cuts,my_mean))
  colnames(running)[which(colnames(running) == "x")] <- i
  
  if (first){
    weekdays_averages <- running
    first <- FALSE
  }else{
    weekdays_averages <- merge(weekdays_averages,running,by="time",all=TRUE)
  }
}

weekdays_averages <- melt(weekdays_averages,"time")

ttl <- paste(paste(sensor_ids,collapse=","),"week days average",paste(date(start),date(end),sep=" / "),sep=" - ")

g <- ggplot(data=weekdays_averages,aes(x=time,y=value,fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) 

g <- style_plot(g,ttl,"time",1)

print(g)
putMsg(ttl, doStop=TRUE)


# Plot hour of the day averages

cuts <- list(time=as.factor(paste(weekdays(hourly_avrg$time),hour(hourly_avrg$time),sep="-")))

first <- TRUE
for ( i in colnames(hourly_avrg)[grep("PM25",colnames(hourly_avrg))] ){
  running <- data.table(aggregate(hourly_avrg[,get(i)],cuts,my_mean))
  colnames(running)[which(colnames(running) == "x")] <- i
  
  if (first){
    days_averages <- running
    first <- FALSE
  }else{
    days_averages <- merge(days_averages,running,by="time",all=TRUE)
  }
}

newCols <- colsplit(days_averages$time, "-", c("day","hour"))
newCols$day <- factor(newCols$day,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),ordered=TRUE)

days_averages[,time:=NULL]
days_averages <- cbind(days_averages, newCols)

setkey(days_averages,day,hour)
days_averages <- melt(days_averages,c("day","hour"))
#levels(days_averages$day) <- factor(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),ordered=TRUE)


ttl <- paste(paste(sensor_ids,collapse=","),"hour of the day average",paste(date(start),date(end),sep=" / "),sep=" - ")

# a<- days_averages[variable %in% colnames(daily_avrg)[grep("^PM25",colnames(daily_avrg))]]
# a<- days_averages[variable %in% c("PM25.697435","PM25.2183229")]
a<- days_averages[variable %in% c("PM25.697435","PM25.2183229")]

g <- ggplot(data=days_averages,aes(x = hour, y = value,colour = variable,linetype=variable)) +
  geom_line() +
  scale_x_continuous(breaks=seq(0,23,4),minor_breaks=0:23) +
  facet_wrap( ~ day, nrow = 1)


g <- style_plot(g,ttl,"time",0)

print(g)
putMsg(ttl, doStop=TRUE)


# waag_val$time[!is.na(waag_val$PM10.2183229) & waag_val$PM10.2183229 > 80]
# waag_val$time[!is.na(waag_val$PM10.697435) & waag_val$PM10.697435 > 80]
