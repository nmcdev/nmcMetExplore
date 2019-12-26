#' Plot day maximum temperature observations over China.
#'
#' @param date, date string, like "20160801"
#' @param threshold, temperature larger or equal than threshold will be displayed
#' @param limit, region limit, (minLat,minLon,maxLat,maxLon)
#' @param table, display topest n record or not, default = TRUE.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @return ggplot2 plot
#' @export
#'
#' @examples
#'    obs_day_maxTem("20160811")
#'
obs_day_maxTem <- function(date, threshold=-100,
                           limit=c(18,72,54,136),
                           table=TRUE){
  # convert time string
  time <- paste(date, "000000", sep="")
  timestr <- strftime(as.POSIXct(time,format="%Y%m%d%H%M%S"), format="%Y-%m-%d")
  
  # read maximum temperature
  obsData <- nmcMetIO::cimiss_obs_in_rect_by_time(
    time,limit[1],limit[2],limit[3],limit[4],
    dataCode="SURF_CHN_MUL_DAY",
    elements="Station_Id_C,Station_Name,Datetime,Lat,Lon,TEM_Max")
  names(obsData) <- c("ID","Name","Datetime","Lat","Lon","MaxTemp")
  
  # remove missing value
  obsData <- filter(obsData, MaxTemp != "999999" & MaxTemp != "",
                    MaxTemp >= threshold)
  
  # convert data type
  obsData$Lat <- as.numeric(obsData$Lat)
  obsData$Lon <- as.numeric(obsData$Lon)
  obsData$MaxTemp <- as.numeric(obsData$MaxTemp)
  
  # get topest temperature
  obsDataTop <- obsData %>% top_n(6, MaxTemp) %>% arrange(desc(MaxTemp)) %>%
    select(Name, Lon, Lat, MaxTemp)
  
  # read china map data
  cmap <- rgdal::readOGR(system.file("extdata",package="nmcMetResources"), "bou2_4p")
  cmap <- fortify(cmap)
  
  # draw map
  colors <- c("#3D0239","#FA00FC","#090079","#5E9DF8","#2E5E7F",
              "#06F9FB","#0BF40B","#006103","#FAFB07","#D50404","#5A0303")
  g <- ggplot(data=obsData, aes(x=Lon, y=Lat, color=MaxTemp)) +
    geom_point(size=1, alpha=0.4) +
    scale_colour_gradientn(name="Max\nTem", colours=colors, limits=c(0, 40)) +
    geom_polygon(data=cmap, aes(x=long, y=lat, group=group), fill=NA, colour="black") +
    ylim(limit[1],limit[3]) + xlim(limit[2],limit[4]) +
    labs(title=paste(timestr, "Maximum Temperature Obaservation"),
         x="Longitude", y="Latitude") +
    theme(text=element_text(size=14),
          title=element_text(hjust=0.5,vjust=1,face=c("bold")))
  if (table) {
    g <- g +
      annotation_custom(gridExtra::tableGrob(obsDataTop),xmin=limit[2],ymin=limit[1],
                        xmax=limit[2]+(limit[4]-limit[2])/5, ymax=limit[1]+(limit[3]-limit[1])/5) +
      geom_text(data=obsDataTop,mapping=aes(x=Lon,y=Lat,label=rownames(obsDataTop)),
                colour="black", fontface=c("bold"), size=5)
  }
  
  # display image
  dev.new(width=14, height=10, noRStudioGD=TRUE)
  g
}


#' Plot day accumulated precipitation observations over China.
#'
#' @param date, date string, like "20160801"
#' @param hour, observation hour, 8 or 20.
#' @param threshold, 24h precipitation larger or equal than threshold will be displayed
#' @param limit, region limit, (minLat,minLon,maxLat,maxLon)
#' @param table, display topest n record or not, default = TRUE.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @return ggplot2 plot
#' @export
#'
#' @examples
#'     obs_day_rain("20160814")
#'
obs_day_rain <- function(date, hour=8, threshold=0.1,
                         limit=c(18,72,54,136),
                         table=TRUE){
  # convert time string
  time <- paste(date, "000000", sep="")
  timect <- as.POSIXct(time,format="%Y%m%d%H%M%S")
  
  # check hour
  if (hour == 8) {
    elements <- "Station_Id_C,Station_Name,Datetime,Lat,Lon,PRE_Time_0808"
    timestr <- paste(strftime(timect+28800, format="%Y/%m/%dT%H"),
                     strftime(timect+115200, format="%Y/%m/%dT%H"),sep="-")
  } else {
    elements <- "Station_Id_C,Station_Name,Datetime,Lat,Lon,PRE_Time_2020"
    timestr <- paste(strftime(timect-14400, format="%Y/%m/%dT%H"),
                     strftime(timect+72000, format="%Y/%m/%dT%H"),sep="-")
  }
  
  # read 24h accumulation precipitation
  obsData <- nmcMetIO::cimiss_obs_in_rect_by_time(
    time,limit[1],limit[2],limit[3],limit[4],
    dataCode="SURF_CHN_MUL_DAY",
    elements=elements)
  names(obsData) <- c("ID","Name","Datetime","Lat","Lon","Rain")
  
  # remove missing value
  obsData <- obsData %>%
    filter(Rain != "999999", Rain != "", Rain != "999990", Rain >= threshold) %>%
    arrange(Rain)
  
  # convert data type
  obsData$Lat <- as.numeric(obsData$Lat)
  obsData$Lon <- as.numeric(obsData$Lon)
  obsData$Rain <- as.numeric(obsData$Rain)
  
  # get topest rain
  obsDataTop <- obsData %>% top_n(6, Rain) %>% arrange(desc(Rain)) %>%
    select(Name, Lon, Lat, Rain)
  
  # read china map data
  cmap <- rgdal::readOGR(system.file("extdata",package="nmcMetResources"), "bou2_4p")
  cmap <- fortify(cmap)
  
  # draw map
  breaks <- c(0.1, 10, 25, 50, 100, 250, Inf)
  values <- c("[0.1,10)"="lightgreen","[10,25)"="green","[25,50)"="lightskyblue",
              "[50,100)"="blue","[100,250)"="magenta","[250,Inf)"="maroon")
  labels <- c("0.1~10","10~25","25~50","50~100","100~250","250~")
  g <- ggplot(data=obsData, aes(x=Lon, y=Lat)) +
    geom_point(aes(colour=cut(Rain,breaks,right=FALSE)), size=1, alpha=0.5) +
    scale_color_manual(name="Rain", values=values, labels=labels) +
    geom_polygon(data=cmap, aes(x=long, y=lat, group=group), fill=NA, colour="black") +
    ylim(limit[1],limit[3]) + xlim(limit[2],limit[4]) +
    labs(title=paste(timestr, "24h Accumulated Precipitation"),
         x="Longitude", y="Latitude") +
    theme(text=element_text(size=14),
          title=element_text(hjust=0.5,vjust=1,face=c("bold")))
  if (table) {
    g <- g +
      annotation_custom(gridExtra::tableGrob(obsDataTop),xmin=limit[2],ymin=limit[1],
                        xmax=limit[2]+(limit[4]-limit[2])/5, ymax=limit[1]+(limit[3]-limit[1])/5)
  }
  
  # display image
  dev.new(width=14, height=10, noRStudioGD=TRUE)
  g
}


#' Plot station one hour temperature series.
#'
#' @description
#'     Plot station one hour temperature observation time series.
#'
#' @param staIds : station ids, like "54511,54416,54406"
#' @param startTime : start and end time string, like "2016071900"
#' @param endTime : end time string, like "2016072100"
#'
#' @import ggplot2
#'
#' @return ggplot plot.
#'
#' @export
#' @examples
#'     obs_temp1h_series("54511,54416,54406") # display latest 72 hours rainfall.
#'     obs_temp1h_series("54511,54416,54406", startTime="2016071900",endTime="2016072100")
#'
obs_temp1h_series <- function(staIds, startTime=NULL, endTime=NULL){
  
  # get default time
  if (is.null(endTime)) endTime <- strftime(trunc(Sys.time(),"hour"), format="%Y%m%d%H")
  if (is.null(startTime)) startTime <- strftime(trunc(Sys.time(),"hour")-72*3600, format="%Y%m%d%H")
  
  # generate time series
  times = seq(strptime(startTime,"%Y%m%d%H"), strptime(endTime,"%Y%m%d%H"),
              by="hour", tz="UTC")
  times = format(times, "%Y%m%d%H%M%S")
  times = paste(times, collapse=",")
  
  # retrieve rain observation from CIMISS
  data <- nmcMetIO::cimiss_obs_by_time_and_staID(
    times, elements="Station_Id_C,Station_Name,Datetime,TEM",
    staIds=staIds)
  data$TEM = as.numeric(data$TEM)
  data[data == 999999.0] <- NA
  data$Datetime = as.POSIXct(data$Datetime, format='%Y%m%d%H%M%S', tz="GMT")
  
  # plot rainfall series
  g <- ggplot(data=data, aes(Datetime, TEM, group=Station_Name, colour=Station_Name))+
    geom_line(size=1) + geom_point(size=3, fill="white") +
    labs(x="Time", y=expression('One hour temperature ('*~degree*C*')')) +
    ggtitle("Station one-hour temperature observation") +
    scale_x_datetime(labels=scales::date_format("%dT%H"),
                     breaks=scales::date_breaks("6 hour"),
                     minor_breaks=scales::date_breaks("1 hour")) +
    ggthemes::theme_igray(base_size=16) +
    ggthemes::scale_colour_tableau(name="Station")
  
  # draw plot
  dev.new(width=12, height=6, noRStudioGD=TRUE)
  g
}


#' Plot station one hour rainfall time series.
#'
#' @description
#'     Plot station one hour rainfall observation time series.
#'
#' @param staIds : station ids, like "54511,54416,54406"
#' @param startTime : start and end time string, like "2016071900"
#' @param endTime : end time string, like "2016072100"
#'
#' @import ggplot2
#'
#' @return ggplot plot.
#'
#' @export
#' @examples
#'     obs_rain1h_series("54511,54416,54406") # display latest 72 hours rainfall.
#'     obs_rain1h_series("54511,54416,54406", startTime="2016071900",endTime="2016072100")
#'
obs_rain1h_series <- function(staIds, startTime=NULL, endTime=NULL){
  
  # get default time
  if (is.null(endTime)) endTime <- strftime(trunc(Sys.time(),"hour"), format="%Y%m%d%H")
  if (is.null(startTime)) startTime <- strftime(trunc(Sys.time(),"hour")-72*3600, format="%Y%m%d%H")
  
  # generate time series
  times = seq(strptime(startTime,"%Y%m%d%H"), strptime(endTime,"%Y%m%d%H"),
              by="hour", tz="UTC")
  times = format(times, "%Y%m%d%H%M%S")
  times = paste(times, collapse=",")
  
  # retrieve rain observation from CIMISS
  data <- nmcMetIO::cimiss_obs_by_time_and_staID(
    times, elements="Station_Id_C,Station_Name,Datetime,PRE_1h",
    staIds=staIds)
  data$PRE_1h = as.numeric(data$PRE_1h)
  data[data == 999999.0] <- NA
  data$Datetime = as.POSIXct(data$Datetime, format='%Y%m%d%H%M%S', tz="GMT")
  
  # plot rainfall series
  g <- ggplot(data=data, aes(Datetime, PRE_1h, group=Station_Name, colour=Station_Name))+
    geom_line(size=1) + geom_point(size=3, fill="white") +
    labs(x="Time", y="One hour rainfall (mm)") +
    ggtitle("Station one-hour rainfall observation") +
    scale_x_datetime(labels=scales::date_format("%dT%H"),
                     breaks=scales::date_breaks("6 hour"),
                     minor_breaks=scales::date_breaks("1 hour")) +
    coord_cartesian(ylim=c(0, max(c(5,max(data$PRE_1h,na.rm=TRUE)+2)))) +
    ggthemes::theme_igray(base_size=16) +
    ggthemes::scale_colour_tableau(name="Station")
  
  # draw plot
  dev.new(width=12, height=6, noRStudioGD=TRUE)
  g
}


