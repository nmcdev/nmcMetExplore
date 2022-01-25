#' Display total rain observation over China.
#'
#' @param totalRain, data.frame, {lon, lat, totalRain}
#' @param nx, interpolated grid nx
#' @param ny, interpolated grid ny
#' @param myat, contour levels
#' @param xlim, map longitude limit
#' @param ylim, map latitude limit
#' @param colors, color pallete
#' @param main, plot titile
#' @param interpKrige, interpolate totalRain by krige method (slowly)
#' @param dgrid, grid interval
#' @param imageFile, output image file
#'
#' @return NULL
#' @export
#'
#' @examples
#'   display_total_rain_china(totalRain)
#'
draw_rain_china <- function(totalRain, nx=128, ny=76,
                            myat=seq(0, 2400, by=300),
                            xlim=c(72,136), ylim=c(16,54),
                            colors=NULL, main=NULL,
                            interpKrige=FALSE, dgrid=0.5,
                            imageFile=NULL){
  # change column names
  colnames(totalRain) <- c("x", "y", "z")
  
  # interpolation to grid
  if (interpKrige) {
    pred <-expand.grid(seq(xlim[1],xlim[2],by=dgrid),seq(ylim[1],ylim[2],by=dgrid))    # create predicted grid
    pred <-cbind(pred[,1], pred[,2])
    pred <-sp::SpatialPoints(pred)
    totalRainFLD <- sp::SpatialPointsDataFrame(totalRain[,1:2],totalRain[,3])          # interpolate to grid
    totalRainFLD <- automap::autoKrige(z~1, totalRainFLD, new_data=pred)
    totalRainFLD <- as.data.frame(totalRainFLD$krige_output)
    totalRainFLD <- raster::rasterFromXYZ(totalRainFLD[,1:3],                          # convert to raster
                                          crs='+proj=longlat +ellps=WGS84')
  } else {
    totalRainFLD <- akima::interp(x=totalRain[[1]], y=totalRain[[2]],
                                  z=totalRain[[3]], linear=TRUE, nx=nx,ny=ny)
    totalRainFLD <- raster::raster(totalRainFLD, crs='+proj=longlat +ellps=WGS84')
  }
  
  # mask out china region
  chinashp <- rgdal::readOGR(system.file("extdata",package="nmcMetResources"), "bou1_4p")
  totalRainFLD <- raster::mask(totalRainFLD, chinashp, inverse = FALSE)
  
  # set color key
  myColorkey <- list(at=myat, labels=list(at=myat,cex=1.5))
  if (is.null(colors)) colors <- rev(RColorBrewer::brewer.pal(length(myat), 'RdBu'))
  
  # draw raster
  provincesBound <- nmcMetResources::provinceDF
  riversBound <- nmcMetResources::river1DF
  p1 <- rasterVis::levelplot(totalRainFLD, layers=1, margin=FALSE, contour=FALSE,
                             at=myat, colorkey=myColorkey, xlim=xlim, ylim=ylim,
                             scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                             xlab=list("Longitude", cex=1.5), ylab=list("Latitude", cex=1.5),
                             main=list(label=main, cex=1.5), lwd=2,
                             par.settings=rasterVis::rasterTheme(region=colors,axis.line=list(lwd = 2))) +
    latticeExtra::layer(panel.lines(lon,lat,col="black"),data=provincesBound) +
    latticeExtra::layer(panel.lines(lon,lat,col="blue"),data=riversBound)
  p2 <- lattice::xyplot(lat ~ lon, provincesBound, type='l', col='black',
                        scales=list(x=list(at=NULL),y=list(at=NULL)), lwd=1.5,
                        aspect="iso",xlab="",ylab="",xlim=c(106,124),ylim=c(3,23))
  
  # show the graphics
  if (is.null(imageFile)) {
    dev.new(width=14, height=10, noRStudioGD=TRUE)
    print(p1, position=c(0,0,1,1), more=TRUE)
    print(p2, position=c(0.1,0.07,0.32,0.4))
  } else {
    cairo_pdf(filename=imageFile,pointsize=12,width=9,height=6.4,family="simhei")
    print(p1, position=c(0,0,1,1), more=TRUE)
    print(p2, position=c(0.1,0.07,0.37,0.45))
    dev.off()
  }
  return(NULL)
}


#' Title
#'
#' @param staIds 
#' @param startTimeStr 
#' @param nhours 
#' 
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @return
#' @export
#'
#' @examples
#'    g <- surf_1h_obs_pre_series("54399,54511", startTimeStr="2020020500", nhours=36)
#'    
surf_1h_obs_pre_series <- function(staIds, startTimeStr, nhours=36){
  
  # get the time range
  timeRange <- get_music_time_range_str(startTimeStr, num=nhours, units="hour")
  
  # retrieve the observation data
  dataCode <- "SURF_CHN_MUL_HOR"
  elements <- "Station_Name,Station_Id_C,Lat,Lon,Alti,Datetime,PRE_1h"
  obsData <- nmcMetIO::cimiss_obs_by_time_range_and_staIds(timeRange, dataCode=dataCode, elements=elements, staIds=staIds)
  
  # prepare the data
  obsData <- obsData %>% dplyr::mutate(Name=paste0(Station_Name,"(",Station_Id_C,")"))
  
  # prepare plot figure
  sysfonts::font_add("simhei", "simhei.ttf")
  showtext::showtext_auto()
  
  # plot the rainfall series
  g <- ggplot(data=obsData, aes(x=Datetime))+
    geom_col(aes(y=PRE_1h, fill=Name), show.legend = FALSE) + 
    geom_text(aes(y=PRE_1h, label=ifelse(obsData$PRE_1h > quantile(obsData$PRE_1h, 0.9),
                                         as.character(obsData$PRE_1h),'')), vjust=-0.5) +
    scale_x_datetime(date_labels="%dT%H", date_breaks="3 hours", date_minor_breaks="1 hours", expand=c(0,0)) +
    scale_y_continuous(limits=c(0, max(0.5, obsData$PRE_1h)), expand=expand_scale(mult = c(0, .1))) + 
    facet_grid(rows = vars(Name)) + 
    labs(y="降水量 ", x="", title="地面气象站逐小时降水观测 ",
         subtitle=paste0(strftime(min(obsData$Datetime), "%Y-%m-%dT%H:%M", tz="GMT"), ' 至 ',
                         strftime(max(obsData$Datetime), "%Y-%m-%dT%H:%M", tz="GMT"), ' 时(UTC)逐1小时累积降水量观测 '),
         caption="Data source from the CIMISS of National Meteorological Information Center. \n Producted by National Meteorological Center of CMA.") +
    hrbrthemes::theme_ipsum(base_family='simhei', base_size=16, axis_title_size=16,
                            plot_title_size=22, subtitle_size=14, strip_text_size=16,
                            caption_family='Arial Narrow', caption_margin=0) +
    theme(panel.spacing = unit(0.8, "lines"))
  return(g)
}


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


