
# load library
library(dkMetExplorer)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(ggthemes)
workDir = 'D:/test/2016_rain_analysis'    # set work directory

#
# retrieve and download data from CIMISS dataset
#
setwd(file.path(workDir,"level0"))
elements <- "Station_Id_C,Station_Name,Station_Id_d,Station_levl,Datetime,Lat,Lon,PRE_Time_0808"
util_day_history_obs_download(years=1981:2015, dateRange=c('0101','1231'),
                              elements=elements, outDir=".")
# there is a limit for retrieve records,
# so have to use staLevels to control the station grade
util_day_history_obs_download(years=2016, dateRange=c('0101','1130'),
                              elements=elements, staLevels="011,012,013", outDir=".")


#
# prepare the data
#

# merge every year data file to one file,
#    convert column type
#    remove missing data(rainfall >= 1600mm records)
setwd(file.path(workDir,"level0"))
data("NWSInfo")
obsRain <- NULL
for (i in 1981:2016) {
  load(paste(as.character(i),"_day_rain_obs.Rdata",sep=""))
  obsData <- obsData%>%
    select(Station_Id_d,Datetime,PRE_Time_0808)%>%
    transmute(ID=as.numeric(Station_Id_d),
              Datetime=parse_date_time(Datetime,"%Y-%m-%d %H:%M:%S.0"),
              rain=as.numeric(PRE_Time_0808))%>%
    filter(ID %in% NWSInfo$ID,rain <= 1600.)
  print(paste('deal data in',as.character(i), ":", as.character(nrow(obsData))))
  obsRain <-rbind(obsRain, obsData)
  rm(obsData)
}
save(obsRain, file="1981-2016_rain08-08.Rdata")

# remove less records station and extract same period (3-8)
setwd(file.path(workDir,"level1"))
load("1981-2016_rain08-08.Rdata")
recNumSta <- obsRain %>% group_by(ID) %>%
  summarise(num=n()) %>% mutate(num=num/max(num)) %>%
  filter(num >= 0.8)
obsRain <- obsRain %>%
  filter(month(Datetime) >= 1 & month(Datetime) <= 11,
         ID %in% recNumSta$ID)
save(obsRain, file="1981-2016_01-11_rain08-08.Rdata")


#========================================================================================================
#
# analysis and diagnosis
#

# load data
setwd(file.path(workDir,"level2"))
load("1981-2016_03-08_rain08-08.Rdata")
data("NWSInfo")

#
# analysis national rain time series
#

# national mean rainfall
tempRain <- obsRain %>% mutate(year=year(Datetime)) %>%
            group_by(year) %>% summarise(meanRain=mean(rain))
g <- ggplot(data=tempRain, aes(x=year, y=meanRain)) +
     geom_point(size=4, fill="white") +
     geom_line(size=2) + xlim(2000,2016) +
     labs(x="Year", y="Rainfall (mm)") +
     ggtitle("China nation average rainfall (Jan 1 to Dec 31)") +
     theme_grey(base_size = 20)

# national heavy rain days percentage
tempRain1 <- obsRain %>% mutate(year=year(Datetime)) %>%
            group_by(year) %>% summarise(num=n())
tempRain2 <- obsRain %>% filter(rain >= 50) %>% mutate(year=year(Datetime)) %>%
             group_by(year) %>% summarise(num50=n()) %>% mutate(num50=num50/tempRain1$num)
tempRain3 <- obsRain %>% filter(rain >= 100) %>% mutate(year=year(Datetime)) %>%
             group_by(year) %>% summarise(num100=n()) %>% mutate(num100=num100/tempRain1$num)
tempRain <- full_join(tempRain2, tempRain3, by="year")
tempRain <- tidyr::gather(tempRain, "Rain", "Percentage", 2:3)
g <- ggplot(data=tempRain, aes(x=year, y=Percentage, colour=Rain)) +
  geom_line() + geom_point()
g <- ggplot(data=tempRain2, aes(x=year, y=num50)) +
  geom_point(size=4, fill="white") +
  geom_line(size=2) + xlim(2000,2016) +
  labs(x="Year", y="heavy rain percentage") +
  ggtitle("China nation heavy rainfall (Jan 1 to Dec 31)") +
  theme_grey(base_size = 20)


# ***************************************************************
# analysis total rain spatial distribution
# ***************************************************************
years <- c(1983, 1994, 1998, 2016)
for (yr in years) {
  tempRain <- obsRain %>% mutate(year=year(Datetime)) %>%
    filter(year==yr) %>% group_by(ID) %>% summarise(totalRain=sum(rain))
  totalRain <- dplyr::left_join(tempRain, NWSInfo, by="ID") %>% dplyr::select(lon,lat,totalRain)
  display_total_rain_china(totalRain, interpKrige=TRUE,
                           main=paste(as.character(yr),"年3月1日-8月31日降水总量(mm)",sep=""),
                           myat=seq(0, 2400, by=300), colors=RColorBrewer::brewer.pal(9, 'PuBu'),
                           imageFile=paste("D:/test/2016_rain_analysis/",
                                           as.character(yr),"_rain_total.pdf",sep=""))
}

years <- c(2016, 1983, 1994, 1998)
for (yr in years) {
  tempRain <- obsRain %>% mutate(year=year(Datetime),month=month(Datetime)) %>%
    filter(year==yr, month >= 3 & month <= 5) %>% group_by(ID) %>% summarise(totalRain=sum(rain))
  totalRain <- dplyr::left_join(tempRain, NWSInfo, by="ID") %>% dplyr::select(lon,lat,totalRain)
  display_total_rain_china(totalRain, interpKrige=TRUE,
                           main=paste(as.character(yr),"年3月1日-5月30日降水总量(mm)",sep=""),
                           myat=seq(0, 1600, by=200), colors=RColorBrewer::brewer.pal(9, 'PuBu'),
                           imageFile=paste("D:/test/2016_rain_analysis/",
                                           as.character(yr),"_rain_total_3-5.pdf",sep=""))

  tempRain <- obsRain %>% mutate(year=year(Datetime),month=month(Datetime)) %>%
    filter(year==yr, month >= 6 & month <= 8) %>% group_by(ID) %>% summarise(totalRain=sum(rain))
  totalRain <- dplyr::left_join(tempRain, NWSInfo, by="ID") %>% dplyr::select(lon,lat,totalRain)
  display_total_rain_china(totalRain, interpKrige=TRUE,
                           main=paste(as.character(yr),"年6月1日-8月31日降水总量(mm)",sep=""),
                           myat=seq(0, 1600, by=200), colors=RColorBrewer::brewer.pal(9, 'PuBu'),
                           imageFile=paste("D:/test/2016_rain_analysis/",
                                           as.character(yr),"_rain_total_6-8.pdf",sep=""))
}


#
# analysis rain anomaly
#

# ***************************************************************
# strong El Nino 3-8 rain anomaly distribution
# ***************************************************************
years <- c(1983, 1994, 1998, 2016)
tempRain_climate <- obsRain %>% mutate(year=year(Datetime)) %>%                     # 1981-2010 climate mean
  filter(year>=1981 & year<=2010 ) %>% group_by(ID) %>%
  summarise(totalRain=mean(rain,na.rm=TRUE))
for (yr in years) {
  tempRain_yr <- obsRain %>% mutate(year=year(Datetime)) %>%                          # single year mean rainfall
    filter(year==yr) %>% group_by(ID) %>% summarise(totalRain=mean(rain,na.rm=TRUE))
  tempRain <- left_join(tempRain_yr, tempRain_climate, by="ID") %>%            # compuate anomaly percentage
    mutate(anomaly=(totalRain.x-totalRain.y)*100.0/totalRain.y) %>%
    left_join(NWSInfo, by="ID") %>% select(lon,lat,anomaly)
  tempRain$anomaly[tempRain$anomaly > 120]  <- 120
  tempRain$anomaly[tempRain$anomaly < -120] <- -120
  display_total_rain_china(tempRain, interpKrige=TRUE,
                           main=paste(as.character(yr),"年3月1日-8月31日降水距平百分率(%)",sep=""),
                           myat=c(seq(-120, 120, by=30)),
                           colors=RColorBrewer::brewer.pal(9, 'RdBu'),
                           imageFile=paste("D:/test/2016_rain_analysis/",
                                           as.character(yr),"_rain_anomlay.pdf",sep=""))
}


# 3-5 month anomaly
years <- c(2016)
tempRain_climate <- obsRain %>% mutate(year=year(Datetime), month=month(Datetime)) %>%                     # 1981-2010 climate mean
  filter(year>=1981 & year<=2010, month >=3 & month <= 5 ) %>% group_by(ID) %>%
  summarise(totalRain=mean(rain,na.rm=TRUE))
for (yr in years) {
  tempRain_yr <- obsRain %>% mutate(year=year(Datetime), month=month(Datetime)) %>%                          # single year mean rainfall
    filter(year==yr, month >=3 & month <= 5) %>% group_by(ID) %>% summarise(totalRain=mean(rain,na.rm=TRUE))
  tempRain <- left_join(tempRain_yr, tempRain_climate, by="ID") %>%            # compuate anomaly percentage
    mutate(anomaly=(totalRain.x-totalRain.y)*100.0/totalRain.y) %>%
    left_join(NWSInfo, by="ID") %>% select(lon,lat,anomaly)
  tempRain$anomaly[tempRain$anomaly > 120]  <- 120
  tempRain$anomaly[tempRain$anomaly < -120] <- -120
  display_total_rain_china(tempRain, interpKrige=TRUE,
                           main=paste(as.character(yr),"年3月1日-5月31日降水距平百分率(%)",sep=""),
                           myat=c(seq(-120, 120, by=30)),
                           colors=RColorBrewer::brewer.pal(9, 'RdBu'),
                           imageFile=paste("D:/test/2016_rain_analysis/",
                                           as.character(yr),"_rain_anomlay_3-5.pdf",sep=""))
}

# 6-8 month anomaly
years <- c(2016)
tempRain_climate <- obsRain %>% mutate(year=year(Datetime), month=month(Datetime)) %>%                     # 1981-2010 climate mean
  filter(year>=1981 & year<=2010, month >=6 & month <= 8 ) %>% group_by(ID) %>%
  summarise(totalRain=mean(rain,na.rm=TRUE))
for (yr in years) {
  tempRain_yr <- obsRain %>% mutate(year=year(Datetime), month=month(Datetime)) %>%                          # single year mean rainfall
    filter(year==yr, month >=6 & month <= 8) %>% group_by(ID) %>% summarise(totalRain=mean(rain,na.rm=TRUE))
  tempRain <- left_join(tempRain_yr, tempRain_climate, by="ID") %>%            # compuate anomaly percentage
    mutate(anomaly=(totalRain.x-totalRain.y)*100.0/totalRain.y) %>%
    left_join(NWSInfo, by="ID") %>% select(lon,lat,anomaly)
  tempRain$anomaly[tempRain$anomaly > 120]  <- 120
  tempRain$anomaly[tempRain$anomaly < -120] <- -120
  display_total_rain_china(tempRain, interpKrige=TRUE,
                           main=paste(as.character(yr),"年6月1日-8月31日降水距平百分率(%)",sep=""),
                           myat=c(seq(-120, 120, by=30)),
                           colors=RColorBrewer::brewer.pal(9, 'RdBu'),
                           imageFile=paste("D:/test/2016_rain_analysis/",
                                           as.character(yr),"_rain_anomlay_6-8.pdf",sep=""))
}


# 2016 3-8 monthly rain anomaly
for (mon in 3:8) {
  tempRain_2016 <- obsRain %>% mutate(year=year(Datetime),month=month(Datetime)) %>%
    filter(year==2016, month==mon) %>% group_by(ID) %>% summarise(totalRain=mean(rain,na.rm=TRUE))
  tempRain_climate <- obsRain %>% mutate(year=year(Datetime),month=month(Datetime)) %>%
    filter(year>=1981 & year<=2010, month==mon) %>% group_by(ID) %>%
    summarise(totalRain=mean(rain,na.rm=TRUE))
  tempRain <- dplyr::left_join(tempRain_2016, tempRain_climate, by="ID") %>%
    dplyr::mutate(anomaly=(totalRain.x-totalRain.y)*100.0/totalRain.y) %>%
    dplyr::left_join(NWSInfo, by="ID") %>%
    dplyr::select(lon,lat,anomaly)
  display_total_rain_china(tempRain,
                           main=paste("2016/",as.character(mon),
                                      " rainfall anomaly percentage (climate 1981-2010)",sep=""),
                           myat=c(-300, seq(-120, 120, by=20),300))
}

# ***************************************************************
# 2016 China Daily mean rain time series + climate daily rain box
#****************************************************************
tempRain_1998 <- obsRain %>% mutate(year=year(Datetime)) %>%
  filter(year==1998) %>% group_by(Datetime) %>%
  summarise(totalRain=mean(rain,na.rm=TRUE)) %>%
  mutate(date=format(Datetime,"%m-%d"))

yr <- 2016
tempRain_yr <- obsRain %>% mutate(year=year(Datetime)) %>%
  filter(year==yr) %>% group_by(Datetime) %>%
  summarise(totalRain=mean(rain,na.rm=TRUE)) %>%
  mutate(date=format(Datetime,"%m-%d"))
tempRain_climate <- obsRain %>% mutate(year=year(Datetime)) %>%     # 计算日平均降水量
  filter(year>=1981 & year<=2010) %>% group_by(Datetime) %>%
  summarise(totalRain=mean(rain,na.rm=TRUE)) %>%
  mutate(date=format(Datetime,"%m-%d"))
#tempRain_extreme99 <- tempRain_climate %>% group_by(date) %>%
#  summarise(extreme99=quantile(totalRain,0.99, na.rm=TRUE)) %>% arrange(date) %>%
#  mutate(extreme99=zoo::rollapply(extreme99,5,"quantile",0.99,na.rm=TRUE,partial=TRUE))

labels = levels(factor(tempRain_yr$date))
axisAt = c(1,11,20,32,42,52,62,72,82,93,103,113,123,133,143,154,164,174,184)
cairo_pdf(filename=paste("D:/test/2016_rain_analysis/mean_rain_climate_boxplot_",as.character(yr),".pdf",sep=""),
         pointsize=12,width=15,height=8,family="simhei")
bpsum <- boxplot(totalRain~factor(date),data=tempRain_climate,
        main=paste(as.character(yr),"年中国范围日平均降水量与气候(1981-2010年)分布的对比",sep=""),
        xlab="日期", ylab="日平均降水量 (mm)", col="wheat4", ylim=c(0,25),
        boxwex=0.5, boxlwd=2, boxcol="wheat4", outpch=20, outcol="wheat4",
        cex.lab=1.7, cex.main=2.5, cex.names=1.5, outline=FALSE,
        axes=FALSE, frame.plot=FALSE)
axis(side=1,at=axisAt,labels=labels[axisAt],lwd=3,cex.axis=1.2)
axis(side=2,at=seq(0,25,5),lwd=3,cex.axis=1.2)
lines(x=factor(tempRain_yr$date),y=tempRain_yr$totalRain, col="BLACK", lwd=2)
# anomaly
temp <- zoo::rollapply(bpsum$stat[4,],5,"mean",na.rm=TRUE,partial=TRUE)
points(x=(factor(tempRain_yr$date))[tempRain_yr$totalRain >= temp], cex=3,
       y=tempRain_yr$totalRain[tempRain_yr$totalRain >= temp],pch=20,col="blue")
# extreme
temp <- zoo::rollapply(bpsum$stat[5,],5,"mean",na.rm=TRUE,partial=TRUE)
points(x=(factor(tempRain_yr$date))[tempRain_yr$totalRain >= temp], cex=3,
       y=tempRain_yr$totalRain[tempRain_yr$totalRain >= temp],pch=20,col="red")
# legend
legend(x="topright", legend=c("极端降水","异常降水"),
       pch=c(20,20),col=c("red","blue"),cex=2)
dev.off()
#lines(x=factor(tempRain_1998$date),y=tempRain_1998$totalRain, col="#DF8A26", lwd=2)


# 1981- 2016 Changjiang mean rain time series
regionRain <- left_join(obsRain, select(NWSInfo,ID,lon,lat), by="ID") %>%
  filter(lon >= 105 & lon <= 122, lat >= 27 & lat <= 33) %>%
  mutate(year=year(Datetime),month=month(Datetime))
tempRain <- regionRain %>% group_by(year) %>%
  summarise(meanRain=mean(rain,na.rm=TRUE))
g <- ggplot(data=tempRain, aes(x=year, y=meanRain)) +
  geom_point(size=4, fill="white") +
  geom_line(size=2) +
  labs(x="Year", y="Rainfall (mm)") +
  ggtitle("ChangeJiang average rainfall (March 1 to Aug 31)") +
  theme_grey(base_size = 20)
g

# 1981- 2016 Changjiang heavy rain days percentage
tempRain1 <- regionRain %>% group_by(year) %>% summarise(num=n())
tempRain2 <- regionRain %>% filter(rain >= 50) %>%
  group_by(year) %>% summarise(num50=n()) %>% mutate(num50=num50*100./tempRain1$num)
tempRain3 <- regionRain %>% filter(rain >= 100) %>%
  group_by(year) %>% summarise(num100=n()) %>% mutate(num100=num100*100./tempRain1$num)
tempRain <- full_join(tempRain2, tempRain3, by="year")
g <- ggplot(data=tempRain, aes(x=year, y=num100)) +
  geom_point(size=4, fill="white") +
  geom_line(size=2) +
  labs(x="Year", y="Percentage (mm)") +
  ggtitle("ChangeJiang heavy rain days (>100mm) percentage (March 1 to Aug 31)") +
  theme_grey(base_size = 20)
g
#tempRain <- tidyr::gather(tempRain, "Rain", "Percentage", 2:3)
#g <- ggplot(data=tempRain, aes(x=year, y=Percentage)) +
#  geom_line() + geom_point()

# Nothern China mean rain time series
regionRain <- left_join(obsRain, select(NWSInfo,ID,lon,lat), by="ID") %>%
  filter(lat >= 34) %>% mutate(year=year(Datetime),month=month(Datetime))
tempRain <- regionRain %>% group_by(year) %>%
  summarise(meanRain=mean(rain,na.rm=TRUE))
g <- ggplot(data=tempRain, aes(x=year, y=meanRain)) +
  geom_point(size=4, fill="white") +
  geom_line(size=2) +
  labs(x="Year", y="Rainfall (mm)") +
  ggtitle("North China average rainfall (March 1 to Aug 31)") +
  theme_grey(base_size = 20)
g

# 1981- 2016 Nothern China heavy rain days percentage
tempRain1 <- regionRain %>% group_by(year) %>% summarise(num=n())
tempRain2 <- regionRain %>% filter(rain >= 50) %>%
  group_by(year) %>% summarise(num50=n()) %>% mutate(num50=num50*100./tempRain1$num)
tempRain3 <- regionRain %>% filter(rain >= 100) %>%
  group_by(year) %>% summarise(num100=n()) %>% mutate(num100=num100*100./tempRain1$num)
tempRain <- full_join(tempRain2, tempRain3, by="year")
g <- ggplot(data=tempRain, aes(x=year, y=num100)) +
  geom_point(size=4, fill="white") +
  geom_line(size=2) +
  labs(x="Year", y="Percentage (mm)") +
  ggtitle("North China heavy rain days (>100mm) percentage (March 1 to Aug 31)") +
  theme_grey(base_size = 20)
g

#
# analysis extreme rainfall station
#

# tempRain_extreme <- obsRain %>% mutate(year=year(Datetime),yofd=yday(Datetime)) %>%
#   filter(year>=1981 & year<=2010) %>% group_by(ID,year) %>% arrange(ID,year,Datetime) %>%
#   mutate(runningMaxRain=rollmax(rain,5,fill = NA)) %>%
# tempRain_extreme1 <- ungroup(tempRain_extreme) %>% filter(runningMaxRain>=1) %>%
#   group_by(ID,yofd) %>% summarise(extremeRain=quantile(runningMaxRain,0.99))
#
# tempRain <- obsRain %>% mutate(year=year(Datetime),date=format(Datetime,"%m-%d"))
# tempRain_extreme <- tempRain %>% filter(year>=1981 & year<=2010) %>%
#   group_by(ID, date) %>% summarise(extremeRain=quantile(rain[rain >= 0.1],0.99))
#
# tempRain_extreme1 <- tempRain_extreme %>% arrange(ID, date) %>%
#   mutate(moveExtremeRain=zoo::rollapply(extremeRain,5,"max",na.rm=TRUE,partial=TRUE))
#
# tempRain <- left_join(tempRain,tempRain_extreme1,by=c("ID","date"))
#
#
# #compute same period extreme precipitation
# tempRain <- obsRain %>% mutate(year=year(Datetime)) %>%
#   filter(year>=1981 & year<=2016) %>%
#   mutate(date=format(Datetime,"%m-%d")) %>%
#   group_by(ID,year) %>% arrange(ID,year,date) %>%
#   mutate(rollMaxRain=zoo::rollapply(rain,5,"max",na.rm=TRUE,partial=TRUE))
# tempRain <- ungroup(tempRain)
#
# tempRain_extreme = tempRain %>% group_by(ID,date) %>%
#   summarise(R99=quantile(rollMaxRain[rollMaxRain >= 1],0.99,na.rm=TRUE))
#
# tempRain <- left_join(tempRain, tempRain_extreme, by=c("ID","date"))
# tempRain <- ungroup(tempRain)

# ***************************************************************
# compute extrem wet day number
# ***************************************************************

# compute R99p, Extremely wet day precipitation amount
# Annual total precipitation when daily precipitation>99th percentile

tempRain <- obsRain %>% mutate(year=year(Datetime)) %>%
  filter(year>=1981 & year<=2016)
tempRain_extreme <- tempRain %>% group_by(ID,year) %>%
  summarise(R99=quantile(rain[rain >= 0.1],0.99,na.rm=TRUE),
            R95=quantile(rain[rain >= 0.1],0.95,na.rm=TRUE)) %>%
  ungroup() %>% group_by(ID) %>%
  summarise(R99=mean(R99,na.rm=TRUE),
            R95=mean(R95,n.rm=TRUE))
tempRain <- left_join(tempRain, tempRain_extreme, by="ID") %>%
  left_join(select(NWSInfo,ID,lon,lat), by="ID")

tempRain_extreme_days <- tempRain %>% group_by(year) %>%
  summarise(China=sum(rain >= R99,na.rm=TRUE)*100./sum(rain >= 0.1, na.rm=TRUE))
tempRain_extreme_days_Yangze <- tempRain %>%
  filter(lon >= 105 & lon <= 122, lat >= 27 & lat <= 34) %>% group_by(year) %>%
  summarise(Yangze=sum(rain >= R99,na.rm=TRUE)*100./sum(rain >= 0.1, na.rm=TRUE))
tempRain_extreme_days_North <- tempRain %>%
  filter(lat >= 35) %>% group_by(year) %>%
  summarise(North=sum(rain >= R99,na.rm=TRUE)*100./sum(rain >= 0.1, na.rm=TRUE))
tempRain_extreme_days <- tempRain_extreme_days %>%
  left_join(tempRain_extreme_days_Yangze, by="year") %>%
  left_join(tempRain_extreme_days_North, by="year")
tempRain_extreme_days <- tidyr::gather(tempRain_extreme_days, "region", "percent", 2:4)

sysfonts::font.add("heiti", "simhei.ttf")
showtext::showtext.auto()
ggthemr::ggthemr("greyscale")
g <- ggplot(data=tempRain_extreme_days, aes(x=year, y=percent, color=region)) +
  geom_rect(aes(xmin=1982, xmax=1983, ymin=0, ymax=3),fill="grey60",color="grey60")+
  geom_rect(aes(xmin=1987, xmax=1988, ymin=0, ymax=3),fill="grey85",color="grey85")+
  geom_rect(aes(xmin=1991, xmax=1992, ymin=0, ymax=3),fill="grey85",color="grey85")+
  geom_rect(aes(xmin=1994, xmax=1995, ymin=0, ymax=3),fill="grey85",color="grey85")+
  geom_rect(aes(xmin=1997, xmax=1998, ymin=0, ymax=3),fill="grey60",color="grey60")+
  geom_rect(aes(xmin=2002, xmax=2003, ymin=0, ymax=3),fill="grey85",color="grey85")+
  geom_rect(aes(xmin=2006, xmax=2007, ymin=0, ymax=3),fill="grey85",color="grey85")+
  geom_rect(aes(xmin=2009, xmax=2010, ymin=0, ymax=3),fill="grey85",color="grey85")+
  geom_rect(aes(xmin=2015, xmax=2016, ymin=0, ymax=3),fill="grey60",color="grey60")+
  geom_point(size=3, fill="white") + geom_line(size=1.5,alpha=0.6) +
  labs(x="年份", y="极端降水日数百分比") +
  scale_x_continuous(breaks=seq(1981,2016,5)) + ylim(0,3) +
  scale_color_manual("区域", labels=c("全国","北方","长江"),
                     values=wesanderson::wes_palette(n=3, name="Darjeeling")) +
  ggtitle("1981-2016年中国春夏季极端降水日数百分比变化") +
  theme(text=element_text(size=16, family="heiti"))
g
pdf("D:/test/2016_rain_analysis/extreme_days.pdf", 10.5, 6)
print(g)
dev.off()

plot(tempRain_extreme_days$year,tempRain_extreme_days$extremeDayPercent,type="l")

 # tempExtreme <- tempRain_extreme %>% left_join(NWSInfo, by="ID") %>%
 #                select(lon,lat,R99)
 # display_total_rain_china(tempExtreme,
 #                          main="Extreme Rain",
 #                          myat=c(seq(0.1,120,by=10)))

# tempRain_extreme_mount <- tempRain %>% group_by(year) %>%
#   summarise(extremeDayPercent=sum(rain[rain >= R99],na.rm=TRUE)/sum(rain[rain >= 0.1],na.rm=TRUE))
# plot(tempRain_extreme_mount$year,tempRain_extreme_mount$extremeDayPercent,type="l")


#
# extreme stations distributions
#

# daily rain extreme station distribution (colored by month)
yr <- 2016
tempRain_yr <- obsRain %>% mutate(year=year(Datetime),month=month(Datetime)) %>%
  filter(year==yr) %>% group_by(ID, month) %>%
  summarise(maxRain=max(rain,na.rm=TRUE))
maxRain_climate <- obsRain %>% mutate(year=year(Datetime),month=month(Datetime)) %>%
  filter(year>=1981 & year<=2010) %>% group_by(ID, month) %>%
  summarise(ClimateMaxRain=max(rain,na.rm=TRUE))
tempRain <- left_join(tempRain_yr, maxRain_climate, by=c("ID","month")) %>%
  left_join(select(NWSInfo,ID,lon,lat), by="ID") %>%
  mutate(maxDiff=maxRain-ClimateMaxRain) %>%
  filter(maxDiff >= 0)

cmap  <- rgdal::readOGR(system.file("extdata",package="dkMetExplorer"), "bou2_4l")
cmap  <- fortify(cmap)
limit <- c(18,72,54,136)

sysfonts::font.add("heiti", "simhei.ttf")
showtext::showtext.auto()
g <- ggplot(data=tempRain, aes(x=lon, y=lat)) +
  geom_point(aes(color=factor(month), size=maxDiff)) +
  geom_path(data=cmap, aes(x=long, y=lat, group=group), colour="black") +
  ylim(limit[1],limit[3]) + xlim(limit[2],limit[4]) +
  scale_color_manual("月份", values=RColorBrewer::brewer.pal(6,'Dark2')) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  scale_size_continuous("差值") +
  labs(title=paste(as.character(yr),"年3-8月逐月极端降水站点分布",sep=""),
       x="经度", y="纬度") +
  theme(text=element_text(size=16, family="heiti"),
        legend.text=element_text(size=16),
        title=element_text(size=22, hjust=0.5,vjust=1,family="heiti",face=c("bold")))

g1 <- ggplot() +
  geom_path(data=cmap, aes(x=long, y=lat, group=group), color='black', fill='white', size=.5) +
  ylim(3,23) + xlim(106,124) +
  theme_map() + theme(panel.background = element_rect(colour = "white"))





pdf(paste("D:/test/2016_rain_analysis/extreme_spatial",as.character(yr),".pdf",sep=""), 14, 10)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width=0.18, height=0.26, x=0.18, y=0.19) #plot area for the inset map
print(g,vp=v1)
print(g1,vp=v2)
dev.off()
