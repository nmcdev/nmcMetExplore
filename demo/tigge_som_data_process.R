
# load library
library(dkMetExplorer)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ncdf4)

# set work directory
workDir = 'L:/reanalysis/som_synoptic/daily_obs'
setwd(file.path(workDir))

#
# set data

# set download element
element <- c("TEM_Avg","TEM_Max","TEM_Min","PRE_Time_0808","WIN_S")
years <- 1979:2015

# station Info
data("NWSInfo")


#
# retrieve and download data from CIMISS dataset
#

# loop every element for downloading
for (el in element) {
  elements <- paste("Station_Name,Province,Station_Id_d,Station_levl,Datetime,Lat,Lon,",el,sep="")
  util_day_history_obs_download(years=years, dateRange=c('0101','1231'),
                                elements=elements, outName=el, outDir=".")
}


#
# process the station data
#

# deal precipitation data
obsRain <- NULL
for (i in years) {
  load(paste("PRE_Time_0808_",as.character(i),".Rdata",sep=""))
  obsData <- obsData%>%
    select(Station_Id_d,Datetime,PRE_Time_0808)%>%
    transmute(ID=as.numeric(Station_Id_d),
              Datetime=parse_date_time(Datetime,"%Y-%m-%d %H:%M:%S.0"),
              rain=as.numeric(PRE_Time_0808))%>%
    filter(ID %in% NWSInfo$ID, rain <= 1600.)
  obsRain <-rbind(obsRain, obsData)
  rm(obsData)
}
obsRain <- spread(obsRain,Datetime,rain)
obsRainInfo <- obsRain %>% select(ID) %>%
  left_join(NWSInfo,by="ID")
obsRain <- select(obsRain, -ID)
write.table(obsRain, file="PRE_Time_0808.data",
            quote=FALSE, na="-9999.0", row.names=FALSE)
write.table(obsRainInfo, file="PRE_Time_0808.info", sep=",",
            quote=FALSE, row.names=FALSE, col.names=FALSE)

# deal average temperature
obsTemp <- NULL
for (i in years) {
  load(paste("TEM_Avg_",as.character(i),".Rdata",sep=""))
  obsData <- obsData%>%
    select(Station_Id_d,Datetime,TEM_Avg)%>%
    transmute(ID=as.numeric(Station_Id_d),
              Datetime=parse_date_time(Datetime,"%Y-%m-%d %H:%M:%S.0"),
              temp=as.numeric(TEM_Avg))%>%
    filter(ID %in% NWSInfo$ID, temp <= 100.)
  obsTemp <-rbind(obsTemp, obsData)
  rm(obsData)
}
obsTemp <- spread(obsTemp,Datetime,temp)
obsTempInfo <- obsTemp %>% select(ID) %>%
  left_join(NWSInfo,by="ID")
obsTemp <- select(obsTemp, -ID)
write.table(obsTemp, file="TEM_Avg.data",
            quote=FALSE, na="-9999.0", row.names=FALSE)
write.table(obsTempInfo, file="TEM_Avg.info", sep=",",
            quote=FALSE, row.names=FALSE, col.names=FALSE)

# deal max temperature
obsTemp <- NULL
for (i in years) {
  load(paste("TEM_Max_",as.character(i),".Rdata",sep=""))
  obsData <- obsData%>%
    select(Station_Id_d,Datetime,TEM_Max)%>%
    transmute(ID=as.numeric(Station_Id_d),
              Datetime=parse_date_time(Datetime,"%Y-%m-%d %H:%M:%S.0"),
              temp=as.numeric(TEM_Max))%>%
    filter(ID %in% NWSInfo$ID, temp <= 100.)
  obsTemp <-rbind(obsTemp, obsData)
  rm(obsData)
}
obsTemp <- spread(obsTemp,Datetime,temp)
obsTempInfo <- obsTemp %>% select(ID) %>%
  left_join(NWSInfo,by="ID")
obsTemp <- select(obsTemp, -ID)
write.table(obsTemp, file="TEM_Max.data",
            quote=FALSE, na="-9999.0", row.names=FALSE)
write.table(obsTempInfo, file="TEM_Max.info", sep=",",
            quote=FALSE, row.names=FALSE, col.names=FALSE)

# deal min temperature
obsTemp <- NULL
for (i in years) {
  load(paste("TEM_Min_",as.character(i),".Rdata",sep=""))
  obsData <- obsData%>%
    select(Station_Id_d,Datetime,TEM_Min)%>%
    transmute(ID=as.numeric(Station_Id_d),
              Datetime=parse_date_time(Datetime,"%Y-%m-%d %H:%M:%S.0"),
              temp=as.numeric(TEM_Min))%>%
    filter(ID %in% NWSInfo$ID, temp <= 100.)
  obsTemp <-rbind(obsTemp, obsData)
  rm(obsData)
}
obsTemp <- spread(obsTemp,Datetime,temp)
obsTempInfo <- obsTemp %>% select(ID) %>%
  left_join(NWSInfo,by="ID")
obsTemp <- select(obsTemp, -ID)
write.table(obsTemp, file="TEM_Min.data",
            quote=FALSE, na="-9999.0", row.names=FALSE)
write.table(obsTempInfo, file="TEM_Min.info", sep=",",
            quote=FALSE, row.names=FALSE, col.names=FALSE)

# deal wind speed
obsTemp <- NULL
for (i in years) {
  load(paste("WIN_S_",as.character(i),".Rdata",sep=""))
  obsData <- obsData%>%
    select(Station_Id_d,Datetime,WIN_S)%>%
    transmute(ID=as.numeric(Station_Id_d),
              Datetime=parse_date_time(Datetime,"%Y-%m-%d %H:%M:%S.0"),
              temp=as.numeric(WIN_S))%>%
    filter(ID %in% NWSInfo$ID, temp <= 100.)
  obsTemp <-rbind(obsTemp, obsData)
  rm(obsData)
}
obsTemp <- spread(obsTemp,Datetime,temp)
obsTempInfo <- obsTemp %>% select(ID) %>%
  left_join(NWSInfo,by="ID")
obsTemp <- select(obsTemp, -ID)
write.table(obsTemp, file="WIN_S.data",
            quote=FALSE, na="-9999.0", row.names=FALSE)
write.table(obsTempInfo, file="WIN_S.info", sep=",",
            quote=FALSE, row.names=FALSE, col.names=FALSE)


