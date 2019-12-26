
library(tidyr)
library(dplyr)

setwd('E:\\SURF_CLI_CHN_TEM_DAY_HOMO\\datasets\\Tave')

files = list.files(pattern="*.txt",full.names=TRUE)
qdata <- NULL
for (ifile in files) {
  rdata <- read.table(ifile, stringsAsFactors=FALSE)
  ID <- strtoi(strsplit(ifile,c("_|[.]"))[[1]][8])
  rdata <- cbind(rdata, rep(ID,nrow(rdata)))
  names(rdata) <- c("year","month","day","value","ID")
  if (exists("qdata")) qdata <- rbind(qdata, rdata) else qdata <- rdata
}

