
#
# 该程序展示如何分析历年的定量降水评分
#
# 数据来源于国家气象中心检验网站, 下载xls文件,
# 删除第一行, 并把第一列的预报时效修改对应每行,
# 删除后面的空白列, 保存成为YYYY.csv格式文件.
#
# Writed by Dai Kan, 2016/7/21
#

# load library
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)

setwd('D:\\test\\qpf_verif\\data\\入汛')

# read ts score from csv files
qdata <- NULL
for (i in 2007:2016){
  filename = paste(as.character(i),"csv",sep=".")
  rdata <- read.csv(filename, stringsAsFactors=FALSE)
  names(rdata) <- c("Fhour","Quantity","Forecaster","EC","T639","JAPAN")
  rdata$year  <- i
  if (exists("qdata")) qdata <- rbind(qdata, rdata) else qdata <- rdata
}
qdata$Fhour <- factor(qdata$Fhour)

# tidy data
qdata <- gather(qdata, qpfSource, qpfTS, Forecaster:JAPAN)
qdata$qpfSource <- factor(qdata$qpfSource, levels=c("Forecaster","EC","JAPAN","T639"))

#
# Analysis Forecaster's torrential QPF score
#

forecasterQPF <- filter(qdata, Quantity=="暴" & qpfSource=="Forecaster")
g <- ggplot(forecasterQPF, aes(year, qpfTS, fill=Fhour)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x="Year", y="Threat Score") +
  ggtitle("2007-16年（6月1日至7月21日）预报员1-7天降水评分") +
  scale_x_continuous(breaks=2010:2016)+
  theme_igray(base_size=16) + scale_fill_tableau(name="时效")
ggsave("forecaster_qpf.png", width=12, height=6)

forecasterQPF <- filter(qdata, Quantity=="暴" & qpfSource=="Forecaster" &
                          (Fhour == 24 | Fhour == 48))

g <- ggplot(forecasterQPF, aes(year, qpfTS, group=Fhour, colour=Fhour, label=qpfTS)) +
  geom_line(size=3) + geom_point(size=5, fill="white") +
  #geom_text(vjust=-0.5, colour="black") +
  labs(x="Year", y="Threat Score") +
  ggtitle("2007-16年（3月21日至7月21日）预报员1-2天降水评分") +
  scale_x_continuous(breaks=2007:2016)+
  theme_igray(base_size=16) + scale_colour_tableau(name="时效")

ggsave("forecaster_qpf_1-2.png", width=12, height=6)

#
# Compare Forecaster to model
#
heavyQPF <- filter(qdata, Quantity=="暴")
g <- ggplot(heavyQPF, aes(year, qpfTS, fill=qpfSource)) +
  geom_bar(stat="identity", position="dodge",width=.8) +
  labs(x="Year", y="Threat Score") +
  ggtitle("2010-16年（3月21日至7月21日）预报员及模式1-7天降水评分") +
  scale_x_continuous(breaks=2010:2016)+
  theme_fivethirtyeight(base_size=16) + scale_fill_ptol(name="") +
  facet_grid(Fhour ~ .)
ggsave("forecaster_model_qpf.png", width=10, height=12)

#
# 中央台大雨预报评分曲线
#
TS_25mm = data.frame(years=2007:2015,
                     Day1=c(0.232,0.239,0.248,0.253,0.262,0.272,0.285,0.27,0.281),
                     Day2=c(0.193,0.204,0.208,0.235,0.236,0.237,0.254,0.243,0.25),
                     Day3=c(0.166,0.187,0.178,0.207,0.208,0.207,0.221,0.213,0.225))
TS_25mm = gather(TS_25mm, Days, qpfTS, Day1:Day3)

g <- ggplot(TS_25mm, aes(years, qpfTS, group=Days, colour=Days)) +
  geom_line(size=3) + geom_point(size=5, fill="white") +
  labs(x="年份", y="TS评分") +
  ggtitle("2007-15年中央气象台1-3天定量降水预报大雨量级(>=25mm/day)评分") +
  scale_x_continuous(breaks=2007:2016)+
  scale_y_continuous(limits=c(0.15, 0.3))+
  theme_igray(base_size=16) + scale_colour_tableau(name="时效")
ggsave("D:\\forecaster_qpf_25mm.png", width=12, height=6)
