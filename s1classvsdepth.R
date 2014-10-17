library(gdata)
library(ggplot2)

data1011 <- file.path("/Volumes/HD/Users/MoritzSchmid-Takuvik/Documents/Work/Projects/LOKI_methods/R/attempt2/101a/1011RFspecweight5data.xls")

# Read in XLS data and rename headers for ease of access
jdata = read.xls(data1011, pattern="RunNo")    # Header line using pattern

jdata1 = jdata[c(1:9425), c(1:4,7,38)]
names(jdata1) = c('RunNo', 'prediction' , 'length_pred','width_pred','Area_mm','depth')

#making subsets for taxa
subset.medcala<-subset(jdata1, PREDICTION=="medcala", select = c(PREDICTION, Realdepth))

#code for single taxa
qplot(Realdepth, data=subset.medcala, geom="freqpoly", group=PREDICTION, colour=PREDICTION, position="identity", binwidth=1,xlim=c(5,0))+coord_flip()

#code for all taxa in column "PREDICTION"
qplot(Realdepth, data=jdata1, geom="freqpoly", group=PREDICTION, colour=PREDICTION, position="identity",binwidth=1,xlim=c(5,0))+coord_flip()