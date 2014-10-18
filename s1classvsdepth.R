library(gdata)
library(ggplot2)

data1011 <- file.path("/Volumes/HD/Users/MoritzSchmid-Takuvik/Documents/Work/Projects/LOKI_methods/R/attempt2/101a/1011RFspecweight5data.xls")

# Read in XLS data and rename headers for ease of access
jdata = read.xls(data1011, pattern="RunNo")    # Header line using pattern

jdata1 = jdata[c(1:9424), c(1:4,7,38)]
names(jdata1) = c('RunNo', 'prediction' , 'length_pred','width_pred','Area_mm','depth')

c1<-as.numeric(jdata1$prediction)
jdata1$pred2<-c1
#c2<-c1==31
#c3<-c1==17
#c4<-c1==29

"c3<-jdata1$prediction=='ChypIVantF_dors'
c4<-jdata1$prediction=='ChypIVdorsEX'
c5<-jdata1$prediction=='ChypIVlat'"

#c5<-c(c2|c3|c4)


#making subsets for taxa
subset.chypIV<-subset(jdata1, pred2==31&&17&&29,select = c(prediction, depth,pred2))



#code for single taxa
qplot(depth, data=subset.chypIV, geom="freqpoly", group=prediction, colour=prediction, position="identity", binwidth=2,xlim=c(315.3574,0))+coord_flip()

#code for all taxa in column "PREDICTION"
qplot(Realdepth, data=jdata1, geom="freqpoly", group=PREDICTION, colour=PREDICTION, position="identity",binwidth=1,xlim=c(5,0))+coord_flip()