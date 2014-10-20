library(dplyr)
library(gdata)
library(ggplot2)

data1011 <- file.path("/Volumes/HD/Users/MoritzSchmid-Takuvik/Documents/Work/Projects/LOKI_methods/R/attempt2/101a/1011RFspecweight5data.xls")

# Read in XLS data and rename headers for ease of access
jdata = read.xls(data1011, pattern="RunNo")    # Header line using pattern

jdata1 <- tbl_df(jdata) #more handable table of dplyr

jdata2 = jdata1[c(1:9424), c(1:4,7,38,39,41,42,43,45,46)] #which columns and rows to take
names(jdata2) = c('runno', 'prediction' , 'length_pred','width_pred','area_mm','depth','salinity','oxy_concentration','temperature(oxy)','oxy_saturation','Fluorescence(0-200)','Fluorescence(0-20)') #header names

allcglac4<-filter(jdata2,prediction %in% c("CglacIVdors","CglacIVlat"))
allcglac4$pred<-c("Calanus glacialis stage 4")

allcglac5<-filter(jdata2,prediction %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5$pred<-c("Calanus glacialis stage 5")

allcglacf<-filter(jdata2,prediction %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacf$pred<-c("Calanus glacialis female")

allchyp4<-filter(jdata2,prediction %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4$pred<-c("Calanus hyperboreus stage 4")

allchyp5<-filter(jdata2,prediction %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5$pred<-c("Calanus hyperboreus stage 5")

allchypf<-filter(jdata2,prediction %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypf$pred<-c("Calanus hyperboreus female")

allcyctrico<-filter(jdata2,prediction %in% c("CycTricoDORS" ,"CycTricoLAT","CycTricoMF"))
allcyctrico$pred<-c("Triconia sp.")

alleggs<-filter(jdata2,prediction %in% c("Egg"))
alleggs$pred<-c("Eggs")

allmicroc<-filter(jdata2,prediction %in% c("MicroCdors","MicroClat"))
allmicroc$pred<-c("Microcalanus sp.")

allmlong4<-filter(jdata2,prediction %in% c("MlongaIVlat","MlongIVdors"))
allmlong4$pred<-c("Metridia longa stage 4")

allmlong5<-filter(jdata2,prediction %in% c("MlongVdors","MlongVlat"))
allmlong5$pred<-c("Metridia longa stage 5")

allmlongf<-filter(jdata2,prediction %in% c("MlongFdors","MlongFdorsEx","MlongFlat"))
allmlongf$pred<-c("Metridia longa female")

allnauplius<-filter(jdata2,prediction %in% c("Nauplius"))
allnauplius$pred<-c("Nauplii")

alloithona<-filter(jdata2,prediction %in% c("Oithona"))
alloithona$pred<-c("Oithona sp.")

allpseudo4<-filter(jdata2,prediction %in% c("PseudoIVlat"))
allpseudo4$pred<-c("Pseudocalanus sp. stage 4")

allpseudo5<-filter(jdata2,prediction %in% c("PseudoVdors","PseudoVlat"))
allpseudo5$pred<-c("Pseudocalanus sp. stage 5")

allpseudof<-filter(jdata2,prediction %in% c("PseudoFdors","PseudoFlat"))
allpseudof$pred<-c("Pseudocalanus sp. female")

#graphs, code for single taxa
p1<-qplot(depth, data=allcglac4, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p2<-qplot(depth, data=allcglac5, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p3<-qplot(depth, data=allcglacf, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p4<-qplot(depth, data=allchyp4, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p5<-qplot(depth, data=allchyp5, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p6<-qplot(depth, data=allchypf, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p7<-qplot(depth, data=allcyctrico, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p8<-qplot(depth, data=alleggs, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p9<-qplot(depth, data=allmicroc, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p10<-qplot(depth, data=allmlong4, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p11<-qplot(depth, data=allmlong5, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p12<-qplot(depth, data=allmlongf, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p13<-qplot(depth, data=allnauplius, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p14<-qplot(depth, data=alloithona, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p15<-qplot(depth, data=allpseudo4, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p16<-qplot(depth, data=allpseudo5, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p17<-qplot(depth, data=allpseudof, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()


source('multiplot.R')
multiplot(p1, p2, p3,cols=2)
multiplot(p4, p5, p6,cols=2)
multiplot(p7, p8, p9,cols=2)
multiplot(p10, p11, p12,cols=2)
multiplot(p15, p16, p17,cols=2)
multiplot(p13, p14,cols=1)


cglac<-as.data.frame(mapply(c, allcglac4, allcglac5, allcglacf, SIMPLIFY=FALSE))
p18<-qplot(depth, data=cglac, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p18

chyp<-as.data.frame(mapply(c, allchyp4, allchyp5, allchypf, SIMPLIFY=FALSE))
p19<-qplot(depth, data=chyp, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p19

mlong<-as.data.frame(mapply(c, allmlong4, allmlong5, allmlongf, SIMPLIFY=FALSE))
p20<-qplot(depth, data=mlong, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p20

#classes vs env factors
qplot(depth, Fluorescence.0.20.,data=cglac, xlim=c(315.3574,0),colour=pred,geom=c("point","smooth"))+coord_flip()
qplot(depth, salinity,data=cglac, colour=pred, xlim=c(315.3574,0),colour=pred,geom=c("point","smooth"))+coord_flip()
qplot(depth,oxy_concentration,  data=cglac, xlim=c(315.3574,0),colour=pred,geom=c("point","smooth"))+coord_flip()
qplot(depth,temperature.oxy.,  data=cglac, xlim=c(315.3574,0),colour=pred,geom=c("point","smooth"))+coord_flip()
qplot(depth,oxy_saturation,  data=cglac,xlim=c(315.3574,0),colour=pred,geom=c("point","smooth"))+coord_flip()
qplot(depth,area_mm,  data=cglac, xlim=c(315.3574,0),colour=pred,geom=c("point","smooth"))+coord_flip()

#code for all taxa in column "PREDICTION"
qplot(depth, data=jdata2, geom="freqpoly", group=prediction, colour=prediction, position="identity",binwidth=3,xlim=c(315.3574,0))+coord_flip()


#test vs environmental data?
cglac_group <- group_by(cglac, pred)
pred.pos <- summarise(cglac_group,count = n(), mean.depth = weighted.mean(depth, na.rm = TRUE), mean.fluo = weighted.mean(Fluorescence.0.20.,na.rm = TRUE))

ggplot(pred.pos, aes(mean.fluo, mean.depth)) +
  geom_point(aes(size = count,colour=pred), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

ggplot(jdata1_df,aes(Fluo_A, Realdepth)) + 
  geom_point(aes(colour=PREDICTION), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()