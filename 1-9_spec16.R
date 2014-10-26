library(dplyr)
library(gdata)
library(ggplot2)
#theme,get rid of grey background
#theme_set(theme_bw())
data1011 <- file.path("/Volumes/HD/Users/MoritzSchmid-Takuvik/Documents/Work/Projects/LOKI_methods/R/attempt2/101a/combined_101_2,5,8,9_combined_w_spec_5_16_1-9_final.csv")

# Read in XLS data and rename headers for ease of access
jdata = read.csv(data1011)    # Header line using pattern

jdata1 <- tbl_df(jdata) #more handable table of dplyr

jdata2 = jdata1[c(1:75420), c(1:7,10,41,42,44,45,46,48)] #which columns and rows to take
names(jdata2) = c('overallno','runno','haul', 'prediction16' , 'prediction5', 'length_pred','width_pred','area_mm','depth','salinity','oxy_concentration','temperature','oxy_saturation','Fluorescence_high') #header names

haul1<-filter(jdata2,haul==1)
haul2<-filter(jdata2,haul==2)
haul5<-filter(jdata2,haul==5)
haul8<-filter(jdata2,haul==8)
haul9<-filter(jdata2,haul==9)



#sepc16
#HAUL1


#combine all stage subclasses(lat/dors) to one
#combine all stage subclasses(lat/dors) to one
allcglac3h1<-filter(haul1,prediction16 %in% c("CglacIIIdors","CglacIIIlat"))
#add new column pred so they are all called the same
allcglac3h1$pred<-c("Calanus glacialis stage 3")
boxplot(allcglac3h1$depth)

allcglac4h1<-filter(haul1,prediction16 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4h1$pred<-c("Calanus glacialis stage 4")
weighted.mean(allcglac4h1$depth)

allcglac5h1<-filter(haul1,prediction16 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5h1$pred<-c("Calanus glacialis stage 5")
weighted.mean(allcglac5h1$depth)


allcglacfh1<-filter(haul1,prediction16 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacfh1$pred<-c("Calanus glacialis female")

allchyp3h1<-filter(haul1,prediction16 %in% c("ChypIIIdorEX", "ChypIIIdors",     "ChypIIIlat"))
allchyp3h1$pred<-c("Calanus hyperboreus stage 3")

allchyp4h1<-filter(haul1,prediction16 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4h1$pred<-c("Calanus hyperboreus stage 4")

allchyp5h1<-filter(haul1,prediction16 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5h1$pred<-c("Calanus hyperboreus stage 5")

allchypfh1<-filter(haul1,prediction16 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypfh1$pred<-c("Calanus hyperboreus female")

allmlong3h1<-filter(haul1,prediction16 %in% c("MlongIIIdors","MlongIIIlat"))
allmlong3h1$pred<-c("Metridia longa stage 3")

allmlong4h1<-filter(haul1,prediction16 %in% c("MlongaIVlat","MlongIVdors"))
allmlong4h1$pred<-c("Metridia longa stage 4")

allmlong5h1<-filter(haul1,prediction16 %in% c("MlongVdors","MlongVlat"))
allmlong5h1$pred<-c("Metridia longa stage 5")

allmlongfh1<-filter(haul1,prediction16 %in% c("MlongFdors","MlongFdorsEx","MlongFlat"))
allmlongfh1$pred<-c("Metridia longa female")

#HAUL2


#-------------

#combine all stage subclasses(lat/dors) to one

allcglac3h2<-filter(haul2,prediction16 %in% c("CglacIIIdors","CglacIIIlat"))
#add new column pred so they are all called the same
allcglac3h2$pred<-c("Calanus glacialis stage 3")

allcglac4h2<-filter(haul2,prediction16 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4h2$pred<-c("Calanus glacialis stage 4")

allcglac5h2<-filter(haul2,prediction16 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5h2$pred<-c("Calanus glacialis stage 5")

allcglacfh2<-filter(haul2,prediction16 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacfh2$pred<-c("Calanus glacialis female")

allchyp3h2<-filter(haul2,prediction16 %in% c("ChypIIIdorEX", "ChypIIIdors",     "ChypIIIlat"))
allchyp3h2$pred<-c("Calanus hyperboreus stage 3")

allchyp4h2<-filter(haul2,prediction16 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4h2$pred<-c("Calanus hyperboreus stage 4")

allchyp5h2<-filter(haul2,prediction16 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5h2$pred<-c("Calanus hyperboreus stage 5")

allchypfh2<-filter(haul2,prediction16 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypfh2$pred<-c("Calanus hyperboreus female")

allmlong3h2<-filter(haul2,prediction16 %in% c("MlongIIIdors","MlongIIIlat"))
allmlong3h2$pred<-c("Metridia longa stage 3")

allmlong4h2<-filter(haul2,prediction16 %in% c("MlongaIVlat","MlongIVdors"))
allmlong4h2$pred<-c("Metridia longa stage 4")

allmlong5h2<-filter(haul2,prediction16 %in% c("MlongVdors","MlongVlat"))
allmlong5h2$pred<-c("Metridia longa stage 5")

allmlongfh2<-filter(haul2,prediction16 %in% c("MlongFdors","MlongFdorsEx","MlongFlat"))
allmlongfh2$pred<-c("Metridia longa female")

#HAUL5
#-------------
#combine all stage subclasses(lat/dors) to one

allcglac3h5<-filter(haul5,prediction16 %in% c("CglacIIIdors","CglacIIIlat"))
#add new column pred so they are all called the same
allcglac3h5$pred<-c("Calanus glacialis stage 3")

allcglac4h5<-filter(haul5,prediction16 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4h5$pred<-c("Calanus glacialis stage 4")

allcglac5h5<-filter(haul5,prediction16 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5h5$pred<-c("Calanus glacialis stage 5")

allcglacfh5<-filter(haul5,prediction16 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacfh5$pred<-c("Calanus glacialis female")

allchyp3h5<-filter(haul5,prediction16 %in% c("ChypIIIdorEX", "ChypIIIdors",     "ChypIIIlat"))
allchyp3h5$pred<-c("Calanus hyperboreus stage 3")

allchyp4h5<-filter(haul5,prediction16 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4h5$pred<-c("Calanus hyperboreus stage 4")

allchyp5h5<-filter(haul5,prediction16 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5h5$pred<-c("Calanus hyperboreus stage 5")

allchypfh5<-filter(haul5,prediction16 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypfh5$pred<-c("Calanus hyperboreus female")

allmlong3h5<-filter(haul5,prediction16 %in% c("MlongIIIdors","MlongIIIlat"))
allmlong3h5$pred<-c("Metridia longa stage 3")

allmlong4h5<-filter(haul5,prediction16 %in% c("MlongaIVlat","MlongIVdors"))
allmlong4h5$pred<-c("Metridia longa stage 4")

allmlong5h5<-filter(haul5,prediction16 %in% c("MlongVdors","MlongVlat"))
allmlong5h5$pred<-c("Metridia longa stage 5")

allmlongfh5<-filter(haul5,prediction16 %in% c("MlongFdors","MlongFdorsEx","MlongFlat"))
allmlongfh5$pred<-c("Metridia longa female")



#HAUL8
#-------------
#combine all stage subclasses(lat/dors) to one

allcglac3h8<-filter(haul8,prediction16 %in% c("CglacIIIdors","CglacIIIlat"))
#add new column pred so they are all called the same
allcglac3h8$pred<-c("Calanus glacialis stage 3")

allcglac4h8<-filter(haul8,prediction16 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4h8$pred<-c("Calanus glacialis stage 4")

allcglac5h8<-filter(haul8,prediction16 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5h8$pred<-c("Calanus glacialis stage 5")

allcglacfh8<-filter(haul8,prediction16 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacfh8$pred<-c("Calanus glacialis female")

allchyp3h8<-filter(haul8,prediction16 %in% c("ChypIIIdorEX", "ChypIIIdors",     "ChypIIIlat"))
allchyp3h8$pred<-c("Calanus hyperboreus stage 3")

allchyp4h8<-filter(haul8,prediction16 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4h8$pred<-c("Calanus hyperboreus stage 4")

allchyp5h8<-filter(haul8,prediction16 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5h8$pred<-c("Calanus hyperboreus stage 5")

allchypfh8<-filter(haul8,prediction16 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypfh8$pred<-c("Calanus hyperboreus female")

allmlong3h8<-filter(haul8,prediction16 %in% c("MlongIIIdors","MlongIIIlat"))
allmlong3h8$pred<-c("Metridia longa stage 3")

allmlong4h8<-filter(haul8,prediction16 %in% c("MlongaIVlat","MlongIVdors"))
allmlong4h8$pred<-c("Metridia longa stage 4")

allmlong5h8<-filter(haul8,prediction16 %in% c("MlongVdors","MlongVlat"))
allmlong5h8$pred<-c("Metridia longa stage 5")

allmlongfh8<-filter(haul8,prediction16 %in% c("MlongFdors","MlongFdorsEx","MlongFlat"))
allmlongfh8$pred<-c("Metridia longa female")


#HAUL9
#-------------
#combine all stage subclasses(lat/dors) to one

allcglac3h9<-filter(haul9,prediction16 %in% c("CglacIIIdors","CglacIIIlat"))
#add new column pred so they are all called the same
allcglac3h9$pred<-c("Calanus glacialis stage 3")

allcglac4h9<-filter(haul9,prediction16 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4h9$pred<-c("Calanus glacialis stage 4")

allcglac5h9<-filter(haul9,prediction16 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5h9$pred<-c("Calanus glacialis stage 5")

allcglacfh9<-filter(haul9,prediction16 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacfh9$pred<-c("Calanus glacialis female")

allchyp3h9<-filter(haul9,prediction16 %in% c("ChypIIIdorEX", "ChypIIIdors",     "ChypIIIlat"))
allchyp3h9$pred<-c("Calanus hyperboreus stage 3")

allchyp4h9<-filter(haul9,prediction16 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4h9$pred<-c("Calanus hyperboreus stage 4")

allchyp5h9<-filter(haul9,prediction16 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5h9$pred<-c("Calanus hyperboreus stage 5")

allchypfh9<-filter(haul9,prediction16 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypfh9$pred<-c("Calanus hyperboreus female")

allmlong3h9<-filter(haul9,prediction16 %in% c("MlongIIIdors","MlongIIIlat"))
allmlong3h9$pred<-c("Metridia longa stage 3")

allmlong4h9<-filter(haul9,prediction16 %in% c("MlongaIVlat","MlongIVdors"))
allmlong4h9$pred<-c("Metridia longa stage 4")

allmlong5h9<-filter(haul9,prediction16 %in% c("MlongVdors","MlongVlat"))
allmlong5h9$pred<-c("Metridia longa stage 5")

allmlongfh9<-filter(haul9,prediction16 %in% c("MlongFdors","MlongFdorsEx","MlongFlat"))
allmlongfh9$pred<-c("Metridia longa female")




# make plots for 24h cycle for chyp,cglac,mlong freqpoly

#haul1
#plot all those species/stage combinations

p13h1<-qplot(depth, data=allcglac3h1, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 3 at t0")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p1h1<-qplot(depth, data=allcglac4h1, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 4 at t0")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p2h1<-qplot(depth, data=allcglac5h1, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,50))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 5 at t0")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p3h1<-qplot(depth, data=allcglacfh1, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis female at t0")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p14h1<-qplot(depth, data=allchyp3h1, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 3 at t0")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p4h1<-qplot(depth, data=allchyp4h1, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,72))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 4 at t0")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p5h1<-qplot(depth, data=allchyp5h1, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,35))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 5 h1")
p6h1<-qplot(depth, data=allchypfh1, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,20))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus female at t0")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p15h1<-qplot(depth, data=allmlong3h1, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,15))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 3 at t0")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p10h1<-qplot(depth, data=allmlong4h1, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 4 at t0")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p11h1<-qplot(depth, data=allmlong5h1, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,10))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 5 at t0")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p12h1<-qplot(depth, data=allmlongfh1, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa female at t0")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")


#plot all those species/stage combinations

p13h2<-qplot(depth, data=allcglac3h2, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 3 at t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p1h2<-qplot(depth, data=allcglac4h2, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 4 at t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p2h2<-qplot(depth, data=allcglac5h2, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,50))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 5 at t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p3h2<-qplot(depth, data=allcglacfh2, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis female at t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p14h2<-qplot(depth, data=allchyp3h2, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 3 at t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p4h2<-qplot(depth, data=allchyp4h2, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,72))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 4 at t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p5h2<-qplot(depth, data=allchyp5h2, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,35))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 5 at t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p6h2<-qplot(depth, data=allchypfh2, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,20))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus female at t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p15h2<-qplot(depth, data=allmlong3h2, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,15))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 3 at t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p10h2<-qplot(depth, data=allmlong4h2, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 4 at t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p11h2<-qplot(depth, data=allmlong5h2, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,10))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 5 at t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p12h2<-qplot(depth, data=allmlongfh2, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa female at t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")


#plot all those species/stage combinations

p13h5<-qplot(depth, data=allcglac3h5, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 3 at t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p1h5<-qplot(depth, data=allcglac4h5, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 4 at t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p2h5<-qplot(depth, data=allcglac5h5, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,50))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 5 at t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p3h5<-qplot(depth, data=allcglacfh5, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis female at t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")


p14h5<-qplot(depth, data=allchyp3h5, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 3 at t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p4h5<-qplot(depth, data=allchyp4h5, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,72))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 4 at t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p5h5<-qplot(depth, data=allchyp5h5, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,35))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 5 at t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p6h5<-qplot(depth, data=allchypfh5, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,20))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus female at t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p15h5<-qplot(depth, data=allmlong3h5, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,15))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 3 at t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p10h5<-qplot(depth, data=allmlong4h5, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 4 at t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p11h5<-qplot(depth, data=allmlong5h5, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,10))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 5 at t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p12h5<-qplot(depth, data=allmlongfh5, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa female at t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")


#plot all those species/stage combinations
p13h8<-qplot(depth, data=allcglac3h8, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 3 at t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p1h8<-qplot(depth, data=allcglac4h8, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 4 at t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p2h8<-qplot(depth, data=allcglac5h8, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,50))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 5 at t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p3h8<-qplot(depth, data=allcglacfh8, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis female at t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p14h8<-qplot(depth, data=allchyp3h8, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 3 at t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p4h8<-qplot(depth, data=allchyp4h8, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,72))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 4 at t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p5h8<-qplot(depth, data=allchyp5h8, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,35))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 5 at t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p6h8<-qplot(depth, data=allchypfh8, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,20))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus female at t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p15h8<-qplot(depth, data=allmlong3h8, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,15))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 3 at t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p10h8<-qplot(depth, data=allmlong4h8, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 4 at t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p11h8<-qplot(depth, data=allmlong5h8, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,10))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 5 at t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p12h8<-qplot(depth, data=allmlongfh8, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa female at t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")


#plot all those species/stage combinations

p13h9<-qplot(depth, data=allcglac3h9, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 3 at t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p1h9<-qplot(depth, data=allcglac4h9, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 4 at t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p2h9<-qplot(depth, data=allcglac5h9, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,50))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 5 at t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p3h9<-qplot(depth, data=allcglacfh9, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,12.5))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis female at t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p14h9<-qplot(depth, data=allchyp3h9, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 3 at t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p4h9<-qplot(depth, data=allchyp4h9, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,72))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 4 at t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p5h9<-qplot(depth, data=allchyp5h9, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,35))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 5 at t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p6h9<-qplot(depth, data=allchypfh9, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,20))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus female at t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p15h9<-qplot(depth, data=allmlong3h9, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,15))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 3 at t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p10h9<-qplot(depth, data=allmlong4h9, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 4 at t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p11h9<-qplot(depth, data=allmlong5h9, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,10))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 5 at t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")

p12h9<-qplot(depth, data=allmlongfh9, geom="freqpoly", group=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,25))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa female at t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")




#bw theme
theme_set(theme_bw())
source('multiplot.R')

multiplot(p13h1, p13h2, p13h5, p13h8, p13h9,cols=2)
multiplot(p14h1, p14h2, p14h5, p14h8, p14h9,cols=2)
multiplot(p15h1, p15h2, p15h5, p15h8, p15h9,cols=2)

multiplot(p1h1, p1h2, p1h5, p1h8, p1h9,cols=2)
multiplot(p2h1, p2h2, p2h5, p2h8, p2h9,cols=2)
multiplot(p3h1, p3h2, p3h5, p3h8, p3h9,cols=2)
multiplot(p4h1, p4h2, p4h5, p4h8, p4h9,cols=2)
multiplot(p5h1, p5h2, p5h5, p5h8, p5h9,cols=2)
multiplot(p6h1, p6h2, p6h5, p6h8, p6h9,cols=2)
multiplot(p10h1, p10h2, p10h5, p10h8, p10h9,cols=2)
multiplot(p11h1, p11h2, p11h5, p11h8, p11h9,cols=2)
multiplot(p12h1, p12h2, p12h5, p12h8, p12h9,cols=2)

theme_set(theme_bw())
source('multiplot.R')
#fluo graphs
fl1<-qplot(depth, Fluorescence_high,data=haul1, xlim=c(344.9678,0),ylim=c(0,4.5),geom=c("point","smooth"))+coord_flip()+labs(title = "Timestep t0 (6:34h)")+ labs(x = "Depth (m)")+ labs(y = "Fluorescence (µg/l)")

fl2<-qplot(depth, Fluorescence_high,data=haul2, xlim=c(344.9678,0),ylim=c(0,4.5),geom=c("point","smooth"))+coord_flip()+labs(title = "Timestep t1 (10:27h)")+ labs(x = "Depth (m)")+ labs(y = "Fluorescence (µg/l)")

fl5<-qplot(depth, Fluorescence_high,data=haul5, xlim=c(344.9678,0),ylim=c(0,4.5),geom=c("point","smooth"))+coord_flip()+labs(title = "Timestep t2 (18:37h)")+ labs(x = "Depth (m)")+ labs(y = "Fluorescence (µg/l)")

fl8<-qplot(depth, Fluorescence_high,data=haul8, xlim=c(344.9678,0),ylim=c(0,4.5),geom=c("point","smooth"))+coord_flip()+labs(title = "Timestep t3 (23:15h)")+ labs(x = "Depth (m)")+ labs(y = "Fluorescence (µg/l)")

fl9<-qplot(depth, Fluorescence_high,data=haul9, xlim=c(344.9678,0),ylim=c(0,4.5),geom=c("point","smooth"))+coord_flip()+labs(title = "Timestep t4 (2:37h )")+ labs(x = "Depth (m)")+ labs(y = "Fluorescence (µg/l)")

multiplot(fl1,fl2,fl5,fl8,fl9,cols=2)

#boxplots for fluorescence
#p6h9<-qplot(prediction16,Fluorescence_high, data=haul1, geom="boxplot", group=prediction16, position="identity")
