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
allcglac3h1<-filter(haul1,prediction16 %in% c("CglacIIIdors","CglacIIIlat"))
#add new column pred so they are all called the same
allcglac3h1$pred<-c("Calanus glacialis stage 3")

#combine all stage subclasses(lat/dors) to one
allcglac4h1<-filter(haul1,prediction16 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4h1$pred<-c("Calanus glacialis stage 4")

allcglac5h1<-filter(haul1,prediction16 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5h1$pred<-c("Calanus glacialis stage 5")

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

#combine all stage subclasses(lat/dors) to one
allcglac4h2<-filter(haul2,prediction16 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4h2$pred<-c("Calanus glacialis stage 4")

allcglac5h2<-filter(haul2,prediction16 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5h2$pred<-c("Calanus glacialis stage 5")

allcglacfh2<-filter(haul2,prediction16 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacfh2$pred<-c("Calanus glacialis female")

#----------

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
allcglac3h8<-filter(haul8,prediction16 %in% c("CglacIIIdors","CglacIIIlat"))
#add new column pred so they are all called the same
allcglac3h8$pred<-c("Calanus glacialis stage 3")

#combine all stage subclasses(lat/dors) to one
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


#------------------------------
#haul1
#mapply to combine all stages of species A to one table
cglach1<-as.data.frame(mapply(c, allcglac3h1, allcglac4h1, allcglac5h1, allcglacfh1, SIMPLIFY=FALSE))
#plot all the sages in one freqpoly plot
p18h1<-qplot(depth, data=cglach1, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,55))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis t0")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p18h1

chyph1<-as.data.frame(mapply(c, allchyp3h1,allchyp4h1, allchyp5h1, allchypfh1, SIMPLIFY=FALSE))
p19h1<-qplot(depth, data=chyph1, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,70))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus t0")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p19h1

mlongh1<-as.data.frame(mapply(c, allmlong3h1,allmlong4h1, allmlong5h1, allmlongfh1, SIMPLIFY=FALSE))
p20h1<-qplot(depth, data=mlongh1, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0),ylim=c(0,35))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa t0")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p20h1


#haul2

#mapply to combine all stages of species A to one table
cglach2<-as.data.frame(mapply(c, allcglac3h2, allcglac4h2, allcglac5h2, allcglacfh2, SIMPLIFY=FALSE))
#plot all the sages in one freqpoly plot
p18h2<-qplot(depth, data=cglach2, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,55))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p18h2

chyph2<-as.data.frame(mapply(c, allchyp3h2,allchyp4h2, allchyp5h2, allchypfh2, SIMPLIFY=FALSE))
p19h2<-qplot(depth, data=chyph2, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,70))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p19h2

mlongh2<-as.data.frame(mapply(c, allmlong3h2,allmlong4h2, allmlong5h2, allmlongfh2, SIMPLIFY=FALSE))
p20h2<-qplot(depth, data=mlongh2, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(323.6896,0),ylim=c(0,35))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa t1")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p20h2


#haul5

#mapply to combine all stages of species A to one table
cglach5<-as.data.frame(mapply(c, allcglac3h5, allcglac4h5, allcglac5h5, allcglacfh5, SIMPLIFY=FALSE))
#plot all the sages in one freqpoly plot
p18h5<-qplot(depth, data=cglach5, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,55))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p18h5

chyph5<-as.data.frame(mapply(c, allchyp3h5,allchyp4h5, allchyp5h5, allchypfh5, SIMPLIFY=FALSE))
p19h5<-qplot(depth, data=chyph5, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,70))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p19h5

mlongh5<-as.data.frame(mapply(c, allmlong3h5,allmlong4h5, allmlong5h5, allmlongfh5, SIMPLIFY=FALSE))
p20h5<-qplot(depth, data=mlongh5, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(344.9678,0),ylim=c(0,35))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa t2")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p20h5




#haul8

#mapply to combine all stages of species A to one table
cglach8<-as.data.frame(mapply(c, allcglac3h8, allcglac4h8, allcglac5h8, allcglacfh8, SIMPLIFY=FALSE))
#plot all the sages in one freqpoly plot
p18h8<-qplot(depth, data=cglach8, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,55))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p18h8

chyph8<-as.data.frame(mapply(c, allchyp3h8,allchyp4h8, allchyp5h8, allchypfh8, SIMPLIFY=FALSE))
p19h8<-qplot(depth, data=chyph8, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,70))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p19h8

mlongh8<-as.data.frame(mapply(c, allmlong3h8,allmlong4h8, allmlong5h8, allmlongfh8, SIMPLIFY=FALSE))
p20h8<-qplot(depth, data=mlongh8, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(238.7503,0),ylim=c(0,35))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa t3")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p20h8




#haul9

#mapply to combine all stages of species A to one table
cglach9<-as.data.frame(mapply(c, allcglac3h9, allcglac4h9, allcglac5h9, allcglacfh9, SIMPLIFY=FALSE))
#plot all the sages in one freqpoly plot
p18h9<-qplot(depth, data=cglach9, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,55))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p18h9

chyph9<-as.data.frame(mapply(c, allchyp3h9,allchyp4h9, allchyp5h9, allchypfh9, SIMPLIFY=FALSE))
p19h9<-qplot(depth, data=chyph9, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,70),ylim=c(0,70))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p19h9

mlongh9<-as.data.frame(mapply(c, allmlong3h9,allmlong4h9, allmlong5h9, allmlongfh9, SIMPLIFY=FALSE))
p20h9<-qplot(depth, data=mlongh9, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(265.7695,0),ylim=c(0,35))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa t4")+ labs(x = "Depth (m)")+ labs(y = "Count of individuals (n)")
#p20h9

#bw theme
theme_set(theme_bw())
source('multiplot.R')
multiplot(p18h1, p18h2, p18h5, p18h8, p18h9,cols=2)
multiplot(p19h1, p19h2, p19h5, p19h8, p19h9,cols=2)
multiplot(p20h1, p20h2, p20h5, p20h8, p20h9,cols=2)

