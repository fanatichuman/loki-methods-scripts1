library(dplyr)
library(gdata)
library(ggplot2)
#theme,get rid of grey background
#theme_set(theme_bw())
data1011 <- file.path("/Volumes/HD/Users/MoritzSchmid-Takuvik/Documents/Work/Projects/LOKI_methods/R/attempt2/101a/combined_101_2,5,8,9_combined_w_spec_5_16_1-9.csv")

# Read in XLS data and rename headers for ease of access
jdata = read.csv(data1011)    # Header line using pattern

jdata1 <- tbl_df(jdata) #more handable table of dplyr

jdata2 = jdata1[c(1:75420), c(1:7,10,41,42,44,45,46,48)] #which columns and rows to take
names(jdata2) = c('overallno','runno','haul', 'prediction16' , 'prediction5', 'length_pred','width_pred','area_mm','depth','salinity','oxy_concentration','temperature(oxy)','oxy_saturation','Fluorescence_high') #header names

haul1<-filter(jdata2,haul==1)
haul2<-filter(jdata2,haul==2)
haul5<-filter(jdata2,haul==5)
haul8<-filter(jdata2,haul==8)
haul9<-filter(jdata2,haul==9)

#sepc16
#HAUL1


#combine all stage subclasses(lat/dors) to one
allcglac4<-filter(haul1,prediction16 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4$pred<-c("Calanus glacialis stage 4")

allcglac5<-filter(haul1,prediction16 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5$pred<-c("Calanus glacialis stage 5")

allcglacf<-filter(haul1,prediction16 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacf$pred<-c("Calanus glacialis female")

allchyp4<-filter(haul1,prediction16 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4$pred<-c("Calanus hyperboreus stage 4")

allchyp5<-filter(haul1,prediction16 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5$pred<-c("Calanus hyperboreus stage 5")

allchypf<-filter(haul1,prediction16 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypf$pred<-c("Calanus hyperboreus female")

allcyctrico<-filter(haul1,prediction16 %in% c("CycTricoDORS" ,"CycTricoLAT","CycTricoMF"))
allcyctrico$pred<-c("Triconia sp.")

alleggs<-filter(haul1,prediction16 %in% c("Egg"))
alleggs$pred<-c("Eggs")

allmicroc<-filter(haul1,prediction16 %in% c("MicroCdors","MicroClat"))
allmicroc$pred<-c("Microcalanus sp.")

allmlong4<-filter(haul1,prediction16 %in% c("MlongaIVlat","MlongIVdors"))
allmlong4$pred<-c("Metridia longa stage 4")

allmlong5<-filter(haul1,prediction16 %in% c("MlongVdors","MlongVlat"))
allmlong5$pred<-c("Metridia longa stage 5")

allmlongf<-filter(haul1,prediction16 %in% c("MlongFdors","MlongFdorsEx","MlongFlat"))
allmlongf$pred<-c("Metridia longa female")

allnauplius<-filter(haul1,prediction16 %in% c("Nauplius"))
allnauplius$pred<-c("Nauplii")

alloithona<-filter(haul1,prediction16 %in% c("Oithona"))
alloithona$pred<-c("Oithona sp.")

allpseudo4<-filter(haul1,prediction16 %in% c("PseudoIVlat"))
allpseudo4$pred<-c("Pseudocalanus sp. stage 4")

allpseudo5<-filter(haul1,prediction16 %in% c("PseudoVdors","PseudoVlat"))
allpseudo5$pred<-c("Pseudocalanus sp. stage 5")

allpseudof<-filter(haul1,prediction16 %in% c("PseudoFdors","PseudoFlat"))
allpseudof$pred<-c("Pseudocalanus sp. female")


#HAUL2


#-------------

#combine all stage subclasses(lat/dors) to one
allcglac4<-filter(haul2,prediction16 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4$pred<-c("Calanus glacialis stage 4")

allcglac5<-filter(haul2,prediction16 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5$pred<-c("Calanus glacialis stage 5")

allcglacf<-filter(haul2,prediction16 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacf$pred<-c("Calanus glacialis female")

allchyp4<-filter(haul2,prediction16 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4$pred<-c("Calanus hyperboreus stage 4")

allchyp5<-filter(haul2,prediction16 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5$pred<-c("Calanus hyperboreus stage 5")

allchypf<-filter(haul2,prediction16 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypf$pred<-c("Calanus hyperboreus female")

allcyctrico<-filter(haul2,prediction16 %in% c("CycTricoDORS" ,"CycTricoLAT","CycTricoMF"))
allcyctrico$pred<-c("Triconia sp.")

alleggs<-filter(haul2,prediction16 %in% c("Egg"))
alleggs$pred<-c("Eggs")

allmicroc<-filter(haul2,prediction16 %in% c("MicroCdors","MicroClat"))
allmicroc$pred<-c("Microcalanus sp.")

allmlong4<-filter(haul2,prediction16 %in% c("MlongaIVlat","MlongIVdors"))
allmlong4$pred<-c("Metridia longa stage 4")

allmlong5<-filter(haul2,prediction16 %in% c("MlongVdors","MlongVlat"))
allmlong5$pred<-c("Metridia longa stage 5")

allmlongf<-filter(haul2,prediction16 %in% c("MlongFdors","MlongFdorsEx","MlongFlat"))
allmlongf$pred<-c("Metridia longa female")

allnauplius<-filter(haul2,prediction16 %in% c("Nauplius"))
allnauplius$pred<-c("Nauplii")

alloithona<-filter(haul2,prediction16 %in% c("Oithona"))
alloithona$pred<-c("Oithona sp.")

allpseudo4<-filter(haul2,prediction16 %in% c("PseudoIVlat"))
allpseudo4$pred<-c("Pseudocalanus sp. stage 4")

allpseudo5<-filter(haul2,prediction16 %in% c("PseudoVdors","PseudoVlat"))
allpseudo5$pred<-c("Pseudocalanus sp. stage 5")

allpseudof<-filter(haul2,prediction16 %in% c("PseudoFdors","PseudoFlat"))
allpseudof$pred<-c("Pseudocalanus sp. female")


#HAUL5
#-------------
#combine all stage subclasses(lat/dors) to one
allcglac4<-filter(haul5,prediction16 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4$pred<-c("Calanus glacialis stage 4")

allcglac5<-filter(haul5,prediction16 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5$pred<-c("Calanus glacialis stage 5")

allcglacf<-filter(haul5,prediction16 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacf$pred<-c("Calanus glacialis female")

allchyp4<-filter(haul5,prediction16 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4$pred<-c("Calanus hyperboreus stage 4")

allchyp5<-filter(haul5,prediction16 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5$pred<-c("Calanus hyperboreus stage 5")

allchypf<-filter(haul5,prediction16 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypf$pred<-c("Calanus hyperboreus female")





#HAUL6
#-------------
#combine all stage subclasses(lat/dors) to one
allcglac4<-filter(haul8,prediction16 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4$pred<-c("Calanus glacialis stage 4")

allcglac5<-filter(haul8,prediction16 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5$pred<-c("Calanus glacialis stage 5")

allcglacf<-filter(haul8,prediction16 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacf$pred<-c("Calanus glacialis female")

allchyp4<-filter(haul8,prediction16 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4$pred<-c("Calanus hyperboreus stage 4")

allchyp5<-filter(haul8,prediction16 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5$pred<-c("Calanus hyperboreus stage 5")

allchypf<-filter(haul8,prediction16 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypf$pred<-c("Calanus hyperboreus female")




#HAUL9
#-------------
#combine all stage subclasses(lat/dors) to one
allcglac4<-filter(haul9,prediction16 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4$pred<-c("Calanus glacialis stage 4")

allcglac5<-filter(haul9,prediction16 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5$pred<-c("Calanus glacialis stage 5")

allcglacf<-filter(haul9,prediction16 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacf$pred<-c("Calanus glacialis female")

allchyp4<-filter(haul9,prediction16 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4$pred<-c("Calanus hyperboreus stage 4")

allchyp5<-filter(haul9,prediction16 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5$pred<-c("Calanus hyperboreus stage 5")

allchypf<-filter(haul9,prediction16 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypf$pred<-c("Calanus hyperboreus female")

#spec5
#HAUL1


#combine all stage subclasses(lat/dors) to one
allcglac4<-filter(haul1,prediction5 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4$pred<-c("Calanus glacialis stage 4")

allcglac5<-filter(haul1,prediction5 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5$pred<-c("Calanus glacialis stage 5")

allcglacf<-filter(haul1,prediction5 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacf$pred<-c("Calanus glacialis female")

allchyp4<-filter(haul1,prediction5 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4$pred<-c("Calanus hyperboreus stage 4")

allchyp5<-filter(haul1,prediction5 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5$pred<-c("Calanus hyperboreus stage 5")

allchypf<-filter(haul1,prediction5 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypf$pred<-c("Calanus hyperboreus female")

allcyctrico<-filter(haul1,prediction5 %in% c("CycTricoDORS" ,"CycTricoLAT","CycTricoMF"))
allcyctrico$pred<-c("Triconia sp.")

alleggs<-filter(haul1,prediction5 %in% c("Egg"))
alleggs$pred<-c("Eggs")

allmicroc<-filter(haul1,prediction5 %in% c("MicroCdors","MicroClat"))
allmicroc$pred<-c("Microcalanus sp.")

allmlong4<-filter(haul1,prediction5 %in% c("MlongaIVlat","MlongIVdors"))
allmlong4$pred<-c("Metridia longa stage 4")

allmlong5<-filter(haul1,prediction5 %in% c("MlongVdors","MlongVlat"))
allmlong5$pred<-c("Metridia longa stage 5")

allmlongf<-filter(haul1,prediction5 %in% c("MlongFdors","MlongFdorsEx","MlongFlat"))
allmlongf$pred<-c("Metridia longa female")

allnauplius<-filter(haul1,prediction5 %in% c("Nauplius"))
allnauplius$pred<-c("Nauplii")

alloithona<-filter(haul1,prediction5 %in% c("Oithona"))
alloithona$pred<-c("Oithona sp.")

allpseudo4<-filter(haul1,prediction5 %in% c("PseudoIVlat"))
allpseudo4$pred<-c("Pseudocalanus sp. stage 4")

allpseudo5<-filter(haul1,prediction5 %in% c("PseudoVdors","PseudoVlat"))
allpseudo5$pred<-c("Pseudocalanus sp. stage 5")

allpseudof<-filter(haul1,prediction5 %in% c("PseudoFdors","PseudoFlat"))
allpseudof$pred<-c("Pseudocalanus sp. female")


#HAUL2


#-------------

#combine all stage subclasses(lat/dors) to one
allcglac4<-filter(haul2,prediction5 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4$pred<-c("Calanus glacialis stage 4")

allcglac5<-filter(haul2,prediction5 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5$pred<-c("Calanus glacialis stage 5")

allcglacf<-filter(haul2,prediction5 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacf$pred<-c("Calanus glacialis female")

allchyp4<-filter(haul2,prediction5 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4$pred<-c("Calanus hyperboreus stage 4")

allchyp5<-filter(haul2,prediction5 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5$pred<-c("Calanus hyperboreus stage 5")

allchypf<-filter(haul2,prediction5 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypf$pred<-c("Calanus hyperboreus female")

allcyctrico<-filter(haul2,prediction5 %in% c("CycTricoDORS" ,"CycTricoLAT","CycTricoMF"))
allcyctrico$pred<-c("Triconia sp.")

alleggs<-filter(haul2,prediction5 %in% c("Egg"))
alleggs$pred<-c("Eggs")

allmicroc<-filter(haul2,prediction5 %in% c("MicroCdors","MicroClat"))
allmicroc$pred<-c("Microcalanus sp.")

allmlong4<-filter(haul2,prediction5 %in% c("MlongaIVlat","MlongIVdors"))
allmlong4$pred<-c("Metridia longa stage 4")

allmlong5<-filter(haul2,prediction5 %in% c("MlongVdors","MlongVlat"))
allmlong5$pred<-c("Metridia longa stage 5")

allmlongf<-filter(haul2,prediction5 %in% c("MlongFdors","MlongFdorsEx","MlongFlat"))
allmlongf$pred<-c("Metridia longa female")

allnauplius<-filter(haul2,prediction5 %in% c("Nauplius"))
allnauplius$pred<-c("Nauplii")

alloithona<-filter(haul2,prediction5 %in% c("Oithona"))
alloithona$pred<-c("Oithona sp.")

allpseudo4<-filter(haul2,prediction5 %in% c("PseudoIVlat"))
allpseudo4$pred<-c("Pseudocalanus sp. stage 4")

allpseudo5<-filter(haul2,prediction5 %in% c("PseudoVdors","PseudoVlat"))
allpseudo5$pred<-c("Pseudocalanus sp. stage 5")

allpseudof<-filter(haul2,prediction5 %in% c("PseudoFdors","PseudoFlat"))
allpseudof$pred<-c("Pseudocalanus sp. female")


#HAUL3
#-------------
#combine all stage subclasses(lat/dors) to one
allcglac4<-filter(haul5,prediction5 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4$pred<-c("Calanus glacialis stage 4")

allcglac5<-filter(haul5,prediction5 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5$pred<-c("Calanus glacialis stage 5")

allcglacf<-filter(haul5,prediction5 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacf$pred<-c("Calanus glacialis female")

allchyp4<-filter(haul5,prediction5 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4$pred<-c("Calanus hyperboreus stage 4")

allchyp5<-filter(haul5,prediction5 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5$pred<-c("Calanus hyperboreus stage 5")

allchypf<-filter(haul5,prediction5 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypf$pred<-c("Calanus hyperboreus female")





#HAUL6
#-------------
#combine all stage subclasses(lat/dors) to one
allcglac4<-filter(haul8,prediction5 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4$pred<-c("Calanus glacialis stage 4")

allcglac5<-filter(haul8,prediction5 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5$pred<-c("Calanus glacialis stage 5")

allcglacf<-filter(haul8,prediction5 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacf$pred<-c("Calanus glacialis female")

allchyp4<-filter(haul8,prediction5 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4$pred<-c("Calanus hyperboreus stage 4")

allchyp5<-filter(haul8,prediction5 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5$pred<-c("Calanus hyperboreus stage 5")

allchypf<-filter(haul8,prediction5 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypf$pred<-c("Calanus hyperboreus female")




#HAUL9
#-------------
#combine all stage subclasses(lat/dors) to one
allcglac4<-filter(haul9,prediction5 %in% c("CglacIVdors","CglacIVlat"))
#add new column pred so they are all called the same
allcglac4$pred<-c("Calanus glacialis stage 4")

allcglac5<-filter(haul9,prediction5 %in% c("CglacVdors","CglacVdorsex","CglacVlat"))
allcglac5$pred<-c("Calanus glacialis stage 5")

allcglacf<-filter(haul9,prediction5 %in% c("CglacFdorsL","CglacFdorsS","CglacFlat"))
allcglacf$pred<-c("Calanus glacialis female")

allchyp4<-filter(haul9,prediction5 %in% c("ChypIVantF_dors" ,"ChypIVdorsEX" ,"ChypIVlat"))
allchyp4$pred<-c("Calanus hyperboreus stage 4")

allchyp5<-filter(haul9,prediction5 %in% c("ChypVdors" ,"ChypVdorsext","ChypVlat"))
allchyp5$pred<-c("Calanus hyperboreus stage 5")

allchypf<-filter(haul9,prediction5 %in% c("ChypFdors" ,"ChypFdorsEXT","ChypFlat"))
allchypf$pred<-c("Calanus hyperboreus female")


#plot all those species/stage combinations
p1<-qplot(depth, data=allcglac4, geom="freqpoly", group=pred, colour="black", position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 4")
p2<-qplot(depth, data=allcglac5, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis stage 5")
p3<-qplot(depth, data=allcglacf, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus glacialis female")
p4<-qplot(depth, data=allchyp4, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 4")
p5<-qplot(depth, data=allchyp5, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus stage 5")
p6<-qplot(depth, data=allchypf, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Calanus hyperboreus female")
p7<-qplot(depth, data=allcyctrico, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Triconia sp.")
p8<-qplot(depth, data=alleggs, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Eggs")
p9<-qplot(depth, data=allmicroc, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Microcalanus sp.")
p10<-qplot(depth, data=allmlong4, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 4")
p11<-qplot(depth, data=allmlong5, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa stage 5")
p12<-qplot(depth, data=allmlongf, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Metridia longa female")
p13<-qplot(depth, data=allnauplius, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Nauplii")
p14<-qplot(depth, data=alloithona, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Oithona sp.")
p15<-qplot(depth, data=allpseudo4, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Pseudocalanus sp. stage 4")
p16<-qplot(depth, data=allpseudo5, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Pseudocalanus sp. stage 5")
p17<-qplot(depth, data=allpseudof, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()+ theme(legend.position = "none")+labs(title = "Pseudocalanus sp. female")

#multiplot version statified by species
source('multiplot.R')
multiplot(p1, p2, p3,cols=2)
multiplot(p4, p5, p6,cols=2)
multiplot(p7, p8, p9,cols=2)
multiplot(p10, p11, p12,cols=2)
multiplot(p15, p16, p17,cols=2)
multiplot(p13, p14,cols=1)


#mapply to combine all stages of species A to one table
cglac<-as.data.frame(mapply(c, allcglac4, allcglac5, allcglacf, SIMPLIFY=FALSE))
#plot all the sages in one freqpoly plot
p18<-qplot(depth, data=cglac, geom="density", group=pred, colour=pred, alpha=0.2, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p18

#bw theme
#set_theme(theme_complete_bw)
#plots cglac with histo and density
cglac$pred2<-as.factor(cglac$pred)
ggplot(cglac, aes(x=depth, fill=pred2)) + geom_bar(aes(y=..density..),binwidth=3, alpha=.5, position="stack")+geom_density(alpha=.2)+xlab("Depth (m)")+ylab("Counts & Density")+coord_flip()+ scale_x_reverse()+theme_bw()

chyp<-as.data.frame(mapply(c, allchyp4, allchyp5, allchypf, SIMPLIFY=FALSE))
p19<-qplot(depth, data=chyp, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=3,xlim=c(315.3574,0))+coord_flip()
p19

mlong<-as.data.frame(mapply(c, allmlong4, allmlong5, allmlongf, SIMPLIFY=FALSE))
p20<-qplot(depth, data=mlong, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()
p20

tricomlong.a<-as.data.frame(mapply(c, allcyctrico, allmlong4, SIMPLIFY=FALSE))
p21<-qplot(depth, data=tricomlong.a, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()
p21

tricomlong.b<-as.data.frame(mapply(c, allcyctrico, allmlong5, SIMPLIFY=FALSE))
p22<-qplot(depth, data=tricomlong.b, geom="freqpoly", group=pred, colour=pred, position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()
p22

tricomlong.c<-as.data.frame(mapply(c, allcyctrico, allmlongf, SIMPLIFY=FALSE))
p23<-qplot(depth, data=tricomlong.c, geom="freqpoly", group=pred, colour=pred,position="identity", binwidth=1,xlim=c(315.3574,0))+coord_flip()
p23



)#plot classes vs env factors and add smooth line
qplot(depth, Fluorescence.0.20.,data=cglac, xlim=c(315.3574,0),colour=pred,geom=c("point","smooth"))+coord_flip()
qplot(depth, salinity,data=cglac, colour=pred, xlim=c(315.3574,0),colour=pred,geom=c("point","smooth"))+coord_flip()
qplot(depth,oxy_concentration,  data=cglac, xlim=c(315.3574,0),colour=pred,geom=c("point","smooth"))+coord_flip()
qplot(depth,temperature.oxy.,  data=cglac, xlim=c(315.3574,0),colour=pred,geom=c("point","smooth"))+coord_flip()
qplot(depth,oxy_saturation,  data=cglac,xlim=c(315.3574,0),colour=pred,geom=c("point","smooth"))+coord_flip()
qplot(depth,area_mm,  data=cglac, xlim=c(315.3574,0),colour=pred,geom=c("point","smooth"))+coord_flip()


#test vs environmental data?
cglac_group <- group_by(cglac, pred)
pred.pos <- summarise(cglac_group,count = n(), mean.depth = mean(depth, na.rm = TRUE), mean.fluo = mean(Fluorescence.0.20.,na.rm = TRUE))
"
ggplot(pred.pos, aes(mean.fluo, mean.depth)) +
  geom_point(aes(size = count,colour=pred), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

ggplot(jdata2,aes(Fluorescence("0-20"), depth)) + 
  geom_point(aes(colour=prediction), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()"