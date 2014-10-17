library(gdata)
library(ggplot2)

baysys2012 <- file.path("/Volumes/HD/Users/MoritzSchmid-Takuvik/Documents/Work/PhD/LOKI/BaySys2012/BaySys2012, HudsonBay/data_and_modeling/rdydata/preds/hauloverview/sample.atsushi.xls")

# Read in XLS data and rename headers for ease of access
jdata = read.xls(baysys2012, pattern="CASEID")    # Header line using pattern

jdata1 = jdata[c(1:1000), c(1:51)]
names(jdata1) = c('CASEID','PREDICTION','AREA_PIX','FORM','AREA','CONVEXITY','STRUCTURE','GRAYMEAN','KURTOSIS','SKEWNESS','HUMO1','HUMO2','HUMO3','HUMO4','HUMO5','HUMO6','HUMO7','FOURIER1','FOURIER2','FOURIER3','FOURIER4','FOURIER5','FOURIER6','FOURIER7','FOURIER8','FOURIER9','FOURIER10','INDEX','RUNNUMB','x1','x2','x3','x4','Cruise','Vessel','Station','Year','Month','Day','Hour','Min','Sec','Realdepth','Pressure','Temp','Sal','Cond','Oxygen_conc','Temp_oxy','Oxygen_sat','Fluo_A')

#making subsets for taxa
subset.medcala<-subset(jdata1, PREDICTION=="medcala", select = c(PREDICTION, Realdepth))

#code for single taxa
qplot(Realdepth, data=subset.medcala, geom="freqpoly", group=PREDICTION, colour=PREDICTION, position="identity", binwidth=1,xlim=c(5,0))+coord_flip()

#code for all taxa in column "PREDICTION"
qplot(Realdepth, data=jdata1, geom="freqpoly", group=PREDICTION, colour=PREDICTION, position="identity",binwidth=1,xlim=c(5,0))+coord_flip()