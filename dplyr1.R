library(dplyr)
jdata1_df <- tbl_df(jdata1)
medcal1<-filter(jdata1, jdata1$PREDICTION == "medcala")
subs1<-select(jdata1,PREDICTION,Realdepth,Fluo_A)

pred <- group_by(jdata1_df, PREDICTION)
pred.pos <- summarise(pred,
  count = n(),
  mean.depth = mean(Realdepth, na.rm = TRUE),
  mean.fluo = mean(Fluo_A, na.rm = TRUE))
  
  ggplot(pred.pos, aes(mean.fluo, mean.depth)) +
    geom_point(aes(size = count,colour=PREDICTION), alpha = 1/2) +
    geom_smooth() +
   scale_size_area()

ggplot(jdata1_df,aes(Fluo_A, Realdepth)) + 
  geom_point(aes(colour=PREDICTION), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

