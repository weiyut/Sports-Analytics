library(ggplot2)
library(xts)
#library(dplyr)
library(forecast)
library(cowplot)
library(ggfortify)
library(TSA)

setwd("D:/Desktop/Courses/Data Science/Time Series Analysis/Final/")
stats<-read.csv("MLB.csv",header = T)
lb<-read.csv("Leaderboard.csv",header = T)
year=stats[,2]
hrg=stats[,4]
sog=stats[,5]
rg=stats[,3]
isop=stats[,6]
stats=stats[,-(1:2)]
stats=zoo(stats,year)
tsstats=as.ts(stats)

ggplot()+geom_line(aes(x=year,y=lb$HR))+
  ylab("Home Runs")+xlab("Season")+
  labs(title = "Total Home Runs by Year")+
  geom_point(aes(x=c(2019,2017,2000,1981),y=c(6776,6105,5693,1781)))+
  geom_text(aes(x=c(2019,2017,2000,1981),y=c(6776,6105,5693,1781)),label=c("6776","6105","5693","1781"),vjust=c(-0.7,-0.7,-0.7,1),hjust=c(0,0.7,0,0))+
  scale_x_continuous(breaks=seq(1925, 2020, 15))+
  geom_line(aes(x=1992:2006,y=lb$HR[72:86]),col="red")+
  geom_line(aes(x=2014:2019,y=lb$HR[94:99]),col="red")

ggplot()+geom_line(aes(x=year[1:95],y=hrg[1:95]))+
  ylab("Home Runs per game")+xlab("Season")+
  labs(title = "Home Runs per game by Year(1921-2015)")+
  geom_point(aes(x=c(2019,2017,2000,1981,2014,1992),y=hrg[c(99,97,80,61,94,72)]))+
  geom_text(aes(x=c(2019,2017,2000,1981,2014,1992),y=hrg[c(99,97,80,61,94,72)]),label=hrg[c(99,97,80,61,94,72)],vjust=c(-0.7,-0.7,-0.7,1,1,1),hjust=c(0,0.7,0,0,0,0))+
  scale_x_continuous(breaks=seq(1925, 2020, 15))+
  geom_line(aes(x=1992:2006,y=hrg[72:86]),col="red")+
  geom_line(aes(x=2014:2019,y=hrg[94:99]),col="red")

ggplot()+geom_line(aes(x=year,y=isop))+
  ylab("Isolated Power")+xlab("Season")+
  labs(title = "League's Isolated Power by Year")+
  geom_point(aes(x=c(2019,2017,2000,1981,2014,1992),y=isop[c(99,97,80,61,94,72)]))+
  geom_text(aes(x=c(2019,2017,2000,1981,2014,1992),y=isop[c(99,97,80,61,94,72)]),label=isop[c(99,97,80,61,94,72)],vjust=c(-0.7,-0.7,-0.7,1,1,1),hjust=c(0,0.7,0,0,0,0))+
  scale_x_continuous(breaks=seq(1925, 2020, 15))+
  geom_line(aes(x=1992:2006,y=isop[72:86]),col="red")+
  geom_line(aes(x=2014:2019,y=isop[94:99]),col="red")

ap<-read.csv("SepYear.csv",header = T)
G1014 = ap[ap$yearID<2015,]
G1014 =G1014[G1014$AB>200,]
G1519 = ap[ap$yearID>2015,]
G1519 =G1519[G1519$AB>200,]



compHR = data.frame(
  Seasons = c( rep("2010-2014", dim(G1014)[1]), rep("2015-2019", dim(G1519)[1]) ),
  value = c( G1014$HR, G1519$HR) )
compHR %>%
  ggplot( aes(x=value, fill=Seasons)) +
  geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("red", "cyan")) +
  labs(title="Home runs hit by players")+
  xlab("Home Runs")+ylab("Number of players")+
  geom_vline(xintercept = c(mean(G1519$HR),mean(G1014$HR)), col = c("cyan","red"), linetype = c("longdash","longdash"),size=c(1.2,1.2))+
  scale_x_continuous(breaks=seq(0, 60, 10))
  
compHR %>%
  ggplot( aes(x=value, fill=Seasons)) +
  geom_histogram( aes(y = ..density..),color="#e9ecef", alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("red", "cyan")) +
  labs(title="Density plot of Home runs hit by players")+
  xlab("Home Runs")+ylab("Percentage of players")+
  geom_vline(xintercept = c(median(G1519$HR),median(G1014$HR)), col = c("cyan","red"), linetype = c("longdash","longdash"),size=c(1.2,1.2))+
  scale_x_continuous(breaks=seq(0, 60, 10))
  
  
wilcox.test(G1014$HR,G1519$HR,alternative = "less", paired = FALSE,conf.level = 0.95)

hrfbr=as.numeric(lb$HR.FB[82:99])

ggplot()+
  scale_y_continuous(sec.axis = sec_axis(~.*10,name = "HR/FB (%)"),name = "GB/FB")+
  geom_line(aes(x=2002:2019,y=lb$GB.FB[82:99],col="GB/FB"),size=1.2,show.legend =TRUE)+
  geom_line(aes(x=2002:2019,y=10.5*lb$HR.FB[82:99],col="HR/FB"),size=1.2,show.legend =TRUE)+
  scale_colour_manual(name=" ",
                      values=c("GB/FB"="red","HR/FB"="blue"))+
  xlab("Season")+
  scale_x_continuous(breaks=seq(2002, 2019,1))+
  labs(title = "Ground Ball to Fly Ball ratio/ Home Run to Fly Ball rate by Year")
  
#4yr
ggAcf(hrg[1:95],30,main="ACF of Home Run per game(1921-2015)")
ggPacf(hrg[1:95],30,main="PACF of Home Run per game(1921-2015)")

BoxCox(hrg,BoxCox.lambda(hrg[1:95]))
ggplot()+geom_line(aes(x=year[1:95],y=BoxCox(hrg[1:95],BoxCox.lambda(hrg[1:95]))))+
  ylab("Transformed Data")+xlab("Season")+
  labs(title = "Box-Cox Transformation of Home Runs per game by Year(1921-2015)")

ggplot()+geom_point(aes(x=hrg[1:95],y=BoxCox(hrg[1:95],BoxCox.lambda(hrg[1:95]))))+
  geom_line(aes(x=hrg[1:95],y=hrg[1:95]-1))+xlab("Original Value")+ylab("transformed Value")+labs(title="Transformed values vs Original values")

ggplot()+geom_line(aes(x=year[2:95],y=diff(BoxCox(hrg[1:95],BoxCox.lambda(hrg[1:95])))))+
  ylab("Transformed Data")+xlab("Season")+labs(title = "First Order Differencing")

y=BoxCox(hrg[1:95],BoxCox.lambda(hrg[1:95],'loglik'))
fit1 = arima(y, order=c(1,1,0))
summary(fit1)
ggtsdiag(fit1)

fit1.pred = predict(fit1,n.ahead=4)
r = range(c(fit1.pred$pred+1.96*fit1.pred$se,fit1.pred$pred-1.96*fit1.pred$se,y))
U95 = fit1.pred$pred+1.96*fit1.pred$se; L95 = fit1.pred$pred-1.96*fit1.pred$se 
xx95.1 = c(c(95,time(U95)), rev(c(95,time(U95)))); 
yy95.1 = c(c(y[95],L95), rev(c(y[95],U95))) 
U90 = fit1.pred$pred+1.645*fit1.pred$se; L90 = fit1.pred$pred-1.645*fit1.pred$se
xx90.1 = c(c(95,time(U90)), rev(c(95,time(U90)))); 
yy90.1 = c(c(y[95],L90), rev(c(y[95],U90))) 
U80 = fit1.pred$pred+1.28*fit1.pred$se; L80 = fit1.pred$pred-1.28*fit1.pred$se 
xx80.1 = c(c(95,time(U80)), rev(c(95,time(U80))));
yy80.1 = c(c(y[95],L80), rev(c(y[95],U80))) 


ggplot()+geom_line(aes(x=year,y=c(BoxCox(hrg[1:95],BoxCox.lambda(hrg[1:95],'loglik')),fit1.pred$pred)))+
  geom_line(aes(x=2016:2019,y=fit1.pred$pred),col="blue",size=1.2)+ylim(r)+
  scale_x_continuous(breaks=seq(1925, 2020,15))+
  labs(title = "Box-Cox Transformation of Home Runs per game by Year(1921-2019)")+
  xlab("Season")+
  ylab("Transformed Data")+
  geom_polygon(aes(x=1920+xx95.1,y=yy95.1),col = gray(.6), alpha = .1)+
  geom_polygon(aes(x=1920+xx80.1,y=yy80.1),col = gray(.6), alpha = .3)+
  geom_vline(xintercept = 1920+length(y)-1,linetype="longdash")
  
  
lambda = BoxCox.lambda(hrg[1:95],'loglik')  

p1619<-
  ggplot()+geom_line(aes(x=year[1:99],y=c(hrg[1:95],InvBoxCox(fit1.pred$pred,lambda))))+
  geom_line(aes(x=2015:2019,y=hrg[95:99],col="Actual Values"),size=1.2,show.legend = TRUE)+
  geom_line(aes(x=2015:2019,y=c(hrg[95],InvBoxCox(fit1.pred$pred,lambda)),col="Predicted Values"),size=1.2,show.legend = TRUE)+ylim(0,1.6)+
  scale_x_continuous(breaks=seq(1925, 2020,15))+
  labs(title = "AR(1) Predictions of Home Runs per game by Year(2016-2019)")+
  xlab("Season")+
  ylab("Home Runs per game")+
  geom_polygon(aes(x=1920+xx95.1,y=InvBoxCox(yy95.1,lambda)),col = gray(.6), alpha = .1)+
  geom_polygon(aes(x=1920+xx90.1,y=InvBoxCox(yy90.1,lambda)),col = gray(.6), alpha = .2)+
  geom_polygon(aes(x=1920+xx80.1,y=InvBoxCox(yy80.1,lambda)),col = gray(.6), alpha = .3)+
  geom_vline(xintercept = 1920+length(y),linetype="longdash")+
  geom_point(aes(x=2016:2019,y=InvBoxCox(fit1.pred$pred,lambda)),col="blue",size = 2)+
  geom_point(aes(x=2016:2019,y=hrg[96:99]),col="red",size = 2)+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('Actual Values' = 'red',
                                 'Predicted Values' = 'blue'),
                      labels = c('Actual Values',
                                 'Predicted Values'))+
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(0, 5, 0, 0)
  )
p1619


p1619L<-
  ggplot()+geom_line(aes(x=year[95:99],y=c(hrg[95],InvBoxCox(fit1.pred$pred,lambda))))+
  geom_line(aes(x=2015:2019,y=hrg[95:99],col="Actual Values"),size=1.2,show.legend = TRUE)+
  geom_line(aes(x=2015:2019,y=c(hrg[95],InvBoxCox(fit1.pred$pred,lambda)),col="Predicted Values"),size=1.2,show.legend = TRUE)+ylim(0.65,1.5)+
  scale_x_continuous(breaks=seq(2015, 2019,1))+
  labs(title = "Predictions of Home Runs per game by Year(2016-2019)")+
  xlab("Season")+
  ylab("Home Runs per game")+
  geom_polygon(aes(x=1920+xx95.1,y=InvBoxCox(yy95.1,lambda)),col = gray(.6), alpha = .1)+
  geom_polygon(aes(x=1920+xx90.1,y=InvBoxCox(yy90.1,lambda)),col = gray(.6), alpha = .2)+
  geom_polygon(aes(x=1920+xx80.1,y=InvBoxCox(yy80.1,lambda)),col = gray(.6), alpha = .3)+
  geom_vline(xintercept = 1920+length(y),linetype="longdash")+
  geom_point(aes(x=2016:2019,y=InvBoxCox(fit1.pred$pred,lambda)),col="blue",size = 4)+
  geom_point(aes(x=2016:2019,y=hrg[96:99]),col="red",size = 4)+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('Actual Values' = 'red',
                                 'Predicted Values' = 'blue'),
                      labels = c('Actual Values',
                                 'Predicted Values'))+
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(0, 5, 0, 0)
  )

p1619L
  

#3yr
ggAcf(hrg[1:96],30,main="ACF of Home Run per game(1921-2016)")
ggPacf(hrg[1:96],30,main="PACF of Home Run per game(1921-2016)")


y2=BoxCox(hrg[1:96],BoxCox.lambda(hrg[1:96]))

fit2 = arima(y2, order=c(1,1,0))
summary(fit2)
ggtsdiag(fit2)

fit2.pred = predict(fit2,n.ahead=3)
r2 = range(c(fit2.pred$pred+1.96*fit2.pred$se,fit2.pred$pred-1.96*fit2.pred$se,y2))

U95 = fit2.pred$pred+1.96*fit2.pred$se; L95 = fit2.pred$pred-1.96*fit2.pred$se 
xx95.2 = c(c(96,time(U95)), rev(c(96,time(U95)))); 
yy95.2 = c(c(y2[96],L95), rev(c(y2[96],U95))) 
U90 = fit2.pred$pred+1.645*fit2.pred$se; L90 = fit2.pred$pred-1.645*fit2.pred$se
xx90.2 = c(c(96,time(U90)), rev(c(96,time(U90)))); 
yy90.2 = c(c(y2[96],L90), rev(c(y2[96],U90))) 
U80 = fit2.pred$pred+1.28*fit2.pred$se; L80 = fit2.pred$pred-1.28*fit2.pred$se 
xx80.2 = c(c(96,time(U80)), rev(c(96,time(U80))));
yy80.2 = c(c(y2[96],L80), rev(c(y2[96],U80))) 


lambda2 = BoxCox.lambda(hrg[1:96])


p1719<-
  ggplot()+geom_line(aes(x=year[1:99],y=c(hrg[1:96],InvBoxCox(fit2.pred$pred,lambda2))))+
  geom_line(aes(x=2016:2019,y=hrg[96:99],col="Actual Values"),size=1.2,show.legend = TRUE)+
  geom_line(aes(x=2016:2019,y=c(hrg[96],InvBoxCox(fit2.pred$pred,lambda2)),col="Predicted Values"),size=1.2,show.legend = TRUE)+ylim(0,1.6)+
  scale_x_continuous(breaks=seq(1925, 2020,15))+
  labs(title = "Predictions of Home Runs per game by Year(2017-2019)")+
  xlab("Season")+
  ylab("Home Runs per game")+
  geom_polygon(aes(x=1920+xx95.2,y=InvBoxCox(yy95.2,lambda2)),col = gray(.6), alpha = .1)+
  geom_polygon(aes(x=1920+xx80.2,y=InvBoxCox(yy80.2,lambda2)),col = gray(.6), alpha = .3)+
  geom_vline(xintercept = 1920+length(y2),linetype="longdash")+
  geom_point(aes(x=2017:2019,y=InvBoxCox(fit2.pred$pred,lambda2)),col="blue",size = 2)+
  geom_point(aes(x=2017:2019,y=hrg[97:99]),col="red",size = 2)+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('Actual Values' = 'red',
                                 'Predicted Values' = 'blue'),
                      labels = c('Actual Values',
                                 'Predicted Values'))+
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(0, 5, 0, 0)
  )
p1719

p1719L<-
  ggplot()+geom_line(aes(x=year[96:99],y=c(hrg[96],InvBoxCox(fit2.pred$pred,lambda2))))+
  geom_line(aes(x=2016:2019,y=hrg[96:99],col="Actual Values"),size=1.2,show.legend = TRUE)+
  geom_line(aes(x=2016:2019,y=c(hrg[96],InvBoxCox(fit2.pred$pred,lambda2)),col="Predicted Values"),size=1.2,show.legend = TRUE)+ylim(0.65,1.5)+
  scale_x_continuous(breaks=seq(2016, 2019,1))+
  labs(title = "Predictions of Home Runs per game by Year(2017-2019)")+
  xlab("Season")+
  ylab("Home Runs per game")+
  geom_polygon(aes(x=1920+xx95.2,y=InvBoxCox(yy95.2,lambda2)),col = gray(.6), alpha = .1)+
  geom_polygon(aes(x=1920+xx90.2,y=InvBoxCox(yy90.2,lambda2)),col = gray(.6), alpha = .2)+
  geom_polygon(aes(x=1920+xx80.2,y=InvBoxCox(yy80.2,lambda2)),col = gray(.6), alpha = .3)+
  geom_vline(xintercept = 1920+length(y2),linetype="longdash")+
  geom_point(aes(x=2017:2019,y=InvBoxCox(fit2.pred$pred,lambda2)),col="blue",size = 4)+
  geom_point(aes(x=2017:2019,y=hrg[97:99]),col="red",size = 4)+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('Actual Values' = 'red',
                                 'Predicted Values' = 'blue'),
                      labels = c('Actual Values',
                                 'Predicted Values'))+
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(0, 5, 0, 0)
  )

p1719L

#2yr

ggAcf(hrg[1:97],30,main="ACF of Home Run per game(1921-2017)")
ggPacf(hrg[1:97],30,main="PACF of Home Run per game(1921-2017)")

y3=BoxCox(hrg[1:97],BoxCox.lambda(hrg[1:97]))

fit3 = arima(y3, order=c(1,1,0))
summary(fit3)
tsdiag(fit3)

fit3.pred = predict(fit3,n.ahead=2)
r3 = range(c(fit3.pred$pred+1.96*fit3.pred$se,fit3.pred$pred-1.96*fit3.pred$se,y3))

U95 = fit3.pred$pred+1.96*fit3.pred$se; L95 = fit3.pred$pred-1.96*fit3.pred$se 
xx95.3 = c(c(97,time(U95)), rev(c(97,time(U95)))); 
yy95.3 = c(c(y3[97],L95), rev(c(y3[97],U95)));
U90 = fit3.pred$pred+1.645*fit3.pred$se; L90 = fit3.pred$pred-1.645*fit3.pred$se
xx90.3 = c(c(97,time(U90)), rev(c(97,time(U90)))); 
yy90.3 = c(c(y3[97],L90), rev(c(y3[97],U90))) 
U80 = fit3.pred$pred+1.28*fit3.pred$se; L80 = fit3.pred$pred-1.28*fit3.pred$se 
xx80.3 = c(c(97,time(U80)), rev(c(97,time(U80))));
yy80.3 = c(c(y3[97],L80), rev(c(y3[97],U80))) 


lambda3 = BoxCox.lambda(hrg[1:97])


p1819<-
  ggplot()+geom_line(aes(x=year[1:99],y=c(hrg[1:97],InvBoxCox(fit3.pred$pred,lambda3))))+
  geom_line(aes(x=2017:2019,y=hrg[97:99],col="Actual Values"),size=1.2,show.legend = TRUE)+
  geom_line(aes(x=2017:2019,y=c(hrg[97],InvBoxCox(fit3.pred$pred,lambda3)),col="Predicted Values"),size=1.2,show.legend = TRUE)+ylim(0,1.6)+
  scale_x_continuous(breaks=seq(1925, 2020,15))+
  labs(title = "Predictions of Home Runs per game by Year(2018-2019)")+
  xlab("Season")+
  ylab("Home Runs per game")+
  geom_polygon(aes(x=1920+xx95.3,y=InvBoxCox(yy95.3,lambda3)),col = gray(.6), alpha = .1)+
  geom_polygon(aes(x=1920+xx80.3,y=InvBoxCox(yy80.3,lambda3)),col = gray(.6), alpha = .3)+
  geom_vline(xintercept = 1920+length(y3),linetype="longdash")+
  geom_point(aes(x=2018:2019,y=InvBoxCox(fit3.pred$pred,lambda3)),col="blue",size = 2)+
  geom_point(aes(x=2018:2019,y=hrg[98:99]),col="red",size = 2)+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('Actual Values' = 'red',
                                 'Predicted Values' = 'blue'),
                      labels = c('Actual Values',
                                 'Predicted Values'))+
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(0, 5, 0, 0)
  )
p1819

p1819L<-
  ggplot()+geom_line(aes(x=year[97:99],y=c(hrg[97],InvBoxCox(fit3.pred$pred,lambda3))))+
  geom_line(aes(x=2017:2019,y=hrg[97:99],col="Actual Values"),size=1.2,show.legend = TRUE)+
  geom_line(aes(x=2017:2019,y=c(hrg[97],InvBoxCox(fit3.pred$pred,lambda3)),col="Predicted Values"),size=1.2,show.legend = TRUE)+ylim(1,1.5)+
  scale_x_continuous(breaks=seq(2017, 2019,1))+
  labs(title = "Predictions of Home Runs per game by Year(2018-2019)")+
  xlab("Season")+
  ylab("Home Runs per game")+
  geom_polygon(aes(x=1920+xx95.3,y=InvBoxCox(yy95.3,lambda3)),col = gray(.6), alpha = .1)+
  geom_polygon(aes(x=1920+xx90.3,y=InvBoxCox(yy90.3,lambda3)),col = gray(.6), alpha = .2)+
  geom_polygon(aes(x=1920+xx80.3,y=InvBoxCox(yy80.3,lambda3)),col = gray(.6), alpha = .3)+
  geom_vline(xintercept = 1920+length(y3),linetype="longdash")+
  geom_point(aes(x=2018:2019,y=InvBoxCox(fit3.pred$pred,lambda3)),col="blue",size = 4)+
  geom_point(aes(x=2018:2019,y=hrg[98:99]),col="red",size = 4)+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('Actual Values' = 'red',
                                 'Predicted Values' = 'blue'),
                      labels = c('Actual Values',
                                 'Predicted Values'))+
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(0, 5, 0, 0)
  )

p1819L

#1yr

ggAcf(hrg[1:98],30,main="ACF of Home Run per game(1921-2018)")
ggPacf(hrg[1:98],30,main="PACF of Home Run per game(1921-2018)")

y4=BoxCox(hrg[1:98],BoxCox.lambda(hrg[1:98]))

fit4 = arima(y4, order=c(1,1,0))
summary(fit4)
ggtsdiag(fit4)

fit4.pred = predict(fit4,n.ahead=1)
r4 = range(c(fit4.pred$pred+1.96*fit4.pred$se,fit4.pred$pred-1.96*fit4.pred$se,y4))

U95 = fit4.pred$pred+1.96*fit4.pred$se; L95 = fit4.pred$pred-1.96*fit4.pred$se 
xx95.4 = c(c(98,time(U95)), rev(c(98,time(U95)))); 
yy95.4 = c(c(y4[98],L95), rev(c(y4[98],U95))) 
U90 = fit4.pred$pred+1.645*fit4.pred$se; L90 = fit4.pred$pred-1.645*fit4.pred$se 
xx90.4 = c(c(98,time(U90)), rev(c(98,time(U90)))); 
yy90.4 = c(c(y4[98],L90), rev(c(y4[98],U90))) 
U80 = fit4.pred$pred+1.28*fit4.pred$se; L80 = fit4.pred$pred-1.28*fit4.pred$se 
xx80.4 = c(c(98,time(U80)), rev(c(98,time(U80))));
yy80.4 = c(c(y4[98],L80), rev(c(y4[98],U80))) 


lambda4 = BoxCox.lambda(hrg[1:98])


p19<-
  ggplot()+geom_line(aes(x=year[1:99],y=c(hrg[1:98],InvBoxCox(fit4.pred$pred,lambda4))))+
  geom_line(aes(x=2018:2019,y=hrg[98:99],col="Actual Values"),size=1.2,show.legend = TRUE)+
  geom_line(aes(x=2018:2019,y=c(hrg[98],InvBoxCox(fit4.pred$pred,lambda4)),col="Predicted Values"),size=1.2,show.legend = TRUE)+ylim(0,1.6)+
  scale_x_continuous(breaks=seq(1925, 2020,15))+
  labs(title = "Predictions of Home Runs per game by Year(2019)")+
  xlab("Season")+
  ylab("Home Runs per game")+
  geom_polygon(aes(x=1920+xx95.4,y=InvBoxCox(yy95.4,lambda4)),col = gray(.6), alpha = .1)+
  geom_polygon(aes(x=1920+xx80.4,y=InvBoxCox(yy80.4,lambda4)),col = gray(.6), alpha = .3)+
  geom_vline(xintercept = 1920+length(y4),linetype="longdash")+
  geom_point(aes(x=2019,y=InvBoxCox(fit4.pred$pred,lambda4)),col="blue",size = 2)+
  geom_point(aes(x=2019,y=hrg[99]),col="red",size = 2)+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('Actual Values' = 'red',
                                 'Predicted Values' = 'blue'),
                      labels = c('Actual Values',
                                 'Predicted Values'))+
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(0, 5, 0, 0)
  )
p19

p19L<-
  ggplot()+geom_line(aes(x=year[98:99],y=c(hrg[98],InvBoxCox(fit4.pred$pred,lambda4))))+
  geom_line(aes(x=2018:2019,y=hrg[98:99],col="Actual Values"),size=1.2,show.legend = TRUE)+
  geom_line(aes(x=2018:2019,y=c(hrg[98],InvBoxCox(fit4.pred$pred,lambda4)),col="Predicted Values"),size=1.2,show.legend = TRUE)+ylim(0.8,1.5)+
  scale_x_continuous(breaks=seq(2018, 2019,1))+
  labs(title = "Predictions of Home Runs per game by Year(2019)")+
  xlab("Season")+
  ylab("Home Runs per game")+
  geom_polygon(aes(x=1920+xx95.4,y=InvBoxCox(yy95.4,lambda4)),col = gray(.6), alpha = .1)+
  geom_polygon(aes(x=1920+xx90.4,y=InvBoxCox(yy90.4,lambda4)),col = gray(.6), alpha = .2)+
  geom_polygon(aes(x=1920+xx80.4,y=InvBoxCox(yy80.4,lambda4)),col = gray(.6), alpha = .3)+
  geom_vline(xintercept = 1920+length(y4),linetype="longdash")+
  geom_point(aes(x=2019,y=InvBoxCox(fit4.pred$pred,lambda4)),col="blue",size = 4)+
  geom_point(aes(x=2019,y=hrg[99]),col="red",size = 4)+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('Actual Values' = 'red',
                                 'Predicted Values' = 'blue'),
                      labels = c('Actual Values',
                                 'Predicted Values'))+
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(0, 5, 0, 0)
  )

p19L


plot_grid(p1619L,p1719L,p1819L,p19L,ncol = 2, nrow = 2)
  

#isop

##4yr
ggAcf(isop[1:95],30,main="ACF of Isolated Power(1921-2015)")
ggPacf(isop[1:95],30,main="PACF of Isolated Power(1921-2015)")

BoxCox(isop,BoxCox.lambda(isop[1:95]))
ggplot()+geom_line(aes(x=year[1:95],y=BoxCox(isop[1:95],BoxCox.lambda(isop[1:95]))))+
  ylab("Transformed Data")+xlab("Season")+
  labs(title = "Box-Cox Transformation of Isolated Power by Year")

ggplot()+geom_point(aes(x=isop[1:95],y=BoxCox(isop[1:95],BoxCox.lambda(isop[1:95]))))+
  geom_line(aes(x=isop[1:95],y=isop[1:95]-1))

ggplot()+geom_line(aes(x=year[2:95],y=diff(BoxCox(isop[1:95],BoxCox.lambda(isop[1:95])))))+
  ylab("Transformed Data")+xlab("Season")

y.isop=BoxCox(isop[1:95],BoxCox.lambda(isop[1:95]))

fit1.isop= arima(y.isop, order=c(1,1,0))
summary(fit1.isop)
ggtsdiag(fit1.isop)

fit1.isop.pred = predict(fit1.isop,n.ahead=4)
r = range(c(fit1.isop.pred$pred+1.96*fit1.isop.pred$se,fit1.isop.pred$pred-1.96*fit1.isop.pred$se,y.isop))
U95 = fit1.isop.pred$pred+1.96*fit1.isop.pred$se; L95 = fit1.isop.pred$pred-1.96*fit1.isop.pred$se 
xx95.1.isop = c(c(95,time(U95)), rev(c(95,time(U95)))); 
yy95.1.isop = c(c(y.isop[95],L95), rev(c(y.isop[95],U95))) 
U90 = fit1.isop.pred$pred+1.645*fit1.isop.pred$se; L90 = fit1.isop.pred$pred-1.645*fit1.isop.pred$se 
xx90.1.isop = c(c(95,time(U90)), rev(c(95,time(U90))));
yy90.1.isop = c(c(y.isop[95],L90), rev(c(y.isop[95],U90))) 
U80 = fit1.isop.pred$pred+1.28*fit1.isop.pred$se; L80 = fit1.isop.pred$pred-1.28*fit1.isop.pred$se 
xx80.1.isop = c(c(95,time(U80)), rev(c(95,time(U80))));
yy80.1.isop = c(c(y.isop[95],L80), rev(c(y.isop[95],U80))) 


lambda.isop = BoxCox.lambda(isop[1:95]) 



p1619isopL<-
  ggplot()+geom_line(aes(x=year[95:99],y=c(isop[95],InvBoxCox(fit1.isop.pred$pred,lambda.isop))))+
  geom_line(aes(x=2015:2019,y=isop[95:99],col="Actual Values"),size=1.2,show.legend = TRUE)+
  geom_line(aes(x=2015:2019,y=c(isop[95],InvBoxCox(fit1.isop.pred$pred,lambda.isop)),col="Predicted Values"),size=1.2,show.legend = TRUE)+
  scale_x_continuous(breaks=seq(2015, 2019,1))+
  labs(title = "Predictions of Isolated Power by Year(2016-2019)")+
  xlab("Season")+
  ylab("Isolated Power")+
  geom_polygon(aes(x=1920+xx95.1.isop,y=InvBoxCox(yy95.1.isop,lambda.isop)),col = gray(.6), alpha = .1)+
  geom_polygon(aes(x=1920+xx90.1.isop,y=InvBoxCox(yy90.1.isop,lambda.isop)),col = gray(.6), alpha = .2)+
  geom_polygon(aes(x=1920+xx80.1.isop,y=InvBoxCox(yy80.1.isop,lambda.isop)),col = gray(.6), alpha = .3)+
  geom_vline(xintercept = 1920+length(y.isop),linetype="longdash")+
  geom_point(aes(x=2016:2019,y=InvBoxCox(fit1.isop.pred$pred,lambda.isop)),col="blue",size = 4)+
  geom_point(aes(x=2016:2019,y=isop[96:99]),col="red",size = 4)+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('Actual Values' = 'red',
                                 'Predicted Values' = 'blue'),
                      labels = c('Actual Values',
                                 'Predicted Values'))+
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(0, 5, 0, 0)
  )

p1619isopL

#3yr
 
ggAcf(isop[1:96],30,main="ACF of Isolated Power(1921-2016)")
ggPacf(isop[1:96],30,main="PACF of Isolated Power(1921-2016)")

y2.isop=BoxCox(isop[1:96],BoxCox.lambda(isop[1:96]))

fit2.isop= arima(y2.isop, order=c(2,1,0))
summary(fit2.isop)
ggtsdiag(fit2.isop)

fit2.isop.pred = predict(fit2.isop,n.ahead=3)
r = range(c(fit2.isop.pred$pred+1.96*fit2.isop.pred$se,fit2.isop.pred$pred-1.96*fit2.isop.pred$se,y2.isop))
U95 = fit2.isop.pred$pred+1.96*fit2.isop.pred$se; L95 = fit2.isop.pred$pred-1.96*fit2.isop.pred$se 
xx95.2.isop = c(c(96,time(U95)), rev(c(96,time(U95)))); 
yy95.2.isop = c(c(y2.isop[96],L95), rev(c(y2.isop[96],U95))) 
U90 = fit2.isop.pred$pred+1.645*fit2.isop.pred$se; L90 = fit2.isop.pred$pred-1.645*fit2.isop.pred$se 
xx90.2.isop = c(c(96,time(U90)), rev(c(96,time(U90)))); 
yy90.2.isop = c(c(y2.isop[96],L90), rev(c(y2.isop[96],U90))) 
U80 = fit2.isop.pred$pred+1.28*fit2.isop.pred$se; L80 = fit2.isop.pred$pred-1.28*fit2.isop.pred$se 
xx80.2.isop = c(c(96,time(U80)), rev(c(96,time(U80))));
yy80.2.isop = c(c(y2.isop[96],L80), rev(c(y2.isop[96],U80))) 





lambda2.isop = BoxCox.lambda(isop[1:96]) 


p1719isopL<-
  ggplot()+geom_line(aes(x=year[96:99],y=c(isop[96],InvBoxCox(fit2.isop.pred$pred,lambda2.isop))))+
  geom_line(aes(x=2016:2019,y=isop[96:99],col="Actual Values"),size=1.2,show.legend = TRUE)+
  geom_line(aes(x=2016:2019,y=c(isop[96],InvBoxCox(fit2.isop.pred$pred,lambda2.isop)),col="Predicted Values"),size=1.2,show.legend = TRUE)+
  scale_x_continuous(breaks=seq(2016, 2019,1))+
  labs(title = "Predictions of Isolated Power by Year(2017-2019)")+
  xlab("Season")+
  ylab("Isolated Power")+
  geom_polygon(aes(x=1920+xx95.2.isop,y=InvBoxCox(yy95.2.isop,lambda2.isop)),col = gray(.6), alpha = .1)+
  geom_polygon(aes(x=1920+xx90.2.isop,y=InvBoxCox(yy90.2.isop,lambda2.isop)),col = gray(.6), alpha = .2)+
  geom_polygon(aes(x=1920+xx80.2.isop,y=InvBoxCox(yy80.2.isop,lambda2.isop)),col = gray(.6), alpha = .3)+
  geom_vline(xintercept = 1920+length(y2.isop),linetype="longdash")+
  geom_point(aes(x=2017:2019,y=InvBoxCox(fit2.isop.pred$pred,lambda2.isop)),col="blue",size = 4)+
  geom_point(aes(x=2017:2019,y=isop[97:99]),col="red",size = 4)+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('Actual Values' = 'red',
                                 'Predicted Values' = 'blue'),
                      labels = c('Actual Values',
                                 'Predicted Values'))+
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(0, 5, 0, 0)
  )

p1719isopL
#2yr
ggAcf(isop[1:97],30,main="ACF of Isolated Power(1921-2017)")
ggPacf(isop[1:97],30,main="PACF of Isolated Power(1921-2017)")
y3.isop=BoxCox(isop[1:97],BoxCox.lambda(isop[1:97]))

fit3.isop= arima(y3.isop, order=c(1,1,0))
summary(fit3.isop)
ggtsdiag(fit3.isop)

y3.isop=BoxCox(isop[1:97],BoxCox.lambda(isop[1:97]))

fit3.isop= arima(y3.isop, order=c(2,1,0))
summary(fit3.isop)
ggtsdiag(fit3.isop)

fit3.isop.pred = predict(fit3.isop,n.ahead=2)
r = range(c(fit3.isop.pred$pred+1.96*fit3.isop.pred$se,fit3.isop.pred$pred-1.96*fit3.isop.pred$se,y3.isop))
U95 = fit3.isop.pred$pred+1.96*fit3.isop.pred$se; L95 = fit3.isop.pred$pred-1.96*fit3.isop.pred$se 
xx95.3.isop = c(c(97,time(U95)), rev(c(97,time(U95)))); 
yy95.3.isop = c(c(y3.isop[97],L95), rev(c(y3.isop[97],U95))) 
U90 = fit3.isop.pred$pred+1.645*fit3.isop.pred$se; L90 = fit3.isop.pred$pred-1.645*fit3.isop.pred$se 
xx90.3.isop = c(c(97,time(U90)), rev(c(97,time(U90)))); 
yy90.3.isop = c(c(y3.isop[97],L90), rev(c(y3.isop[97],U90))) 
U80 = fit3.isop.pred$pred+1.28*fit3.isop.pred$se; L80 = fit3.isop.pred$pred-1.28*fit3.isop.pred$se 
xx80.3.isop = c(c(97,time(U80)), rev(c(97,time(U80))));
yy80.3.isop = c(c(y3.isop[97],L80), rev(c(y3.isop[97],U80))) 





lambda3.isop = BoxCox.lambda(isop[1:97]) 

p1819isopL<-
  ggplot()+geom_line(aes(x=year[97:99],y=c(isop[97],InvBoxCox(fit3.isop.pred$pred,lambda3.isop))))+
  geom_line(aes(x=2017:2019,y=isop[97:99],col="Actual Values"),size=1.2,show.legend = TRUE)+
  geom_line(aes(x=2017:2019,y=c(isop[97],InvBoxCox(fit3.isop.pred$pred,lambda3.isop)),col="Predicted Values"),size=1.2,show.legend = TRUE)+
  scale_x_continuous(breaks=seq(2017, 2019,1))+
  labs(title = "Predictions of Isolated Power by Year(2018-2019)")+
  xlab("Season")+
  ylab("Isolated Power")+
  geom_polygon(aes(x=1920+xx95.3.isop,y=InvBoxCox(yy95.3.isop,lambda3.isop)),col = gray(.6), alpha = .1)+
  geom_polygon(aes(x=1920+xx90.3.isop,y=InvBoxCox(yy90.3.isop,lambda3.isop)),col = gray(.6), alpha = .2)+
  geom_polygon(aes(x=1920+xx80.3.isop,y=InvBoxCox(yy80.3.isop,lambda3.isop)),col = gray(.6), alpha = .3)+
  geom_vline(xintercept = 1920+length(y3.isop),linetype="longdash")+
  geom_point(aes(x=2018:2019,y=InvBoxCox(fit3.isop.pred$pred,lambda3.isop)),col="blue",size = 4)+
  geom_point(aes(x=2018:2019,y=isop[98:99]),col="red",size = 4)+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('Actual Values' = 'red',
                                 'Predicted Values' = 'blue'),
                      labels = c('Actual Values',
                                 'Predicted Values'))+
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(0, 5, 0, 0)
  )

p1819isopL

#1yr
ggAcf(isop[1:98],30,main="ACF of Isolated Power(1921-2018)")
ggPacf(isop[1:98],30,main="PACF of Isolated Power(1921-2018)")

y4.isop=BoxCox(isop[1:98],BoxCox.lambda(isop[1:98]))
fit4.isop= arima(y4.isop, order=c(2,1,0))
summary(fit4.isop)
ggtsdiag(fit4.isop)
fit4.isop.pred = predict(fit4.isop,n.ahead=1)
r = range(c(fit4.isop.pred$pred+1.96*fit4.isop.pred$se,fit4.isop.pred$pred-1.96*fit4.isop.pred$se,y4.isop))
U95 = fit4.isop.pred$pred+1.96*fit4.isop.pred$se; L95 = fit4.isop.pred$pred-1.96*fit4.isop.pred$se 
xx95.4.isop = c(c(98,time(U95)), rev(c(98,time(U95)))); 
yy95.4.isop = c(c(y4.isop[98],L95), rev(c(y4.isop[98],U95))) 
U90 = fit4.isop.pred$pred+1.645*fit4.isop.pred$se; L90 = fit4.isop.pred$pred-1.645*fit4.isop.pred$se 
xx90.4.isop = c(c(98,time(U90)), rev(c(98,time(U90)))); 
yy90.4.isop = c(c(y4.isop[98],L90), rev(c(y4.isop[98],U90))) 
U80 = fit4.isop.pred$pred+1.28*fit4.isop.pred$se; L80 = fit4.isop.pred$pred-1.28*fit4.isop.pred$se 
xx80.4.isop = c(c(98,time(U80)), rev(c(98,time(U80))));
yy80.4.isop = c(c(y4.isop[98],L80), rev(c(y4.isop[98],U80))) 
lambda4.isop = BoxCox.lambda(isop[1:98]) 

p19isopL<-
  ggplot()+geom_line(aes(x=year[99],y=c(isop[98],InvBoxCox(fit4.isop.pred$pred,lambda4.isop))))+
  geom_line(aes(x=2018:2019,y=isop[98:99],col="Actual Values"),size=1.2,show.legend = TRUE)+
  geom_line(aes(x=2018:2019,y=c(isop[98],InvBoxCox(fit4.isop.pred$pred,lambda4.isop)),col="Predicted Values"),size=1.2,show.legend = TRUE)+
  scale_x_continuous(breaks=seq(2018, 2019,1))+
  labs(title = "Predictions of Isolated Power by Year(2019)")+
  xlab("Season")+
  ylab("Isolated Power")+
  geom_polygon(aes(x=1920+xx95.4.isop,y=InvBoxCox(yy95.4.isop,lambda4.isop)),col = gray(.6), alpha = .1)+
  geom_polygon(aes(x=1920+xx90.4.isop,y=InvBoxCox(yy90.4.isop,lambda4.isop)),col = gray(.6), alpha = .2)+
  geom_polygon(aes(x=1920+xx80.4.isop,y=InvBoxCox(yy80.4.isop,lambda4.isop)),col = gray(.6), alpha = .3)+
  geom_vline(xintercept = 1920+length(y4.isop),linetype="longdash")+
  geom_point(aes(x=2019,y=InvBoxCox(fit4.isop.pred$pred,lambda4.isop)),col="blue",size = 4)+
  geom_point(aes(x=2019,y=isop[99]),col="red",size = 4)+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('Actual Values' = 'red',
                                 'Predicted Values' = 'blue'),
                      labels = c('Actual Values',
                                 'Predicted Values'))+
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(0, 5, 0, 0)
  )


p19isopL

plot_grid(p1619isopL,p1719isopL,p1819isopL,p19isopL,ncol = 2, nrow = 2)

#intervention analysis

library("TSA")
#Home Runs per game
ggplot()+geom_line(aes(x=year,y=hrg))+
  ylab("Home Runs per game")+xlab("Season")+
  labs(title = "Home Runs per game by Year(1921-2019)")+
  geom_point(aes(x=c(1992,2000,2007),y=hrg[c(72,80,87)]),col="red")+
  geom_text(aes(x=c(1992,2000,2007),y=hrg[c(72,80,87)]),label=year[c(72,80,87)],vjust=c(-0.7,-0.7,-0.8),hjust=c(0,0,0))+
  scale_x_continuous(breaks=seq(1920, 2020, 10))+
  geom_line(aes(x=1992:2007,y=hrg[72:87]),col="red")
yi = BoxCox(hrg,BoxCox.lambda(hrg))
ggtsdisplay(yi)
s9207 <- c(rep(0,72),rep(1,15),rep(0,12))
steroids=arimax(yi,order=c(1,1,0),xtransf=data.frame(s9207),transfer=list(c(2,0)))
summary(steroids)
ggtsdiag(steroids)

yii = BoxCox(hrg[1:72],BoxCox.lambda(hrg[1:72]))
pres = arima(yii,order=c(1,1,0))
summary(pres)
ggtsdiag(pres)
pres.pred = predict(pres,n.ahead = 15)

U95 = pres.pred$pred+1.96*pres.pred$se; L95 = pres.pred$pred-1.96*pres.pred$se 
xx95.pres = c(c(72,time(U95)), rev(c(72,time(U95)))); 
yy95.pres = c(c(yii[72],L95), rev(c(yii[72],U95))) 
U90 = pres.pred$pred+1.645*pres.pred$se; L90 = pres.pred$pred-1.645*pres.pred$se 
xx90.pres = c(c(72,time(U90)), rev(c(72,time(U90)))); 
yy90.pres = c(c(yii[72],L90), rev(c(yii[72],U90))) 
U80 = pres.pred$pred+1.28*pres.pred$se; L80 = pres.pred$pred-1.28*pres.pred$se 
xx80.pres = c(c(72,time(U80)), rev(c(72,time(U80)))); 
yy80.pres = c(c(yii[72],L80), rev(c(yii[72],U80))) 

p9207<-
  ggplot()+geom_line(aes(x=year[1:87],y=c(hrg[1:72],InvBoxCox(pres.pred$pred,BoxCox.lambda(hrg[1:72])))))+
  geom_line(aes(x=1992:2007,y=hrg[72:87],col="Actual Values"),size=1.2,show.legend = TRUE)+
  geom_line(aes(x=1992:2007,y=c(hrg[72],InvBoxCox(pres.pred$pred,BoxCox.lambda(hrg[1:72]))),col="Predicted Values"),size=1.2,show.legend = TRUE)+
  scale_x_continuous(breaks=seq(1920, 2007,10))+
  labs(title = "Predictions of Home Runs per game in Steroids Era(1992-2007)")+
  xlab("Season")+
  ylab("Home Runs per game")+
  geom_polygon(aes(x=1920+xx95.pres,y=InvBoxCox(yy95.pres,BoxCox.lambda(hrg[1:72]))),col = gray(.6), alpha = .1)+
  geom_polygon(aes(x=1920+xx90.pres,y=InvBoxCox(yy90.pres,BoxCox.lambda(hrg[1:72]))),col = gray(.6), alpha = .2)+
  geom_polygon(aes(x=1920+xx80.pres,y=InvBoxCox(yy80.pres,BoxCox.lambda(hrg[1:72]))),col = gray(.6), alpha = .3)+
  geom_vline(xintercept = 1920+length(yii),linetype="longdash")+
  geom_point(aes(x=1993:2007,y=InvBoxCox(pres.pred$pred,BoxCox.lambda(hrg[1:72]))),col="blue",size = 2)+
  geom_point(aes(x=1993:2007,y=hrg[73:87]),col="red",size = 2)+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('Actual Values' = 'red',
                                 'Predicted Values' = 'blue'),
                      labels = c('Actual Values',
                                 'Predicted Values'))+
  theme(legend.position = c(0, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(0, 5, 0, 0)
  )
p9207

st <- steroids$coef["s9207-MA0"]*s9207 + steroids$coef["s9207-MA0"]*filter(s9207, filter=steroids$coef["s9207-AR1"]+steroids$coef["s9207-AR2"], method="recursive")
ggplot()+geom_line(aes(x=year,y=st))+
  labs(title="Estimated Effect of Juicing in Home Runs production per game")+
  scale_x_continuous(breaks=seq(1920, 2020,5))+
  xlab("Season")+
  ylab("")
  





