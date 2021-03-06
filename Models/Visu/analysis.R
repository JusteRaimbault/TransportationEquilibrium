
setwd(paste0(Sys.getenv('CS_HOME'),'/TransportationEquilibrium/Models/Visu'))
#setwd('/home/raimbault/ComplexSystems/TransportationEquilibrium/Models/Visu')

source('functions.R')
source(paste0(Sys.getenv('CN_HOME'),'/Models/Utils/R/plots.R'))


#db = dbConnect(SQLite(),"../../Data/Sytadin/data/sytadin_20160703.sqlite3")
db = dbConnect(SQLite(),"../../Data/Data/traffic_20160201-20160301.sqlite3")
data = dbGetQuery(db,'SELECT * FROM data;') #WHERE ts > 1466545462;')

source('prepareData.R')



###############
## Data variability validation
dbgm = dbConnect(SQLite(),"../../Data/GMaps/Validation/gmaps_20160703.sqlite3")
datagmaps = dbGetQuery(dbgm,'SELECT * FROM data;')
reddata = data[data$ts>1466545000,]

relvars = c();timevar = c()
for(i in 1:nrow(datagmaps)){
  if(i%%100==0){show(i)}
  rows = which(abs(datagmaps$ts[i]-reddata$ts)<120&reddata$id==datagmaps$id[i])
  if(length(rows)>0&datagmaps$tps[i]>60){
    t1=datagmaps$tps[i]/60;t2=mean(reddata$tps[rows])
    relvars=append(relvars,2*abs(t1-t2)/(t1+t2))
    timevar = append(timevar,min(abs(datagmaps$ts[i]-data$ts)))
  }
}

#


# overall congestion
congdata<-data %>% mutate(mintps=rep(mintps$mintps,nrow(data)/148))%>%mutate(congestion=1 - (mintps / (tps+1)))%>%group_by(ts)%>%summarise(
  mcong=mean(congestion),mincong=mean(congestion)-sd(congestion),maxcong=mean(congestion)+sd(congestion)
)

reldata<- data %>% mutate(mintps=rep(mintps$mintps,nrow(data)/148))%>%mutate(reltime= (tps+1)/mintps)


#################
## Basic stats on data

library(ggplot2)

sdata = data[data$ts<min(data$ts)+86400*7,] %>% group_by(ts)%>%summarise(avgtime=mean((tps-tps_th)/(1+tps_th)))
g=ggplot(sdata,aes(x=ts,y=avgtime))
g+geom_line()


g=ggplot(congdata[congdata$ts<min(congdata$ts)+86400*14,],aes(x=as.POSIXct(ts,origin="1960-01-01"),y=mcong))
g+geom_line()+
  geom_vline(xintercept=(as.POSIXct(seq(from=1454371200,to=1455553681,by=86400),origin="1960-01-01")),color='red',linetype=2)+
  scale_x_datetime()+
  xlab("time")+ylab("congestion")+stdtheme
ggsave(file = '../../Results/Stats/congestion.png',width=30,height=20,units='cm')


g=ggplot(reldata,aes(x=reltime))
g+geom_histogram(bins = 30)+scale_x_log10()+xlab("log(relative time)")+stdtheme
ggsave(file='../../Results/Stats/reltime.png',width=30,height=20,units='cm')

# same with rank size law
g=ggplot(reldata[reldata$reltime>3,],aes(x=log(1:length(which(reltime>3))),y=sort(log(reltime),decreasing = T)))
g+geom_point(pch='.')+geom_smooth()+xlab("log(rank)")+ylab("log(relative time)")+stdtheme
ggsave(file='../../Results/Stats/reltime_ranksize.png',width=30,height=20,units='cm')


g=ggplot(reldata,aes(x=reltime,colour=as.character(id)))
g+geom_density()+scale_x_log10()+scale_colour_discrete(guide=F)+xlab("log(relative time)")+stdtheme
ggsave(file='../../Results/Stats/reltime_bylink.png',width=30,height=20,units='cm')

g=ggplot(reldata[reldata$reltime>3,],aes(x=log(1:length(which(reltime>3))),y=sort(log(reltime),decreasing = T),colour=as.character(id)))
g+geom_point(pch='.')+geom_smooth()+xlab("log(rank)")+ylab("log(relative time)")



###############
## Spatial autocorr


# remote computation
#load('res/moran.RData')

#sd(autocorr(time,20))

#decays=c(1,2,5,10,20,30,40)
decays = c(1,10)

dtimes=c()
ddecays=c()
moran=c()
moransd=c()

n=length(roads@lines)
m = matrix(rep(1,n*n),nrow=n,ncol=n);diag(m)<-0
for(decay in decays){
  show(decay)
  w=weightMatrix(decay)
  for(i in 1:length(times)){
    if(i%%100==0){show(i)}
    time=times[i]
    rtimes = abs(times-time)
    time = times[which(rtimes==min(rtimes))]
    currentData = data[data$ts==time,]
    tps = sapply(currentData$tps,function(x){max(1,x)})
    congestion = 1 - (mintps$mintps / tps)
    # get corresponding congestions
    congestion = congestion[roads@data$id]
    
    rho=autocorr(congestion,w,m)
    dtimes=append(dtimes,time);ddecays=append(ddecays,decay)
    moran=append(moran,mean(rho));moransd=append(moransd,sd(rho))
  }
}


## ggplot of results
library(ggplot2)

# first midnight of data : 1454371201 = 02/02/2016 00:00:01



decaysplot = c(1,5,10,20)
indexes=which(ddecays%in%decaysplot)
g<- ggplot(data.frame(dtimes=(dtimes[indexes]-dtimes[1])/3600,
                      decay=ddecays[indexes],moran=moran[indexes],
                      mmin=moran[indexes]-moransd[indexes],mmax=moran[indexes]+moransd[indexes],
                      cong=0.005*rep(congdata$mcong,length(decaysplot)),mincong=0.005*rep(congdata$mincong,length(decaysplot)),maxcong=0.005*rep(congdata$maxcong,length(decaysplot))
                      ),
           aes(x=dtimes,y=moran,colour=decay))+geom_point(pch='.')+stat_smooth(method="loess", span=0.05,n=400)+
  geom_vline(xintercept=(seq(from=1454371200,to=1455467281,by=86400)-1454352603)/3600,color='red',linetype=2)+
  geom_line(aes(x=dtimes,y=cong),color="purple")#+geom_ribbon(aes(ymin=mincong,ymax=maxcong),color="purple")
g+facet_wrap(~decay)+xlab("time (h)")
#g+geom_point(pch='.',cex=5)#+stat_smooth()#+geom_ribbon(aes(ymin=mmin,ymax=mmax))

#decay=10
#indexes=which(ddecays==decay)
#g<- ggplot(data.frame(dtimes=dtimes[indexes],ddecays=ddecays[indexes],moran=moran[indexes],mmin=moran[indexes]-moransd[indexes],mmax=moran[indexes]+moransd[indexes]),aes(x=dtimes,y=moran))
#g+geom_point(colour=decay,pch='.')+stat_smooth(colour=decay,method="loess", span=0.05,n=400)#+geom_ribbon(aes(ymin=mmin,ymax=mmax))





#############
##  graph measures

# 
#plot(g,edge.width=edge_betweenness(g,weights = 1 - as.numeric(E(g)$congestion)/2)/10#20*congestion+5
#     ,edge.arrow.mode="-",vertex.size=4)


load('res/graph.RData')
# loads betweennessesdf, fulldistances

  
g=ggplot(data.frame(dates=dates[200:500],bmean,bmed,bmin,bmax))
g+geom_line(aes(dates,bmean,color="mean"))+geom_line(aes(dates,bmed,color="med")) + theme(axis.text.x = element_text(angle = 90))
#  geom_line(aes(times,bmin,color="min"))+geom_line(aes(times,bmax,color="max"))


bdf  =as.tbl(betweennessesdf)%>% group_by(btimes)%>%summarise(bmax=max(abs(betweennesses)))
(bdf[2:nrow(bdf),2]-bdf[1:(nrow(bdf)-1),2])/bdf[1:(nrow(bdf)-1),2]
bdf=data.frame(dates=dates[indexes],relvar=abs(bdf[2:nrow(bdf),2]-bdf[1:(nrow(bdf)-1),2])/bdf[1:(nrow(bdf)-1),2])
g=ggplot(bdf)
g+geom_line(aes_string("dates","bmax"),colour="lightskyblue3")+stat_smooth(se = FALSE) + theme(axis.text.x = element_text(angle = 90,size=20),axis.text.y=element_text(size=20))+
    xlab("time")+ylab("|???b| / b")



#g=ggplot(data.frame(time=(btimes-btimes[1])/3600,betweenness=betweennesses),aes(x=time,y=betweenness))
#g+geom_point(pch='.')+stat_smooth(method="loess", span=0.02,n=400)


