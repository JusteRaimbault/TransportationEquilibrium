
setwd(paste0(Sys.getenv('CS_HOME'),'/TransportationEquilibrium/Models/Visu'))

source('functions.R')


db = dbConnect(SQLite(),"../../Data/Sytadin/data/sytadin_20160404.sqlite3")
data = dbGetQuery(db,'SELECT * FROM data LIMIT 200000;')

source('prepareData.R')


###############
## Spatial autocorr

# remote computation
load('res/moran.RData')


## ggplot of results
library(ggplot2)

# first midnight of data : 1454371201 = 02/02/2016 00:00:01

# overall congestion
congdata<-data %>% mutate(mintps=rep(mintps$mintps,nrow(data)/148))%>%mutate(congestion=1 - (mintps / (tps+1)))%>%group_by(ts)%>%summarise(
  mcong=mean(congestion),mincong=mean(congestion)-sd(congestion),maxcong=mean(congestion)+sd(congestion)
)

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

  
g=ggplot(data.frame(dates=dates[200:500],bmean,bmed,bmin,bmax))
g+geom_line(aes(dates,bmean,color="mean"))+geom_line(aes(dates,bmed,color="med")) + theme(axis.text.x = element_text(angle = 90))
#  geom_line(aes(times,bmin,color="min"))+geom_line(aes(times,bmax,color="max"))

g=ggplot(data.frame(dates=dates[199:1000],relvar),aes(dates,relvar))
g+geom_line(colour="lightskyblue3")+stat_smooth(method="loess", span=0.025,n=400,se = FALSE) + theme(axis.text.x = element_text(angle = 90))+
    xlab("")+ylab("∆b / b")



#g=ggplot(data.frame(time=(btimes-btimes[1])/3600,betweenness=betweennesses),aes(x=time,y=betweenness))
#g+geom_point(pch='.')+stat_smooth(method="loess", span=0.02,n=400)


