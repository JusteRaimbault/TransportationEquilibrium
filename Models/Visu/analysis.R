
setwd(paste0(Sys.getenv('CS_HOME'),'/TransportationEquilibrium/Models/Visu'))

library(RSQLite)
library(dplyr)

source('functions.R')


db = dbConnect(SQLite(),"../../Data/Sytadin/data/sytadin_20160404.sqlite3")
#data = dbReadTable(db,'data')
data = dbGetQuery(db,'SELECT * FROM data LIMIT 2000000;')
data=as.tbl(data)
data$ts=floor(data$ts)
# add ids
data$id = ((0:(nrow(data)-1))%%148)+1
# load spatial data
library(rgdal)

roads <- readOGR('shiny/gis','troncons')


times = unique(data$ts)
dates = as.POSIXct(times, origin="1970-01-01")

mintps = data %>% group_by(id) %>% summarise(mintps=max(1,min(tps)))




###############
## Spatial autocorr

library(spdep)

#line=roads@lines[[1]]

centroid<-function(line){
  coords = line@Lines[[1]]@coords;center=colSums(coords)/nrow(coords)
  return(SpatialPoints(coords=data.frame(lat=center[1],lng=center[2]),proj4string=roads@proj4string))
}

# computes 1/d_ij ^ decay as spatial weights
weightMatrix<-function(decay){
  n=length(roads@lines)
  points=matrix(0,n,2)
  for(i in 1:n){
    points[i,]=centroid(roads@lines[[i]])@coords
  }
  d = spDists(SpatialPoints(points,proj4string=roads@proj4string),longlat=TRUE)
  w = exp(-d/decay)
  # replace diag
  diag(w)<-0
  return(w)
}

#weightMatrix(1)*weightMatrix(1)  #test

autocorr<-function(congestion,w,m){
  #n=length(roads@lines)  
  ccong = congestion - mean(congestion) # center
  cong=matrix(ccong,nrow=n,ncol=1)
  normalization = (w%*%matrix(rep(1,n),nrow=n,ncol=1))*(m%*%(cong*cong))
  return(((matrix(data = rep(ccong,n),ncol = n,nrow = n,byrow = FALSE)*w)%*%cong)/normalization)
}

#sd(autocorr(time,20))

decays=c(1,2,5,10,20,30,40)

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
    currentData = getCurrentData(data,times,time)
    
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

library(igraph)
source('functions.R')

# 
#plot(g,edge.width=edge_betweenness(g,weights = 1 - as.numeric(E(g)$congestion)/2)/10#20*congestion+5
#     ,edge.arrow.mode="-",vertex.size=4)


bmean=c();bmed=c();bmin=c();bmax=c();relvar=c(0)
betweennesses=c();btimes=c()
for(i in 200:1000){#length(times)){
  time=times[i]
  show(dates[i])
  gg=constructGraph(data,roads,times,time)
  b = betweenness(gg,weights = (1 - as.numeric(E(gg)$congestion)/2)/as.numeric(E(gg)$length))
  currentDist = distances(gg,weights = (1 - as.numeric(E(gg)$congestion)/2)/as.numeric(E(gg)$length))
  diag(currentDist)=1
  if(i>1){relvar=append(relvar,max(abs(((currentDist-prevDist)/currentDist))))}
  prevDist=currentDist
  #betweennesses=append(betweennesses,b);btimes=append(btimes,rep(time,length(b)))
  bmean=append(bmean,mean(b));bmed=append(bmed,quantile(b,0.5));bmin=append(bmin,min(b));bmax=append(bmax,max(b))
}

  
g=ggplot(data.frame(dates=dates[200:500],bmean,bmed,bmin,bmax))
g+geom_line(aes(dates,bmean,color="mean"))+geom_line(aes(dates,bmed,color="med")) + theme(axis.text.x = element_text(angle = 90))
#  geom_line(aes(times,bmin,color="min"))+geom_line(aes(times,bmax,color="max"))

g=ggplot(data.frame(dates=dates[199:1000],relvar),aes(dates,relvar))
g+geom_line(colour="lightskyblue3")+stat_smooth(method="loess", span=0.025,n=400,se = FALSE) + theme(axis.text.x = element_text(angle = 90))+
    xlab("")+ylab("∆b / b")



#g=ggplot(data.frame(time=(btimes-btimes[1])/3600,betweenness=betweennesses),aes(x=time,y=betweenness))
#g+geom_point(pch='.')+stat_smooth(method="loess", span=0.02,n=400)


