
setwd(paste0(Sys.getenv('CS_HOME'),'/TransportationEquilibrium/Models/Visu'))

library(RSQLite)
library(dplyr)

source('functions.R')


db = dbConnect(SQLite(),"../../Data/Sytadin/data/sytadin_20160404.sqlite3")
#data = dbReadTable(db,'data')
data = dbGetQuery(db,'SELECT * FROM data LIMIT 200000;')
data=as.tbl(data)
data$ts=floor(data$ts)
# add ids
data$id = ((0:(nrow(data)-1))%%148)+1
# load spatial data
library(rgdal)

roads <- readOGR('shiny/gis','troncons')


times = unique(data$ts)

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

lstrip <- function (x)  sub("^\\s+", "", x)
nodes = sapply(unlist(sapply(data$troncon[roads@data$id],function(s){strsplit(strsplit(lstrip(s),"_")[[1]][1],"=")})),function(s){strsplit(s,"(",fixed=TRUE)[[1]][1]})
# get coordinates : need extremities of roads
vdf = data.frame()
for(i in 1:(length(roads@lines))){
  lcoords = roads@lines[[i]]@Lines[[1]]@coords
  vdf = rbind(vdf,lcoords[1,]);vdf = rbind(vdf,lcoords[nrow(lcoords),])
}
vdf = data.frame(nodes,vdf)
names(vdf)<-c("ID","x","y")
vdf = vdf[!duplicated(vdf[,1]),]

edf = data.frame(matrix(nodes,ncol=2,byrow=TRUE),congestion=congestion)
# add missing links due to naming pb
#  
edf = correctNetwork(edf)

#g=graph_from_edgelist(matrix(nodes,ncol=2,byrow=TRUE),directed=TRUE)
g=graph_from_data_frame(edf,vertices = vdf)
betweenness(g)
plot(g,edge.width=20*congestion+5,edge.arrow.mode="-",vertex.size=4)





