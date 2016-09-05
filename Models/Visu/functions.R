

library(RSQLite)
library(dplyr)
library(rgdal)
library(igraph)
library(spdep)


# functions


getCurrentData<-function(data,times,time){
  rtimes = abs(times-time)
  time = times[which(rtimes==min(rtimes))]
  return(data[data$ts==time,])
}


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


autocorr<-function(congestion,w,m){
  #n=length(roads@lines)  
  ccong = congestion - mean(congestion) # center
  cong=matrix(ccong,nrow=n,ncol=1)
  normalization = (w%*%matrix(rep(1,n),nrow=n,ncol=1))*(m%*%(cong*cong))
  return(((matrix(data = rep(ccong,n),ncol = n,nrow = n,byrow = FALSE)*w)%*%cong)/normalization)
}



##
# compute shortest paths according to travel time, with corresponding effective geographic distance
#  dirty but O(n3) seems not available in igraph.
graphDistances<-function(gg){
  timeDistances=matrix(0,length(V(gg)),length(V(gg)))
  spaceDistances=matrix(0,length(V(gg)),length(V(gg)))
  for(i in 1:length(V(gg))){
    paths = shortest_paths(gg,from=V(gg)[i],weight=E(gg)$traveltime,output="both")
    for(j in 2:length(paths$vpath)){
      currentvpath=paths$vpath[[j]]
      jj = which(V(gg)$name==currentvpath$name[length(currentvpath)])
      timeDistances[i,jj] = sum(paths$epath[[j]]$traveltime)
      spaceDistances[i,jj] = sum(paths$epath[[j]]$length)
    }
  }
  return(list(timeDistances=timeDistances,spaceDistances=spaceDistances))
}




constructGraph<-function(data,roads,times,time){
  show(length(roads));show(dim(data));show(length(times));show(time)
  lstrip <- function (x)  gsub("^\\t+", "", x)
  #show(length(roads));show(dim(data));show(length(times));show(time)
  #lstrip <- function (x)  gsub("^\\s+", "", x) # change depending UNIX system
  #lstrip <- function (x)  gsub(" ", "", x)
  nodes = sapply(unlist(sapply(data$troncon[roads@data$id],function(s){strsplit(strsplit(lstrip(s),"_")[[1]][1],"=")})),function(s){strsplit(s,"(",fixed=TRUE)[[1]][1]})
  # get coordinates : need extremities of roads
  vdf = data.frame();edgelengths=c()
  for(i in 1:(length(roads@lines))){
    lcoords = roads@lines[[i]]@Lines[[1]]@coords
    vdf = rbind(vdf,lcoords[1,]);vdf = rbind(vdf,lcoords[nrow(lcoords),])
    edgelengths=append(edgelengths,sum(spDists(lcoords,longlat = TRUE,segments = TRUE)))
  }
  vdf = data.frame(nodes,vdf)
  names(vdf)<-c("ID","x","y")
  vdf = vdf[!duplicated(vdf[,1]),]
  
  # get congestion data
  currentData = getCurrentData(data,times,time)
  tps = sapply(currentData$tps,function(x){max(1,x)})
  congestion = 1 - (mintps$mintps / tps)
  # get corresponding congestions
  congestion = congestion[roads@data$id]
  traveltime = as.numeric(tps[roads@data$id])*1.0
  
  edf = data.frame(matrix(nodes,ncol=2,byrow=TRUE),congestion=congestion,length=edgelengths,traveltime=traveltime)
  edf[,1]<-as.character(edf[,1]);edf[,2]<-as.character(edf[,2])
  #edf[,3]<-as.numeric(edf[,3]);edf[,4]<-as.numeric(edf[,4]);edf[,5]<-as.numeric(edf[,5])
  # add missing links due to naming pb
  #  
  #show(as.tbl(edf))
  edf = correctNetwork(edf,vdf)
  
  edf[,3]<-as.numeric(edf[,3]);edf[,4]<-as.numeric(edf[,4]);edf[,5]<-as.numeric(edf[,5])
  #g=graph_from_edgelist(matrix(nodes,ncol=2,byrow=TRUE),directed=TRUE)
  g=graph_from_data_frame(edf,vertices = vdf)
  return(g)
}





correctNetwork <- function(edf,vdf){
  # dirty but no choice than to do it manually
  # shitty encoding also
  
  # 
  edf = addSegment(edf,"PontdeSÃ¨vres","P.Auteuil",0.0,0.5,1)
  edf = addSegment(edf,"P.Orleans","P.OrlÃ©ans",0.0,0.05,0.1)# strange
  
  # add P.Orleans -> Wissous, to be cut as for P.Italie
  edf=rbind(edf,c("P.Orleans","Wissous",edf[which(edf[,1]=="Wissous"&edf[,2]=="P.Orleans")[1],3],9.38489897201865,edf[which(edf[,1]=="Wissous"&edf[,2]=="P.Orleans")[1],5]))
  
  
  # cut rocquencount <-> PAuteil into rocquencourt <-> Vaucresson <-> PAuteuil
  edf=cutSegment(edf,vdf,"Rocquencourt","P.Auteuil","Vaucresson")
  edf=cutSegment(edf,vdf,"Nogent-sur-Marne","CollÃ©gien","Lognes")
  edf=cutSegment(edf,vdf,"P.Italie","Wissous","Fresnes")
  edf=cutSegment(edf,vdf,"P.Orleans","Wissous","Fresnes")
  edf=cutSegment(edf,vdf,"Vitry-sur-Seine","Saint-Maurice","CrÃ©teilPompadour")
  edf=cutSegment(edf,vdf,"Garonor","RoissyCDG","Gonesse")
  
  return(edf)
}


addSegment<-function(edf,o,d,c,l,t){
  edf = rbind(edf,c(o,d,c,l,t))
  edf = rbind(edf,c(d,o,c,l,t))
  return(edf)
}

cutSegment<-function(edf,vdf,e1,e2,en){
 # show(edf[,1])  
# recompute distances : need vertices coordinates
  d1 = spDists(x=matrix(c(vdf[which(vdf$ID==e1),2:3],vdf[which(vdf$ID==en),2:3]),ncol=2,byrow = TRUE),longlat = TRUE,segments = TRUE)
  d2 = spDists(x=matrix(c(vdf[which(vdf$ID==e2),2:3],vdf[which(vdf$ID==en),2:3]),ncol=2,byrow = TRUE),longlat = TRUE,segments = TRUE)
  
  r1 = which(edf[,1]==e1&edf[,2]==e2)[1]
  r2 = which(edf[,1]==e2&edf[,2]==e1)[1]
  d = as.numeric(edf[r1,4])
  #show(r1);show(r2)
  #show(d)
  cong12=edf[r1,3];cong21=edf[r2,3];
  t12=as.numeric(edf[r1,5]);t21=as.numeric(edf[r2,5]);
  edf[r1,]<- c(e1,en,cong12,d1,t12*d1/d)
  edf[r2,]<- c(e2,en,cong21,d2,t21*d2/d)
  edf=rbind(edf,c(en,e2,cong12,d2,t12*d2/d))
  edf=rbind(edf,c(en,e1,cong21,d1,t21*d1/d))
  return(edf)
}

