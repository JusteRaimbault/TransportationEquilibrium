
# functions


getCurrentData<-function(data,times,time){
  rtimes = abs(times-time)
  time = times[which(rtimes==min(rtimes))]
  return(data[data$ts==time,])
}



constructGraph<-function(data,roads,times,time){
  lstrip <- function (x)  sub("^\\s+", "", x)
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
  
  
  edf = data.frame(matrix(nodes,ncol=2,byrow=TRUE),congestion=congestion,length=edgelengths)
  edf[,1]<-as.character(edf[,1]);edf[,2]<-as.character(edf[,2])
  # add missing links due to naming pb
  #  
  edf = correctNetwork(edf,vdf)
  
  #g=graph_from_edgelist(matrix(nodes,ncol=2,byrow=TRUE),directed=TRUE)
  g=graph_from_data_frame(edf,vertices = vdf)
  return(g)
}





correctNetwork <- function(edf,vdf){
  # dirty but no choice than to do it manually
  # shitty encoding also
  
  # 
  edf = addSegment(edf,"PontdeSÃ¨vres","P.Auteuil",0.0,0.5)
  edf = addSegment(edf,"P.Orleans","P.OrlÃ©ans",0.0,0.05)# strange
  
  # add P.Orleans -> Wissous, to be cut as for P.Italie
  edf=rbind(edf,c("P.Orleans","Wissous",edf[which(edf[,1]=="Wissous"&edf[,2]=="P.Orleans")[1],3],9.38489897201865))
  
  # cut rocquencount <-> PAuteil into rocquencourt <-> Vaucresson <-> PAuteuil
  edf=cutSegment(edf,vdf,"Rocquencourt","P.Auteuil","Vaucresson")
  edf=cutSegment(edf,vdf,"Nogent-sur-Marne","CollÃ©gien","Lognes")
  edf=cutSegment(edf,vdf,"P.Italie","Wissous","Fresnes")
  edf=cutSegment(edf,vdf,"P.Orleans","Wissous","Fresnes")
  edf=cutSegment(edf,vdf,"Vitry-sur-Seine","Saint-Maurice","CrÃ©teilPompadour")
  edf=cutSegment(edf,vdf,"Garonor","RoissyCDG","Gonesse")
  
  return(edf)
}


addSegment<-function(edf,o,d,c,l){
  edf = rbind(edf,c(o,d,c,l))
  edf = rbind(edf,c(d,o,c,l))
  return(edf)
}

cutSegment<-function(edf,vdf,e1,e2,en){
  # recompute distances : need vertices coordinates
  d1 = spDists(x=matrix(c(vdf[which(vdf$ID==e1),2:3],vdf[which(vdf$ID==en),2:3]),ncol=2,byrow = TRUE),longlat = TRUE,segments = TRUE)
  d2 = spDists(x=matrix(c(vdf[which(vdf$ID==e2),2:3],vdf[which(vdf$ID==en),2:3]),ncol=2,byrow = TRUE),longlat = TRUE,segments = TRUE)
  
  r1 = which(edf[,1]==e1&edf[,2]==e2)[1]
  r2 = which(edf[,1]==e2&edf[,2]==e1)[1]
  #show(r1);show(r2)
  cong1=edf[r1,3];cong2=edf[r2,3];
  edf[r1,]<- c(e1,en,cong1,d1)
  edf[r2,]<- c(e2,en,cong2,d2)
  edf=rbind(edf,c(en,e2,cong1,d2))
  edf=rbind(edf,c(en,e1,cong2,d1))
  return(edf)
}

