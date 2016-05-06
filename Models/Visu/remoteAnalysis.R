

# remote script to compute everything on the whole db

setwd(paste0(Sys.getenv('CS_HOME'),'/TransportationEquilibrium/Models/Visu'))

source('functions.R')

db = dbConnect(SQLite(),"../../Data/Sytadin/data/sytadin_20160430.sqlite3")
data = dbGetQuery(db,'SELECT * FROM data;')
source('prepareData.R')


##
# autocorrelations

#decays=c(1,2,5,10,20,30,40)

#morantimes=c()
#morandecays=c()
#moran=c()
#moransd=c()

#n=length(roads@lines)
#m = matrix(rep(1,n*n),nrow=n,ncol=n);diag(m)<-0
#for(decay in decays){
#  show(decay)
#  w=weightMatrix(decay)
#  for(i in 1:length(times)){
#    if(i%%1000==0){show(paste0('moran : ',i/length(times)));show(dates[i])}
#    time=times[i]
#    currentData = getCurrentData(data,times,time)
#    
#    tps = sapply(currentData$tps,function(x){max(1,x)})
#    congestion = 1 - (mintps$mintps / tps)
#    # get corresponding congestions
#    congestion = congestion[roads@data$id]
#    
#    rho=autocorr(congestion,w,m)
#    morantimes=append(morantimes,time);morandecays=append(morandecays,decay)
#    moran=append(moran,mean(rho));moransd=append(moransd,sd(rho))
#  }
#}

#moranstats = data.frame(moran,moransd,morandecays,morantimes)

#save(moranstats,file='res/moran.RData')



####
# graph measures



#maxrelvar=c(0);meanrelvar=c(0)
#bsummary=data.frame()
betweennesses=c();btimes=c()
fulldistances=list();
globalgraphmeasures = data.frame()
for(i in 1:length(times)){
  if(i%%1000==0){show(paste0('graph : ',i/length(times)));show(dates[i])}
  time=times[i]
  gg=constructGraph(data,roads,times,time)
  # weights are impedances ? YES
  b = betweenness(gg,weights = traveltime,normalized = TRUE)
  
  #currentDist = distances(gg,weights = (1 - as.numeric(E(gg)$congestion)/2)/as.numeric(E(gg)$length))
  #diag(currentDist)=1
  #if(i>1){
  #  maxrelvar=append(maxrelvar,max(abs(((currentDist-prevDist)/currentDist))))
  #  meanrelvar=append(meanrelvar,mean(abs(((currentDist-prevDist)/currentDist))))
  #}
  #prevDist=currentDist
  
  # diameter computed from distances ; idem for mean distance etc.
  #globalgraphmeasures=rbind(globalgraphmeasures,c(diameter(gg,weights = traveltime)))
  
  fulldistances[[i]] = graphDistances(gg)
  
  # store data
  
  betweennesses=append(betweennesses,b);btimes=append(btimes,rep(time,length(b)))
  # no need to compute summary stats (done later)
  #bsummary=rbind(bsummary,c(mean(b),quantile(b,0.5),min(b),max(b)))
}

betweennessesdf = data.frame(betweennesses,btimes)
save(betweennessesdf,fulldistances,file='res/graph.RData')





