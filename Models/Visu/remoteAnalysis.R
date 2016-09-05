

# remote script to compute everything on the whole db

setwd(paste0(Sys.getenv('CS_HOME'),'/TransportationEquilibrium/Models/Visu'))

source('functions.R')

db = dbConnect(SQLite(),"../../Data/Sytadin/data/sytadin_20160430.sqlite3")
data = dbGetQuery(db,'SELECT * FROM data;')
source('prepareData.R')

#data = data[,2:ncol(data)]
#save(data,file='shiny/data/all.RData')

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
for(i in seq(from = 1440,to=11520,by=5)){#1:length(times)){
  if(i%%100==0){show(paste0('graph : ',i/length(times)));show(dates[i])}
  time=times[i]
  gg=constructGraph(data,roads,times,time)
  # weights are impedances ? YES
  b = betweenness(gg,weights = E(gg)$traveltime,normalized = TRUE)
  
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

load('res/graph.RData')

maxdet=c();maxtime=c();odspace=list();odtime=list();maxalltimes=c()
indexes=seq(from = 1445,to=11520,by=5)
for(ii in 1:length(indexes)){
  i=indexes[ii]
  ds=abs(fulldistances[[i]]$spaceDistances-fulldistances[[i-5]]$spaceDistances)
  dt=abs(fulldistances[[i]]$timeDistances-fulldistances[[i-5]]$timeDistances)
  #show(max(abs(ds)));
  o=which(rowSums(ds==max(ds))==1);d=which(colSums(ds==max(ds))==1)
  #show(V(gg)[o]);show(V(gg)[d])
  maxdet=append(maxdet,max(abs(ds)));maxtime=append(maxtime,abs(fulldistances[[i]]$timeDistances[o,d]-fulldistances[[i-5]]$timeDistances[o,d]))
  maxalltimes=append(maxalltimes,max(dt))
  odspace[[ii]]=c(o,d)
  odtime[[ii]]=c(which(rowSums(dt==max(dt))==1),which(colSums(dt==max(dt))==1))
}

imax = which(maxalltimes==max(maxalltimes))[1]
ismax = which(maxdet==max(maxdet))[1]
maxtime[imax]
indexes[ismax]
maxdet[ismax]

gg=constructGraph(data,roads,times,times[indexes[ismax]])
ggprev=constructGraph(data,roads,times,times[indexes[ismax-1]])
ds = graphDistances(gg)$spaceDistances - graphDistances(ggprev)$spaceDistances
dt = graphDistances(gg)$timeDistances - graphDistances(ggprev)$timeDistances
which(abs(ds)==max(abs(ds))) / 43
ds[3,7]
dt[3,7]
V(ggprev)[3]

shortest_paths(gg,from=V(gg)[3],to=V(gg)[7],output="both",weights = E(gg)$traveltime)
shortest_paths(ggprev,from=V(ggprev)[3],to=V(ggprev)[7],output="both",weights = E(ggprev)$traveltime)

as.POSIXct(times[indexes[ismax-1]], origin="1970-01-01")

# between ggprev : P.Auteuil  P.Maillot  P.Chapelle P.Bagnolet
#   and gg : P.Auteuil         PontdeSÃ¨vres     LePetitClamart   
# Fresnes           Vitry-sur-Seine   CrÃ©teilPompadour
# Saint-Maurice     Nogent-sur-Marne  Rosny-sous-Bois  
# P.Bagnolet 
# -> a huge event should block Paris ring. - +37km detour
# date : 11/02 between 00:06 and 00:16
#  corresponding time variation : only 6min !



#distances(ggprev,weight=E(ggprev)$traveltime)[20,25]
#fulldistances[[indexes[imax]]]$timeDistances[20,25]


###
# variability plots

plot((times[indexes]-times[indexes[1]])/3600,maxalltimes,type='l',xlab="time(h)",ylab="max temporal var (min)",ylim=c(0,30))

g=ggplot(data.frame(dates=dates[indexes],maxalltimes))
g+geom_line(aes(dates,maxalltimes),colour="lightskyblue3")+stat_smooth(method="loess", span=10,n=400,se = FALSE,colour="blue")+ylim(c(0,25)) +xlab("")+ylab("max travel time var (km)") +
  theme(axis.text.x = element_text(size=20,angle = 90), axis.text.y = element_text(size = 20))
   



plot(maxdet,type='l')


## spatial deviation
# Janvry : 8 ; Roissy : 35
plot(unlist(lapply(fulldistances[indexes],function(l){l$spaceDistances[8,35]})),type='l')



