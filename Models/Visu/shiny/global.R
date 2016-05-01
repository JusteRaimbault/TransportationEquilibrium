
# global : load data
#  and prepare it as global variables

# setwd(paste0(Sys.getenv('CS_HOME'),'/TransportationEquilibrium/Models/Visu/shiny'))


## -> adapter 'gis' en /path/to/folder/gis OU changer work directory avec setwd(...)
#france <- readOGR('gis','FRANCE')
#departements <- readOGR('gis','DEPARTEMENTS')

#test = SpatialPoints(coords=data.frame(x=c(100217.1,1202417),y=c(6509646.3,7000480)),proj4string=CRS(proj4string(france)))
#plot(france);plot(departements,col='red',add=T);plot(test,col='blue',add=T)

library(RSQLite)
library(dplyr)

#db = dbConnect(SQLite(),"../../../Data/Sytadin/data/sytadin_20160430.sqlite3")
#data = dbReadTable(db,'data')

# issue in querying sqlite : load RData as a provisory solution
load('data/february.RData')


getData <- function(daysts,dayfts){
  return(data[data$ts>daysts&data$ts<dayfts,])
}



#alltimes = as.tbl(dbGetQuery(db,"SELECT DISTINCT ts FROM data;"))
#alltimes$ts=floor(alltimes$ts)
#alltimes = unique(alltimes$ts)
# not optimal -> should keep in cache collected dates
alldays = seq(from=1454367600,to=1456614000,by=86400)
days = format(as.POSIXct(unique(floor(alldays / 86400))*86400, origin="1970-01-01"),format="%Y-%m-%d")
hours = as.POSIXct(seq(from=1,to=86400,by=120),origin="1970-01-01")

globalReactives = reactiveValues()

globalReactives$currentDay = days[1]
globalReactives$currentDailyData = getData(alldays[1],alldays[2])
globalReactives$times = unique(currentDailyData$ts)
globalReactives$mintps = currentDailyData %>% group_by(id) %>% summarise(mintps=max(1,min(tps)))
globalReactives$dates = as.POSIXct(unique(currentDailyData$ts), origin="1970-01-01")

# load spatial data
library(rgdal)

roads <- readOGR('gis','troncons')




#times = unique(data$ts)
#dates = as.POSIXct(times, origin="1970-01-01")
#mintps = data %>% group_by(id) %>% summarise(mintps=max(1,min(tps)))


#id=55
#plot(unlist(data[which(data$id==id),5]),unlist(data[which(data$id==id),2]),type='l')
#points(unlist(data[which(data$id==id),5]),unlist(data[which(data$id==id),3]),type='l',col="red")



