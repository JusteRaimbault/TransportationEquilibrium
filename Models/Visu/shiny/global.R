
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

db = dbConnect(SQLite(),"../../../Data/Sytadin/data/sytadin_20160404.sqlite3")
#data = dbReadTable(db,'data')
data = dbGetQuery(db,"SELECT * FROM data LIMIT 100000");
data=as.tbl(data)
data$ts=floor(data$ts)
# add ids
data$id = ((0:(nrow(data)-1))%%148)+1

alltimes = as.tbl(dbGetQuery(db,"SELECT DISTINCT ts FROM data;"))
alltimes$ts=floor(alltimes$ts)
alltimes = unique(alltimes$ts)
# not optimal -> should keep in cache collected dates
days = format(as.POSIXct(unique(floor(alltimes / 86400))*86400, origin="1970-01-01"),format="%Y-%m-%d")

# load spatial data
library(rgdal)

roads <- readOGR('gis','troncons')


times = unique(data$ts)
dates = as.POSIXct(times, origin="1970-01-01")

mintps = data %>% group_by(id) %>% summarise(mintps=max(1,min(tps)))


#id=55
#plot(unlist(data[which(data$id==id),5]),unlist(data[which(data$id==id),2]),type='l')
#points(unlist(data[which(data$id==id),5]),unlist(data[which(data$id==id),3]),type='l',col="red")



