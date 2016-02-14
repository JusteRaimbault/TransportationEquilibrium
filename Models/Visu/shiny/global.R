
# global : load data
#  and prepare it as global variables

# setwd(paste0(Sys.getenv('CS_HOME'),'/TransportationEquilibrium/Models/Visu/shiny'))

#library(rgdal)

# load gis files

## -> adapter 'gis' en /path/to/folder/gis OU changer work directory avec setwd(...)
#france <- readOGR('gis','FRANCE')
#departements <- readOGR('gis','DEPARTEMENTS')

#test = SpatialPoints(coords=data.frame(x=c(100217.1,1202417),y=c(6509646.3,7000480)),proj4string=CRS(proj4string(france)))
#plot(france);plot(departements,col='red',add=T);plot(test,col='blue',add=T)

library(RSQLite)
library(dplyr)

db = dbConnect(SQLite(),"../../Test/data/sytadin.sqlite3")
data = dbReadTable(db,'data')
data=as.tbl(data)
data$ts=floor(data$ts)

as.data.frame(unique(data[which(data$ts==1454352603),1]))


