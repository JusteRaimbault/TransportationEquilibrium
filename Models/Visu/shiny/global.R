
# global : load data
#  and prepare it as global variables

# setwd(paste0(Sys.getenv('CS_HOME'),'/TransportationEquilibrium/Models/Visu/shiny'))


## -> adapter 'gis' en /path/to/folder/gis OU changer work directory avec setwd(...)

library(RSQLite)
library(dplyr)
library(rgeos)
library(ggplot2)

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

# begins at Feb. 3 (missing data on 2)
alldays = seq(from=1454540400,to=1456614000,by=86400)
days = format(as.POSIXct(unique(floor(alldays / 86400))*86400, origin="1970-01-01"),format="%Y-%m-%d")
hours = as.POSIXct(seq(from=1,to=86400,by=120),origin="1970-01-01")

globalReactives = reactiveValues()

globalReactives$currentDay = days[1]
currentDailyData = getData(alldays[1],alldays[2]) # needed for startup as can not use reactive already. 
globalReactives$currentDailyData = currentDailyData
globalReactives$times = unique(currentDailyData$ts)
globalReactives$mintps = currentDailyData %>% group_by(id) %>% summarise(mintps=max(1,min(tps)))
globalReactives$dates = as.POSIXct(unique(currentDailyData$ts), origin="1970-01-01")

# load spatial data
library(rgdal)

roads <- readOGR('gis','troncons')

# get bboxes for each segment
lapply(roads@lines,function(l){bbox(l@Lines[[1]])})




