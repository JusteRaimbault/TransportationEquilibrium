

# remote script to compute everything on the whole db

setwd(paste0(Sys.getenv('CS_HOME'),'/TransportationEquilibrium/Models/Visu'))

library(RSQLite)
library(dplyr)
library(rgdal)

source('functions.R')

db = dbConnect(SQLite(),"../../Data/Sytadin/data/sytadin_20160430.sqlite3")
data = dbGetQuery(db,'SELECT * FROM data;')
data=as.tbl(data)
data$ts=floor(data$ts)
data$id = ((0:(nrow(data)-1))%%148)+1


roads <- readOGR('shiny/gis','troncons')

times = unique(data$ts)
dates = as.POSIXct(times, origin="1970-01-01")

mintps = data %>% group_by(id) %>% summarise(mintps=max(1,min(tps)))


