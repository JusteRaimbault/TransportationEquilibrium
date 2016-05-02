
# prepare data

data=as.tbl(data)
data$ts=floor(data$ts)
# add ids
data$id = ((0:(nrow(data)-1))%%148)+1
# load spatial data


roads <- readOGR('shiny/gis','troncons')


times = unique(data$ts)
dates = as.POSIXct(times, origin="1970-01-01")

mintps = data %>% group_by(id) %>% summarise(mintps=max(1,min(tps)))

