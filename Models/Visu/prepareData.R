
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


# export link coordinates
#linkcoords = data.frame()
#for(i in 1:(length(roads@lines))){
#  lcoords = roads@lines[[i]]@Lines[[1]]@coords
#  linkcoords = rbind(linkcoords,c(roads@data$id[i],lcoords[1,],lcoords[nrow(lcoords),]))
#}

#write.table(linkcoords,file='data/linkcoords.csv',row.names = FALSE,col.names = FALSE,sep=";")

