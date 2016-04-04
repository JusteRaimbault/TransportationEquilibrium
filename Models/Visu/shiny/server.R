library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
#set.seed(100)
#zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
#zipdata <- zipdata[order(zipdata$centile),]



shinyServer(function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 2.5, lat = 48.5, zoom = 10)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
#   clustersInBounds <- reactive({
#     if (is.null(input$map_bounds))
#       return(clusterstable[FALSE,])
#     bounds <- input$map_bounds
#     latRng <- range(bounds$north, bounds$south)
#     lngRng <- range(bounds$east, bounds$west)
# 
#     subset(clusterstable,
#       Lat >= latRng[1] & Lat <= latRng[2] &
#         Long >= lngRng[1] & Long <= lngRng[2])
#   })
 
#    isClusterLevel<-reactive({
#      if (is.null(input$map_bounds)){return(TRUE)}
#      bounds <- input$map_bounds
#      boundsall = bbox(france)
#      if((bounds$north-bounds$south)<((boundsall[2,2]-boundsall[2,1])*changeLevelFactor)){
#        return(FALSE)
#      }else{return(TRUE)}
#    })




  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
    #sizeBy <- input$clustersize
    #show(paste0('cluster size by ',sizeBy))
    
    #clusterlevel = isClusterLevel()
    #show(paste0('cluster level : ',clusterlevel))
    
    #show(clusterstable)
    
#     if(clusterlevel==TRUE){
#       clusterColorData <- clusterstable[["activity"]]
#       pal <- colorFactor("Spectral", as.factor(clusterColorData))
#       
#       clusterradius <- clusterstable[[sizeBy]] / max(clusterstable[[sizeBy]]) * 50000
#       #show(clusterradius)
#       #show(clusterstable$long)
#       #show(clusterstable$lat)
#       leafletProxy("map", data = clusterstable) %>%
#         clearShapes() %>%
#         addCircles(~long, ~lat, radius=clusterradius, layerId=~id,
#           stroke=FALSE, fillOpacity=0.4, fillColor=pal(clusterColorData)) %>%
#         addLegend("bottomleft", pal=pal, values=clusterColorData, title="Activity",
#           layerId="colorLegend")
#     }else{
      #radius <- commontable$iscluster * (commontable[[sizeBy]]/ max(clusterstable[[sizeBy]]) * 30000) +
      #   (1-commontable$iscluster )* (commontable$employes / max(commontable$employes) * 50000 )
      #colorData<-commontable$activity
    
     
     # get time
      rtimes = abs(dates-input$time)
      #rtimes = abs(times-min(times))
      time = times[which(rtimes==min(rtimes))]
    
      currentData = data[data$ts==time,]
      tps = sapply(currentData$tps,function(x){max(1,x)})
      
      congestion = 1 - (mintps$mintps / tps)
      
      pal <- colorNumeric(c("green","yellow","red"),domain = congestion[roads@data$id])
      #show(pal)
      #pal <- colorRampPalette(c("green","yellow","red"))
      
      show(pal(congestion[roads@data$id]))
      
      leafletProxy("map", data = data) %>%
        clearShapes() %>%
        #addCircles(~long, ~lat, radius=radius, layerId=~id,
        #           stroke=FALSE, fillOpacity=~(1-iscluster)*0.7+iscluster*0.3, fillColor=pal(colorData)) %>%
        #addLegend("bottomleft", pal=pal, values=colorData, title="Activity",
        #          layerId="colorLegend") %>%
        addPolylines(data = roads,weight=5,color = pal(congestion[roads@data$id]),
                     fillOpacity=0.7
                     )
    #}
  })

  # Show a popup at the given location
#   showZipcodePopup <- function(zipcode, lat, lng) {
#     selectedZip <- allzips[allzips$zipcode == zipcode,]
#     content <- as.character(tagList(
#       tags$h4("Score:", as.integer(selectedZip$centile)),
#       tags$strong(HTML(sprintf("%s, %s %s",
#         selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
#       ))), tags$br(),
#       sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
#       sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
#       sprintf("Adult population: %s", selectedZip$adultpop)
#     ))
#     leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
#   }

  # When map is clicked, show a popup with city info
#   observe({
#     leafletProxy("map") %>% clearPopups()
#     event <- input$map_shape_click
#     if (is.null(event))
#       return()
# 
#     isolate({
#       showZipcodePopup(event$id, event$lat, event$lng)
#     })
#   })


  ## Data Explorer ###########################################
# 
#   observe({
#     cities <- if (is.null(input$states)) character(0) else {
#       filter(cleantable, State %in% input$states) %>%
#         `$`('City') %>%
#         unique() %>%
#         sort()
#     }
#     stillSelected <- isolate(input$cities[input$cities %in% cities])
#     updateSelectInput(session, "cities", choices = cities,
#       selected = stillSelected)
#   })
# 
#   observe({
#     zipcodes <- if (is.null(input$states)) character(0) else {
#       cleantable %>%
#         filter(State %in% input$states,
#           is.null(input$cities) | City %in% input$cities) %>%
#         `$`('Zipcode') %>%
#         unique() %>%
#         sort()
#     }
#     stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
#     updateSelectInput(session, "zipcodes", choices = zipcodes,
#       selected = stillSelected)
#   })
# 
#   observe({
#     if (is.null(input$goto))
#       return()
#     isolate({
#       map <- leafletProxy("map")
#       map %>% clearPopups()
#       dist <- 0.5
#       zip <- input$goto$zip
#       lat <- input$goto$lat
#       lng <- input$goto$lng
#       showZipcodePopup(zip, lat, lng)
#       map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
#     })
#   })
# 
#   output$ziptable <- DT::renderDataTable({
#     df <- cleantable %>%
#       filter(
#         Score >= input$minScore,
#         Score <= input$maxScore,
#         is.null(input$states) | State %in% input$states,
#         is.null(input$cities) | City %in% input$cities,
#         is.null(input$zipcodes) | Zipcode %in% input$zipcodes
#       ) %>%
#       mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
#     action <- DT::dataTableAjax(session, df)
# 
#     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
#   })
})
