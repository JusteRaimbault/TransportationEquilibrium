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
      setView(lng = 2.5, lat = 48.7, zoom = 10)
  })

  # get roads in bounds
  roadsInBounds <- reactive({
    if (is.null(input$map_bounds)) return(rep(TRUE,length(roads)))
    bounds <- input$map_bounds
    mapbbox = bbox(SpatialPoints(coords=matrix(c(bounds$west,bounds$north,bounds$west, bounds$south,bounds$east,bounds$north,bounds$east, bounds$south),ncol=2,byrow = TRUE),proj4string = roads@proj4string))
    mapbbox=readWKT(paste0('POLYGON((',bounds$west,' ',bounds$north,',',bounds$east,' ',bounds$north,',',bounds$east,' ',bounds$south,',',bounds$west,' ',bounds$south,',',bounds$west,' ',bounds$north,'))'),p4s = roads@proj4string)
    return(sapply(roads@lines,function(l){gContains(mapbbox,SpatialLines(list(l),proj4string = roads@proj4string))}))
  })
 
  getCurrentData <- reactive({
    # get time
    currentTime=as.numeric(format(as.POSIXct(globalReactives$currentDay),format="%s"))+as.numeric(format(input$time,format="%s"))
    #show(paste0('current time : ',currentTime))
    rtimes = abs(globalReactives$times-currentTime)
    #rtimes = abs(times-min(times))
    time = globalReactives$times[which(rtimes==min(rtimes))]
    
    return(globalReactives$currentDailyData[globalReactives$currentDailyData$ts==time,])
  })


  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
      currentData = getCurrentData()
      tps = sapply(currentData$tps,function(x){max(1,x)})
      
      congestion = 1 - (globalReactives$mintps$mintps / tps)
      
      pal <- colorNumeric(c("green","yellow","red"),domain = congestion[roads@data$id])
      #show(pal)
      #pal <- colorRampPalette(c("green","yellow","red"))
      
      #show(pal(congestion[roads@data$id]))
      
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

  
  # observer to reload data if current day is changed
  observe({
    if(input$day!=globalReactives$currentDay){
       #show("updating daily data")
       globalReactives$currentDay = input$day
       daysts=as.numeric(format(as.POSIXct(globalReactives$currentDay),format="%s"))
       globalReactives$currentDailyData = getData(daysts,daysts+86400)
       globalReactives$times = unique(globalReactives$currentDailyData$ts)
       globalReactives$mintps = globalReactives$currentDailyData %>% group_by(id) %>% summarise(mintps=max(1,min(tps)))
       globalReactives$dates = as.POSIXct(globalReactives$times, origin="1970-01-01")
       #show("done")
    }
  })

  
  # observer to plot congestion on currentDay
  observe({
    #show(roadsInBounds())
    tps = sapply(globalReactives$currentDailyData$tps,function(x){max(1,x)})
    k=length(globalReactives$mintps$mintps)
    congestion = 1 - (rep(globalReactives$mintps$mintps,length(tps)/k) / tps)
    rib = roadsInBounds()
    bindexes = c(sapply(seq(from=0,to=length(tps),by=k),function(x){rep(x,length(which(rib)))}))
    #bindexes=bindexes+rep(roads@data$id[rib],length(bindexes)/length(which(rib)))
    indexes=bindexes+rep(roads@data$id[rib],length(bindexes)/length(which(rib)))
    # what a mess !
    df = data.frame(congestion=congestion[indexes],id=indexes-bindexes,time=as.POSIXct(globalReactives$currentDailyData$ts[indexes], origin="1970-01-01"))
    
    # render the ggplot
    output$dailyCong <- renderPlot({
      g=ggplot(df,aes(x=time,y=congestion))
      #g+geom_line(aes(x=time,y=congestion,colour=id,group=id))
      g+geom_point(pch='.')+geom_smooth(na.rm=TRUE)#+geom_line(aes(x=time,y=congestion,group=id),color="grey")#+stat_smooth(method="loess", span=0.025,n=400)

    })
    
  })
  # 
  # 
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
