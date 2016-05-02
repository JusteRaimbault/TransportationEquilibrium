library(shiny)
library(leaflet)


vars=c("congestion","traveltime")


shinyUI(navbarPage("Traffic", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
         width = 330, height = "auto",
# 
         #h2("Date"),
# 
        
        #sliderInput(inputId = "time","Time",min = min(data$ts),max=max(data$ts),value=min(data$ts),step=NULL),
         selectInput("day", "day", days),
         #sliderInput(inputId = "time","Time",min = min(globalReactives$dates),max=max(globalReactives$dates),value=min(globalReactives$dates),step=NULL),
        sliderInput(inputId = "time","Time",timeFormat = "%H:%M",min = min(hours),max=max(hours),value=min(hours),step=NULL),
        selectInput("var", "Variable", vars)
       
         # color only activity
         #selectInput("color", "Color", vars),  
         #selectInput("clustersize", "Cluster Size", clustervars, selected = "Employes")#,
         #selectInput("esize", "Entreprise Size", clustervars, selected = "Employes"),
#         conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
#           # Only prompt for threshold when coloring or sizing by superzip
#           numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
#         ),
# 
#         plotOutput("histCentile", height = 200),
#         plotOutput("scatterCollegeIncome", height = 250)
       ),

      # plot panel
      absolutePanel(id = "plots", class = "panel panel-default", fixed = TRUE,
              draggable = TRUE, top = 400, left = "auto", right = 20, bottom = "auto",
              width = 330, height = "auto",

             plotOutput("dailyCong", height = 200)
      ) 

#       tags$div(id="cite",
#         'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
#       )
    )
  ),

  conditionalPanel("false", icon("crosshair"))
))
