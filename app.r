library(shiny)
library(googleway)

# This line is necessary for wherever you move the rate_explorer project to
# You can find out where R is actually pointed at any time with getwd()
setwd('//fs-stl-1/DATA/Business Intelligence/Analytics Projects/rate_explorer')

### NEXT STEPS ###
# Hit DW_PA.dbo.TRANSCORE_DATA_RAW to fetch weekly rates on lane
# Use kma_repcity and kma_zip to filter correctly,
# OR use dw2.dbo.ZiptoZoneHighway.  Test for perormance obvi.
kma_repcity <- read.csv('kma_repcity.csv')
kma_zip <- read.csv('kma_zip.csv')

ui <- fluidPage(
  titlePanel("Transcore Rate Explorer"),
  
  sidebarLayout(position = "right",
                sidebarPanel(
                             textInput("from","From ZIP"),
                             textInput("to","To ZIP"),
                             checkboxGroupInput("mode","Mode",choices = list("Van","Reefer","Flatbed"), selected = "Van"),
                             actionButton("fetch", label="Get Rates"),
                             br(),
                             br(),
                             textOutput('miles')
                             ),
                mainPanel(
                  img(src="HubHeader.png"),
                  google_mapOutput("map")
                  )
                )
)

server <- function(input, output) {
  api_key <- 'YOUR API KEY HERE'
  
  output$map <- renderGoogle_map({
    google_map(key = api_key,
               location = c(39.50,-93.35),
               zoom = 4)
  })
  
  orig_geo_fxn <- reactive({
    return(google_geocode(address="US", components = data.frame(component = "postal_code", value = input$from), key=api_key))
  })
  dest_geo_fxn <- reactive({
    return(google_geocode(address="US", components = data.frame(component = "postal_code", value = input$to), key=api_key))
  })

  mode <- reactive({
    modesList <- substr(input$mode,1,1)
    return(modesList)
  })
  observeEvent(input$fetch,{
    orig_geo <- orig_geo_fxn()
    dest_geo <- dest_geo_fxn()
    
    locs <- data.frame(lat= c(orig_geo$results$geometry$location$lat, dest_geo$results$geometry$location$lat),
                       lon= c(orig_geo$results$geometry$location$lng, dest_geo$results$geometry$location$lng),
                       as_long_as_we_are_spelling_it_stupid_then_cullerr= c('green','red')
    )
    
    directions <- google_directions(as.numeric(locs[1,1:2]),as.numeric(locs[2,1:2]),mode='driving',key=api_key, units='imperial')
    output$miles <- renderText({
      paste('Total Distance: ',round(directions$routes$legs[[1]]$distance$value*0.000621371, digits=0),' miles')
    })
  
    google_map_update(map_id = "map") %>%
      clear_markers() %>%
      clear_polylines() %>%
      add_markers(data = locs, colour= 'as_long_as_we_are_spelling_it_stupid_then_cullerr') %>%
      add_polylines(polyline=directions$routes$overview_polyline$points)
    
  })
  

  
}
  
  
shinyApp(ui = ui, server = server)