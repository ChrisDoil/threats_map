# This app takes information from the threats csv and plots it into a map

# check if this line is displayed

# packages

if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
# if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

# master-record dataset



if (file.exists('master-record.csv')) {
   master_record <- read.csv(file = 'master-record.csv', sep = ',')
} else {
  master_record <- read.csv(file = 'F:/OneDrive/Documents/Defense_Site_1/DataScience/master-record.csv', sep = ',')
}
  
                  
# master_record$Time = as.POSIXct(master_record$Time, origin = "1970-01-01", tz = "")
master_record$Time <- as.POSIXct(as.numeric(master_record$Time), origin = "1970-01-01", tz = "UTC")

# test_record <- master_record %>% filter(Time == '1568473059.70362')



# icons

if (file.exists('icons/')) {
  icon_path = 'icons/'
} else {
  icon_path = 'F:/OneDrive/ProgrammingPractice/Maki/mapbox-maki-a6d16d4/icons/'
}

danger = makeIcon(paste(icon_path, 'danger-11.svg', sep = ''), paste(icon_path, 'danger-15.svg', sep = ''), 11, 11)  
comm_tower = makeIcon(paste(icon_path, 'communications-tower-11.svg', sep = ''), paste(icon_path, 'communications-tower-15.svg', sep = ''), 11, 11)

if (file.exists('css/styles.css')) {
  css_path = "css/styles.css"
} else {
  css_path = "F:/OneDrive/Documents/Defense_Site_1/site/css/styles.css"
}


## DATA PROCESSING

mr_min_date = as.Date(min(master_record$Time), format = "%Y-%m-%d")
mr_max_date = as.Date(max(master_record$Time), format = "%Y-%m-%d")
unique_targets <- unique(master_record$Target)
 

# dataframe w/ aggregate by location (for circle markers)

mr_1 <- mutate(master_record, u_location = paste(Origin, Country)) %>% group_by(u_location, Latitude, Longitude) %>% summarise(n = n()) %>% arrange(desc(n))

## MAP CREATION

threat_map = leaflet() %>%
  
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE)
                  )
  
  

### SHINY UI ###

ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "Cyber Threats", id = "nav",
             tabPanel("Map",
                      div(class = "outer",
                          tags$head(includeCSS(css_path)),
                          leafletOutput("threatmap", width = "107%", height = "107%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 80, left = 20, width = 250, fixed = TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        ### Date Range Slider ###
                                        
                                        sliderInput("date_range",
                                                    label = "Date Range: ",
                                                    min = mr_min_date,
                                                    max = mr_max_date,
                                                    value = c(mr_min_date, mr_max_date),
                                                    timeFormat = "%d %b %y"
                                                    ),
                                        
                                        selectInput("target_selection",
                                                    label = "Target: ",
                                                    target_selection,
                                                    selected = NULL,
                                                   
                                                   )

                                        
                                        h3(textOutput("clean_date_min"), align = "right"),
                                        h3(textOutput("sample_date"), align = "right")
                                        
                                        # tags$meta(http-equiv ="refresh", content="30")
                                        
                                        
                                        )
                          )
                      )
             
             )
)

server <- function(input, output, session) {
  
   
  
   ### VARIABLE CREATION ###

   
   my_range <- reactive({
     # cbind(input$date_range[0], input$date_range[1])
     input$date_range[1]
   })
   
   output$clean_date_min <- renderText({
     # typeof(input$data_range[1])
     format(as.POSIXct(input$date_range[1])) #, "%d %b %y")
     # as.POSIXct(input$date_range[1])
   })
   
   output$sample_date <- renderText({
     # as.POSIXct(master_record[3, 2], origin = "1970-01-01", tz = "") 

   })
      
   reactive_db = reactive({
     # master_record %>% filter(date >= my_range[0] & date <= my_range[1])
     # reactive = master_record %>% filter(as.Date(Time, format = "%Y-%m-%d") >= as.Date(input$date_range[1], format = "%Y-%m-%d") & as.Date(Time, format = "%Y-%m-%d") <= as.Date(input$date_range[2], format = "%Y-%m-%d"))
     reactive = master_record %>% filter(Time >= as.POSIXct(input$date_range[1]) & Time <= as.POSIXct(input$date_range[2]))

     #  %>% filter(Time == as.POSIXct(input$date_range[1], origin = "1970-01-01", tz = ""))
   })
   

   
   reactive_db_circles = reactive({
     reactive = mutate(reactive_db(), u_location = paste(Origin, Country)) %>% group_by(u_location, Latitude, Longitude) %>% summarise(n = n()) %>% arrange(desc(n))
   })
   
   
   
   output$record_count <- renderText({
     paste0(nrow(reactive_db), " attacks prevented")
   })
   
   
   output$slider_range <- renderText({
     my_range()
   })
   
   points <- eventReactive(input$recalc, {
     cbind(master_record$Longitude, master_record$Latitude)
   }, ignoreNULL = FALSE)
   
  output$threatmap <- renderLeaflet({
    threat_map

   })
  
  observeEvent(input$date_range, leafletProxy("threatmap") %>%
    clearMarkers() %>%
      clearShapes() %>%
      # clearMarkerClusters() %>%
      
      
      addCircleMarkers(data = reactive_db_circles(), lat = ~Latitude, lng = ~Longitude, radius = ~(n) ^ (10/11),
                       fillOpacity = 0.1, weight = 1, color = "#CB1717") %>%
      
      addMarkers(lng = reactive_db()$Longitude, lat = reactive_db()$Latitude, icon = comm_tower,
                 label = sprintf("<strong>%s, %s</strong><br/>Threat IP: %s", reactive_db()$Origin, reactive_db()$Country, reactive_db()$IP.Address) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(opacity = 1, textOnly = TRUE),
                 options = markerOptions(riseOnHover = TRUE),
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                                               function(cluster) {
                                                                return new L.DivIcon({
                                                                  html: '<div style=\"opacity:0\"><span>' +
                                                                          cluster.getChildCount() + '</div><span>',
                                                                          className: 'marker-cluster'
                                                                });      
                                                              }")
                 )
      )
    
  )

}

shinyApp(ui = ui, server = server)


