###### Practcing server.R###
library(htmltools)
library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(shinythemes)
library(leaflet)
library(rsconnect)
library(rgdal)# install.packages('rgdal') # https://cran.r-project.org/web/views/Spatial.html
library(plotly)
DeepFish    <- read_csv("data/deep_water_fish_diversity/fish_diversity.csv")
Mobile      <- read_csv("data/kelp_forest/mobileinvertbrate_diversity_web.csv")
FishDensity <- read_csv("data/kelp_forest/fish_density_web.csv")
Kelpbio     <- read_csv("data/kelp_forest/kelp_biomass_web.csv")

#shp = readOGR('/Users/devinspencer/Downloads/ne_10m_admin_1_states_provinces','ne_10m_admin_1_states_provinces') # slotNames(shp) # summary(shp@data) # View(shp@data)
#shp_ca = subset(shp, name == 'California') # plot(shp_ca)

shinyServer(function(input,output,session) {
  
  ##Define data set to plot - filter first by data set and then filter by site
  get_data <- reactive({
    d = get(input$filter_data)
    d['v'] = d[c('Mobile'='richness', 'FishDensity'='density', 'DeepFish'='richness', 'Kelpbio'='Kelp Biomass (kg)')[input$filter_data]]
    
    ## Plot data set on map 
    output$plot <- renderPlot({
      dataset <- get_data()
      
      # (input$filter_data != "DeepFish")
      plot(dataset$year, dataset$v, xlab = "Year", ylab = "v", frame.plot = "FALSE", col = "darkblue", xlim=c(1994, 2016), ylim=c(0,10))
    })
    
    # if (input$filter_site != "All"){
    #   d <- filter(d, site == input$filter_site)
    # }
    # 
    # return data
    return(d)
  })
  
  ##Generate data table
  output$table <- DT::renderDataTable({
    get_data() %>%
      DT::datatable()
  })
  
  ##Summarize data - total number of records for each site, range of years, average diversity per site over years
  summarize_data <- function(dat) {
    
    dat %>% 
      group_by(site) %>% 
      summarise(
        Longitude      = first(longitude),
        Latitude      = first(latitude),
        FirstYear   = min(year),
        LastYear   = max(year),
        YearRange = LastYear - FirstYear,
        NumberofYears   = n(),
        AverageValue    = mean(v))
    
  }
  
  ##Generate map from summary data above
  output$map <- renderLeaflet({
    
    get_data() %>%
      summarize_data() %>%
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%  ##Esri.NatGeoWorldMap,* Esri.OceanBasemap, Esri.WorldImager
        setView(-119.80,34.2,zoom=9) %>%
        addCircleMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          popup = ~paste(
              strong("Site: "), site, br(),
              strong("No. of records: "), as.character(NumberofYears), br(),
              strong("Avg. value: "), as.character(AverageValue)),
          radius = ~NumberofYears,
          fillOpacity=0.8,
          clusterOptions = markerClusterOptions())
  })
  
  ##Generate summary table
  output$summary <- DT::renderDataTable({
    summarize_data(get_data()) %>%
      DT::datatable()
  })
  
  ##Generate downloadable Data 
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$filter_data, '_', '.csv', sep='')
      #paste(input$filter_data, '.zip', sep='')
    },
    content = function(file) {
      write.csv(data(), file)
      
      # # Trying to write shapefile, but getting error: "Layer creation failed"
      # #file = 'fish.zip'
      # tmp_dir = tempdir() # '/Users/devinspencer/Downloads/tmp_shiny' # chmod -R 777 tmp_shiny
      # writeOGR(shp_ca, tmp_dir, tools::file_path_sans_ext(file), driver='ESRI Shapefile', overwrite_layer=T) # ogrDrivers()
      # zip_inputs = list.files(
      #   path=tmp_dir, full.names = T,
      #   pattern=sprintf('%s\\..*', tools::file_path_sans_ext(file)))
      # zip(file, zip_inputs)
    }
  )
  
})