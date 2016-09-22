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

DeepFish    <- read_csv("data/deep_water_fish_diversity/fish_diversity.csv")
Mobile      <- read_csv("data/kelp_forest/mobileinvertbrate_diversity_web.csv")
FishDensity <- read_csv("data/kelp_forest/fish_density_web.csv")
Kelpbio     <- read_csv("data/kelp_forest/kelp_biomass_web.csv")
var_names   <- read_csv("data/var_names.csv")
dataset_v   <- c('Mobile'='richness', 'FishDensity'='density', 'DeepFish'='richness', 'Kelpbio'='kelp_biomass_kg')
#shp = readOGR('/Users/devinspencer/Downloads/ne_10m_admin_1_states_provinces','ne_10m_admin_1_states_provinces') # slotNames(shp) # summary(shp@data) # View(shp@data)
#shp_ca = subset(shp, name == 'California') # plot(shp_ca)

shinyServer(function(input,output,session) {
  
  ##Define data set to plot - filter first by data set and then filter by site
  get_data <- reactive({
    d = get(input$filter_data)
    d['v'] = d[dataset_v[input$filter_data]]
    
    ## Plot data set on map 
    output$plot <- renderPlot({
      dataset <- get_data()
      
      # (input$filter_data != "DeepFish")
      plot(
        dataset$year, dataset$v, 
        xlab = "Year", ylab = dataset_v[input$filter_data], 
        frame.plot = "FALSE", col = "darkblue")
    })
    
    # if (input$filter_site != "All"){
    #   d <- filter(d, site == input$filter_site)
    # }
    # 
    # return data
    return(d)
  })
  
  # rename columns based on var_names.csv
  rename_vars = function(d){
    for (v in var_names$var){
      if (v %in% names(d)){
        names(d)[names(d)==v] = var_names %>% filter(var==v) %>% .$name
      }
    }
    return(d)
  }
  
  ##Generate data table
  output$table <- DT::renderDataTable({
    get_data() %>%
      rename_vars() %>%
      DT::datatable()
  })
  
  ##Summarize data - total number of records for each site, range of years, average diversity per site over years
  summarize_data <- function(dat) {
    
    dat %>% 
      filter(
        !is.na(longitude),
        !is.na(latitude)) %>%
      group_by(site) %>% 
      summarise(
        Longitude     = first(longitude),
        Latitude      = first(latitude),
        FirstYear     = min(year),
        LastYear      = max(year),
        YearRange     = LastYear - FirstYear,
        NumberofYears = n(),
        AverageValue  = mean(v))
    
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
          # radius = ~NumberofYears,
          fillOpacity=0.8,
          clusterOptions = markerClusterOptions())
  })
  
  ##Generate summary table
  output$summary <- DT::renderDataTable({
    get_data() %>%
      summarize_data() %>%
      #rename_vars() %>%
      DT::datatable()
  })
  
  ##Generate downloadable Data 
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$filter_data, '_', '.csv', sep='')
      #paste(input$filter_data, '.zip', sep='')
    },
    content = function(file) {
      write.csv(get_data(), file)
      
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