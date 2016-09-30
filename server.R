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
local_names <- read_csv('data/local_names.csv')
dataset_v   <- c('Mobile'='richness', 'FishDensity'='density', 'DeepFish'='richness', 'Kelpbio'='kelp_biomass_kg')

shinyServer(function(input,output,session) {
  
  ##Define data set to plot - filter first by data set and then filter by site
  
  ###########http://stackoverflow.com/questions/28379937/change-selectize-choices-but-retain-previously-selected-values#########
  get_data <- reactive({
    d = get(input$sel_dataset)
    d['v'] = d[dataset_v[input$sel_dataset]]
    # return data
    return(d)
  })
  
 #selectInput(
 #   "sel_location",
 #   label = div (em("Choose a location:")),
 #   choices = list(
 #     "All"                   = 'all',
 #     "Anacapa Island"        = 'anacapa_island',
 #     "Santa Barbara Island"  = 'santa_barbara_island',
 #     "San Clemente Island"   = 'san_clemente_island',
 #     "Santa Cruz Island"     = 'santa_cruz_island',
 #     "San Miguel Island"     = 'san_miguel_island',
 #     "San Nicolas Island"    = 'san_nicolas_island',
 #     "Santa Rosa Island"     = 'santa_rosa_island',
 #     "Mainland"              = 'mainland',
 #     "Anacapa Passage"       = 'anacapa_passage',
 #     "Footprint"             = 'footprint',
 #     "Piggy Bank"            = 'piggy_bank'
 #     ),
 
  output$ui_location <- renderUI({
    get_data() %>% filter(.$location) %>%
    selectInput(
      "sel_location",
      label = div (em("Choose a location:")),
      choices = get_data() %>% distinct(location) %>% .$location)
  })

  ## Plot data set on map 
  output$plot <- renderPlot({
    dataset<- get_data() 
  
  ## Average diversity/biomass of all sites by year! 
    dataset<-dataset %>%
      group_by(year) %>%
      summarise(v=mean(v,na.rm=T)) %>%
      ungroup()
     fit<-lm(dataset$v~dataset$year)
    plot(dataset$year, dataset$v, xlab = "Year", ylab = dataset_v[input$sel_dataset], frame.plot = "FALSE", col = "darkblue")
    abline(fit,col="darkblue")
  })
  
  # ggplot(data=Kelpbio, aes(x=year, y=kelp_biomass_kg)) + geom_bar(stat="identity")
  
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
      paste(input$sel_dataset, '_', '.csv', sep='')
      #paste(input$sel_dataset, '.zip', sep='')
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