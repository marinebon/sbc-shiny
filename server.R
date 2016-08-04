###### Practcing server.R###


library(shiny)
library(dplyr)
library(tidyr)
library(shinythemes)
library(leaflet)
library(rsconnect)
library(rgdal) # install.packages('rgdal') # https://cran.r-project.org/web/views/Spatial.html

DeepFish <- read.csv("data/deep_water_fish_diversity/fish_density.csv",header=TRUE)
Mobile <- read.csv("data/kelp_forest/mobileinvertbrate_diversity_web.csv",header=TRUE)
FishDensity<- read.csv("data/kelp_forest/fish_density_web.csv",header=TRUE)

#shp = readOGR('/Users/devinspencer/Downloads/ne_10m_admin_1_states_provinces','ne_10m_admin_1_states_provinces') # slotNames(shp) # summary(shp@data) # View(shp@data)
#shp_ca = subset(shp, name == 'California') # plot(shp_ca)

shinyServer(function(input,output,session) {
  
  ##Define data set to plot - filter first by data set and then filter by site
  data <- reactive({
    if (input$filter_data == "Deep Water Fish Diversity"){
      data <- DeepFish
    } else if (input$filter_data == "Mobile Invertebrates"){
      data <- Mobile 
    } else if (input$filter_data == "Fish Density"){
      data <- FishDensity 
    }
    
    if (input$filter_site == "All"){
      data <- data
    } else
      data <- filter(data,site == input$filter_site)
  })
  
  ##Generate data table
  output$table <- DT::renderDataTable({
    dat <- as.data.frame(data())
    colnames(dat) <- c("Year","Site","Richness","Shannon Index","Hill Number","Latitude","Longitude")
    DT::datatable(dat)
  })
  
  ##Summarize data - total number of records for each site, range of years, average diversity per site over years
  
  map_data <- reactive({
    dat <- as.data.frame(data())
    tot_rec <- count(dat,site)
    range <- dat %>% group_by(site) %>% summarise(min=min(year),max=max(year)) %>% mutate(range=max-min)
    avg_div <- dat %>% group_by(site) %>% summarise(avg_div=mean(richness))
    coord <- dat %>% select(site,latitude,longitude)
    sum <- left_join(tot_rec,range,by="site")
    sum <- sum %>% left_join(avg_div,by="site")
    sum <- sum %>% left_join(coord,by="site")
    sum <- distinct(sum)
    sum$avg_div <- round(sum$avg_div,2)
    sum
  })
  
  ##Generate map from summary data above
  
  output$map <- renderLeaflet({
    dat <- as.data.frame(map_data())
    leaflet(data=dat) %>%
      addProviderTiles("Esri.OceanBasemap") %>%  ##Esri.NatGeoWorldMap,* Esri.OceanBasemap, Esri.WorldImager
      setView(-119.7,34.4,zoom=8) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste0(div(strong("Site: ")),as.character(site),br(),div(strong("No. of records: ")),as.character(n),br(),div(strong("Avg. richness: ")),as.character(avg_div)),
        radius= ~n,
        fillOpacity=0.8,
        clusterOptions = markerClusterOptions()
      )
  })
  
  ##Generate summary table
  
  output$summary <- DT::renderDataTable({
    dat_m <- as.data.frame(map_data())
    colnames(dat_m) <- c("Site","Number of records","First year recorded","Last year recorded","Total #years on record","Average diversity","Latitude","Longitude")
    DT::datatable(dat_m)
  })
  
  ##Generate downloadable Data 
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$filter_data, '_', input$filter_site, '.csv', sep='') 
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