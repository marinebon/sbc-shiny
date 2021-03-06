###### Practice ui.R######


library(shiny) #Main library for Shiny
library(dplyr) #Data wrangling package
library(tidyr) #Data wrangling package
library(shinythemes) #Provide CSS themes for app
library(leaflet) #used to devleop map 
library(rsconnect) #Used to deploy shiny app to shinyapp.io for inclusion in website 

######To deploy to the shiny.io server you must use relative path names and your data must be in a data folder within the folder that contains the ui.R file and the server.R file. Only files in the folder and subfolders of your Shiny app are made available to shinyapps.io! ######### 

# DeepFish <- read_csv("data/deep_water_fish_diversity/fish_diversity.csv")
# # Mobile <- read.csv("data/kelp_forest/mobileinvertbrate_diversity_web.csv",header=TRUE)
# # FishDensity<- read.csv("data/kelp_forest/fish_density_web.csv",header=TRUE)
# # Kelpbio<- read.csv("data/kelp_forest/kelp_biomass_web.csv", header=TRUE)
# 
# site <- as.character(unique(DeepFish$site)) ##Convert site names to list - if these differ between datasets, you will have to change

shinyUI(
  fluidPage(
    fluidRow(
      br(),
      h2(div(strong("SBC MBON Interactive Map")),align="center"),
      hr(),
      column(12, tabsetPanel(
        type="tabs",
          
        tabPanel(
          "Interactive Map",
          br(),
          leafletOutput("map", width="100%", height="600"),
                      
          #Panel for the plot and choosing a dataset on the map :) 
          absolutePanel(
            id= "Map Controls", class = "panel panel-default", fixed = TRUE, 
            draggable = TRUE, top = 150, left = "auto", right = 20, bottom = "auto", 
            width = 300, height = "auto", style = "opacity:0.85; z-index = 100" ,
            wellPanel(
              h4(div(strong("Filtering Options"))),
              br(),
              
              # downloadButton('downloadData', 'Download Dataset'),
              # br(), Download button dormant for the moment :) when you're actually ready to share the data it can be reactivated! 
              
              selectInput(
                "sel_dataset",
                label = div(em("Choose a dataset to display:")),
                choices = list(
                  "Kelp Biomass"          = 'Kelpbio',
                  "Deep Fish Density "    = 'DeepFish',
                  "Fish Density"          = 'FishDensity',
                  "Mobile Invertebrates"  = 'Mobile')),
              
              uiOutput("ui_location"),  #Location is coming from renderUI in server.R
              
              # uiOutput("ui_site"),
             
              #Generate plot 
              plotOutput("plot", height = 250)
              )
            )
          ),
        
        tabPanel(
          "Data Table",
          br(),
          DT::dataTableOutput("table")),
        
        tabPanel(
          "Data Summary",
          br(),
          DT::dataTableOutput("summary"))
        )) # closing tabsetPanel, column
      ))) # closing fluidRow, fluidPage, shinyUI


# selectInput(
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
# selected = 'all'),
