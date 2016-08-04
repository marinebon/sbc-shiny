###### Practice ui.R######


library(shiny) #Main library for Shiny
library(dplyr) #Data wrangling package
library(tidyr) #Data wrangling package
library(shinythemes) #Provide CSS themes for app
library(leaflet) #used to devleop map 
library(rsconnect) #Used to deploy shiny app to shinyapp.io for inclusion in website 

######To deploy to the shiny.io server you must use relative path names and your data must be in a data folder within the folder that contains the ui.R file and the server.R file. Only files in the folder and subfolders of your Shiny app are made available to shinyapps.io! ######### 

DeepFish <- read.csv("data/deep_water_fish_diversity/fish_diversity.csv",header=TRUE)
Mobile <- read.csv("data/kelp_forest/mobileinvertbrate_diversity_web.csv",header=TRUE)
FishDensity<- read.csv("data/kelp_forest/fish_density_web.csv",header=TRUE)

site <- as.character(unique(DeepFish$site)) ##Convert site names to list - if these differ between datasets, you will have to change

shinyUI(
  fluidPage(
    fluidRow(
      br(),
      h2(div(strong("SBC MBON Interactive Map")),align="center"),
      hr(),
      column(3,
             wellPanel(
               h3(div(strong("FILTERING OPTIONS"))),
               br(),
               selectInput(
                 "filter_data",
                 label   = div(em("Choose dataset to display:")),
                 choices = list(
                   #"Deep Fish"            = 'DeepFish',      # EMPTY FILE!
                   "Fish Density"         = 'FishDensity',
                   "Mobile Invertebrates" = 'Mobile')),
               br(),
               selectInput("filter_site",
                           label=div(em("Find specific sites:")),
                           choices=c("All",site)
               )
             ),
             downloadButton('downloadData', 'Download')
      ),
      column(9,
             tabsetPanel(
               type="tabs",
               tabPanel("Interactive Map",
                        br(),
                        leafletOutput("map",width="100%",height=600)
               ),
               tabPanel("Data Table",
                        br(),
                        DT::dataTableOutput("table")
               ),
               tabPanel("Data Summary",
                        br(),
                        DT::dataTableOutput("summary")
               )
             )
      )
    )
  )
)