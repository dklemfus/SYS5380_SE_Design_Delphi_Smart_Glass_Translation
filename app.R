# app.R

#' Title: SYS-5380 Final Project ('Delphi Smart Glass')
#' Description: Application to analyze the probability of success of the Delphi
#'              smart glass Near-Real-Time Translation Feature, using the 
#'              liklihood of travel by U.S. citizens by country, the spoken 
#'              languages by country, and the availability of 5G by country. Note
#'              that some data is simulated, due to lack of available (free) data
# Author: Dan Klemfuss (Team: The Essential Ones)

# Load setup script: 
source('functions/basic_setup.R')

##
## Run setup and create global config object: 
##
global.config <<- Setup()
# Load in passable data: 
global.config$statData <<- LoadCountryData(global.config$lookups$StatAnalysis)
global.config$worldBorders <<- LoadWorldBorders(global.config$lookups$WorldBorders)

# Define libraries used (note: packages should be loaded in 'Setup' function)
library('shiny')
library('shinydashboard')
library('shinyWidgets')
library('shinyBS')
library('plotly')
library('ggplot2')
library('htmlwidgets')
library('data.table')
library('dplyr')
library('tidyr')
library('stringr')
library('lubridate')
library('leaflet')
library('leaflet.extras')
library('waiter')

##
## Define the UI
##
UI <- dashboardPage(
  
  # Create Dashboard Header: 
  dashboardHeader(title = "DELPHI"),
  
  # Create Dashboard Sidebar Menu: 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Translation Analysis", tabName = "tab1", icon = icon('globe'))
      #menuItem("Analysis Settings", tabName = "tab2", icon = icon('cog'))
      
    )
  ),
  # Create Dashboard Body (main panel when menu item selected)
  dashboardBody(
    # Create Loading Screen: 
    waiter::use_waiter(),
    tabItem(tabName = "tab1",
            GeoAnalysisUI('tab1'))
  )
  
)

##
## Define the Server Code: 
##
Server <- function(input, output, session){
  # Define source for logging: 
  name <- 'Delphi_Analysis_Main'
  
  load.image <- tagList(
    img(src="Delphi_Logo.png", width="400"), 
    spin_orbit(),
    br(), br(), br(),br(),
    "Initializing App..."
  )
  
  waiter_show(html=load.image)
  
  # Define namespace:
  ns <- session$ns
  
  # Call Modules:
  callModule(module=GeoAnalysisModule, id="tab1", config=global.config)
  # Hide Loading screen:
  waiter_hide()
}

# Return a Shiny app object
shinyApp(ui=UI, server=Server)
