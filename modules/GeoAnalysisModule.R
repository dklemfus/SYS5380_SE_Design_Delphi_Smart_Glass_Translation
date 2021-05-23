# Geographic Analysis Module

################################################################################
#                               MODULE UI                                      #
################################################################################

GeoAnalysisUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(title="Translation Analysis:", solidHeader=TRUE, collapsible=TRUE, width=12,
          h6(tags$b("Hypothesis:"), 'Worldwide successful transactions are greater than 75%'),
          fluidRow(
            valueBoxOutput(ns('theoretical.infobox')),
            valueBoxOutput(ns('simulated.infobox'))
          )
      ),
      tabsetPanel(
        tabPanel("Mapping", leafletOutput(ns("map"), height="70vh")),
        tabPanel("Analysis", 
                 box(title="Analysis", solidHeader=T, collapsible=F, width=12, 
                     uiOutput(ns("analysis.output")))),
        tabPanel("Update Analysis", 
                 box(title="Update Analysis", solidHeader=T, collapsible=F, width=12,
                     uiOutput(ns("update_analysis.output")))),
        tabPanel("About",  
                 box(title="Update Analysis", solidHeader=T, collapsible=F, width=12,
                     uiOutput(ns("about.output"))))
      )
    )
  )
}

################################################################################
#                             MODULE  SERVER                                   #
################################################################################

GeoAnalysisModule <- function(input, output, session, config){
  ns <- session$ns
  
  name <- "GeoAnalysisModule"
  
  # Perform initial analysis (Initialize Data): 
  fipsData <- TEST_CalculateRanking(config$statData)
  statSummaryCountry <- PerformStatAnalysisByCountry(fipsData)
  statDataOverall <- PerformStatAnalysisOverall(statSummaryCountry, type="Theoretical")
  statDataOverallSim <- PerformStatAnalysisOverall(statSummaryCountry, type="Simulated")
  mergedData <- MergeData(sp=config$worldBorders, df=statSummaryCountry)
  
  test <<- config$statData #DELETE
  test2 <<- config$worldBorders #DELETE
  test3 <<- fipsData #CalculateRanking(test) # DELETE
  test4 <<- statSummaryCountry#PerformStatAnalysisByCountry(test3) #DELETE
  test5 <<- statDataOverall #PerformStatAnalysisOverall(test3) #DELETE
  test6 <<- mergedData #MergeData(sp=test2, df=test4) #DELETE
  
  #Initialize Leaflet Map:
  bin.Translation <- c(0,10,20,30,40,50,60,70,80,90,100)
  palette.Translation <- colorBin( palette="PiYG", domain=mergedData$trans_prob, na.color="transparent", bins=bin.Translation)
  
  bin.Travellers <- c(0,1000,10000,100000,1000000,10000000,Inf)
  palette.Travelers <- colorBin( palette="Purples", domain=mergedData$`Approx U.S. Citizen Travel`, na.color="transparent", bins=bin.Travellers)
  
  bin.4g <- c(0,50,60,70,80,90,100)
  palette.4g <- colorBin( palette="Blues", domain=mergedData$`4G_Penetration`, na.color="transparent", bins=bin.4g)
  
  # Prepare the text for tooltips:
  popup.text <- paste(
    "<img src = ", mergedData$Flag_URL," width = 200><br/>",
    "<b>Country: </b>", mergedData$NAME,"<br/>",
    "<b>Translation Prob (Theory): </b>", round(mergedData$trans_prob,2)*100, "%<br/>",
    "<b>Translation Prob (Simulated): </b>", round(mergedData$prob_sim,2)*100, "%<br/>",
    "<b>Appprox. U.S. Citizen Travel: </b>", round(mergedData$`Approx. U.S. Citizen Travel`,0), "<br/>",
    "<b>4G Penetration: </b>", mergedData$`4G_Penetration`, "%<br/>",
    "<b>5G Planned (1 year): </b>", mergedData$`5G_Planned`, "<br/>",
    "<b>Population (2021):  </b>", mergedData$`Pop_2021`, "<br/>",
    sep="")  %>%
    lapply(htmltools::HTML)
  
  output$map <- renderLeaflet({
    map <- leaflet(mergedData, options=leafletOptions(zoomSnap=0.1)) %>%
      addTiles() %>%
      setView(lng=0, lat=0, zoom=3) %>%
      # Create Translation Probability Layer - Theory (Default)
      addPolygons(
        fillColor = ~palette.Translation(trans_prob*100),
        stroke=T,
        fillOpacity=0.5,
        color='white',
        weight=0.3,
        label = popup.text,
        group="Translation Probability (Theoretical)"
      ) %>%
      addLegend( pal=palette.Translation, values=~trans_prob*100, opacity=0.5, 
                 title = "Translation Prob (Non-English) - Theoretical", position="bottomleft",
                 group="Translation Probability (Theoretical)") %>%
      # Create Translation Probability Layer - Simulated 
      addPolygons(
        fillColor = ~palette.Translation(prob_sim*100),
        stroke=T,
        fillOpacity=0.5,
        color='white',
        weight=0.3,
        label = popup.text,
        group="Translation Probability (Simulated)"
      ) %>%
      addLegend( pal=palette.Translation, values=~prob_sim*100, opacity=0.5, 
                 title = "Translation Prob (Non-English) - Simulated", position="bottomleft",
                 group="Translation Probability (Simulated)") %>%
      # Create U.S. Traveler Layer 
      addPolygons(
        fillColor = ~palette.Travelers(mergedData$`Approx. U.S. Citizen Travel`),
        stroke=T,
        fillOpacity=0.5,
        color='white',
        weight=0.3,
        label = popup.text,
        group="U.S. Travelers"
      ) %>%
      addLegend(pal=palette.Travelers, values=~`Approx. U.S. Citizen Travel`, 
                opacity=0.5, title= "Approx. U.S. Citizen Travel", position="bottomleft",
                group="U.S. Travelers") %>%
      # Create 4G Penetration Layer 
      addPolygons(
        fillColor = ~palette.4g(mergedData$`4G_Penetration`),
        stroke=T,
        fillOpacity=0.5,
        color='white',
        weight=0.3,
        label = popup.text,
        group="4G Penetration"
      ) %>%
      addLegend(pal=palette.4g, values=~`4G_Penetration`, 
                opacity=0.5, title= "4G Penetration (%)", position="bottomleft",
                group="4G Penetration") %>%
      addLayersControl(overlayGroups=c("Translation Probability (Theoretical)",
                                       "Translation Probability (Simulated)",
                                       "U.S. Travelers","4G Penetration"),
                       options= layersControlOptions(collapsed=F)) %>% 
      hideGroup("U.S. Travelers") %>% 
      hideGroup("4G Penetration") %>%
      hideGroup("Translation Probability (Simulated)")
    
  })
  
  # Perform initial analysis with default values: 
  #GenerateAnalysis(input, output, session, fipsData, mergedData)
  
  # Update the 'About' Tab: 
  GenerateAbout(input, output, session, fipsData, mergedData)
  
  # Update Value Boxes: 
  output$theoretical.infobox <- renderValueBox({
    valueBox(
      paste0(round(statDataOverall*100,2),"%"), icon=icon('bar-chart'),
      subtitle = "Theoretical Translation Probability", color = "purple")
  })
  output$simulated.infobox <- renderValueBox({
    valueBox(
      paste0(round(statDataOverallSim*100,2),"%"), icon=icon('bar-chart'),
      subtitle = "Simulated Translation Probability (Monte Carlo)", color = "purple")
  })
  
  
  
  
}