################################################################################
#'                      GEOANALYSISMODULE FUNCTIONS                             
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Description: Collection of functions used to support data loading/validation,
#'              Analytics, and Visualizations.
#'              
#' Author: Dan Klemfuss
#' 
#' Classification: Unclassified
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Overview: 
#' 1. LoadCountryData(): Loads Lookup data for Countries
#' 2. PerformStatAnalysis(); Performs Statistical Analysis
#' 3. MergeGeoData(): Merges Statistical Data with Geospatial Layers
#' 4. GenerateStatReport(): Generate Stat Report for use in GUI
#' 5. UpdateGeoData(): Allow user to updated Geo data for additional analysis
#' 
################################################################################
library(data.table)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(sp)



#' Load Country Data for use in Application
#'
#' The CSV data pre-generated to initialize the analysis. Includes a breakdown 
#' by Country, Language, the commonality of the language, whether Google Translate
#' currently supports offline or Talk modes, the approximate number of travellers
#' from the U.S. per year, the 4G Penetration, whether 5G is planned, and the FIPS
#' (Federal Information Processing Standards) Identifier for the country (used as
#' the common key)
#' @param file.path string; Full Path to the CSV file to be loaded
#' @return A Data Frame containing the raw data, and error message
#' @export
LoadCountryData <- function(file.path="/data/Country_Data_A.csv"){
  require(data.table)
  tryCatch({
    raw <- data.table::fread(file.path)
    output <- list(data=raw, error=NA)
    return(output)
    
  }, error=function(e){
    err <- print(paste0("Error in LoadCountryStatisticalData(): ", e))
    output <- list(data=NA, error=err)
    return(output)
  })
}

#' Load Country Data (TM_WORLD_BORDERS-03)
#'
#' Using an open-source wordl borders from:  
#'
#' @param file.path string; Full Path to the source file
#' @return A list of Spatial Polygons Data Frame containing the raw data, and error message
#' @export
#' 
#' Note: This data set included population data and other fields not necessarily
#'       used in this application. 
#
LoadWorldBorders <- function(filePath="./data/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.dvf"){
  tryCatch({
    world_spdf <- rgdal::readOGR(filePath) # Define spatial data frame
    # Clean the data object: 
    world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
    world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)
    return(world_spdf)
  }, error = function(e){
    err <- print(paste0("Error in LoadCountryStatisticalData(): ", e))
    return(output)
  })
  
}


#' Determine the Statistical Success rate by country (FIPS)
#'
#' Using FIPS Data output from CalculateRanking, determine the success rate 
#' by country. 
#'
#' @param fips.data data.frame; Output from CalculateRanking() function
#' @return A Data Frame containing the success rate summary by country
#' @export
#
PerformStatAnalysisByCountry <- function(fips.data){
  tryCatch({
    # Calculate the probability of success by country: 
    fips.data <- fips.data %>% 
      group_by(FIPS, Flag_URL,`4G_Penetration`, `5G_Planned`, `Approx. U.S. Citizen Travel`,`Continent`,`prob_sim`,`var_sim`, `Pop_2021`) %>% 
      summarize(
        trans_prob = mean(prob_theory)
    )

    return(fips.data)
  }, error=function(e){
    print(e)
    return(NULL)
  })
}

#' Determine the Overall Statistical Success (FIPS)
#'
#' Using FIPS Data output from CalculateRanking, determine the success rate 
#' overall 
#'
#' @param fips.data data.frame; Output from CalculateRanking() function
#' @return A Data Frame containing the overall success rate summary
#' @export
#
PerformStatAnalysisOverall <- function(fips.data, type="Theoretical"){
  tryCatch({
    # Calculate the probability of success by country: 
    if (type=="Theoretical"){
      output <- sum(fips.data$trans_prob*fips.data$`Approx. U.S. Citizen Travel`)/sum(fips.data$`Approx. U.S. Citizen Travel`)
    } else if (type=="Simulated"){
      output <- sum(fips.data$prob_sim*fips.data$`Approx. U.S. Citizen Travel`)/sum(fips.data$`Approx. U.S. Citizen Travel`)
    } 
    return(output)
  }, error=function(e){
    print(e)
    return(NULL)
  })
}

#' Merge Country Boundary and Statistical Data
#'
#' Merge the Country boundary and statstical data, and provide information 
#' needed for pop-up in leaflet
#'
#' @param sp spatial polygon data.frame; Output from LoadWorldBorders()
#' @param df data frame containing statistical information; output from
#'                    PerformStatAnalysisOverall()
#' @return A Data Frame containing the overall success rate summary
#' @export
#
MergeData <- function(sp, df){
  tryCatch({
    require(sp)
    output <- merge(sp, df,by="FIPS", all=F)
    return(output)
  }, error=function(e){
    print(e)
    return(NULL)
  })
}


#' Generate Analysis - TODO: UPDATE!!!
#'
#' Document the Analysis for use in the Shiny Dashboard 'Analysis' Tab
#'
#' @param session Handle to current Shiny Session
#' @param fipsData data frame containing statistical information; output from
#'                    PerformStatAnalysisOverall()
#' @param mergedData data frame containing the merged data of the spatial polygon
#'                    and stat data from MergeData()
#' @return A Data Frame containing the overall success rate summary
#' @export
#

GenerateAbout <- function(input, output, session, fipsData, mergedData){
  # Define the namespace for the current session:
  ns <- session$ns
  
  tryCatch({
    output$about.output <- renderUI({
      tagList(
        h2(tags$b("SYS-5380 FINAL PROJECT")),
        h3(tags$b("Team:"), "The UnderAchievers"),
        h3(tags$b("Term:"), "Summer 2021"),
        h3("Florida Institue of Technology"),
        br(), 
        br(),
        h3(tags$b('1. Overview')),
        h4(paste0("This application serves as a tool to investigate the hypothesis ",
                  "that the Delphi Smart Glass product will have a probability of ",
                  "successfully translating a non-English conversation for a U.S. ",
                  "Citizen, greater than 75% of the time. The analysis takes into ",
                  "account the following factors:")),
        h4("- Likelihood of U.S. Citizens Travelling to the country"),
        h4("- Languages spoken in each country"),
        h4("- Commonality of the Language spoken per country"),
        h4("- Google Talk/Offline support by Language"),
        h4("- 4G Penetration and 5G Plans by Country"),
        h3(tags$b('2. Data')),
        h4(paste0("Data was gathered from several sources, and when unavailable ",
                  "supplemented by best-guess estimates. A recommendation for the ",
                  "future is to subscribe to data sets that will provide more ",
                  "complete/up-to-date data (for a price). ")),
        h4(paste0("The following are links to the primary data sets used: ")),
        h4(tags$b("WORLD BORDERS:"),"http://thematicmapping.org/downloads/world_borders.php"),
        h4(tags$b("GOOGLE TRANSLATE:"),"https://translate.google.com/intl/en/about/languages/"),
        h4(tags$b("4G LTE PENETRATION:"),"https://www.opensignal.com/sites/opensignal-com/files/data/reports/global/data-2019-05/the_state_of_mobile_experience_may_2019_0.pdf"),
        h4(tags$b("4G COVERAGE:"),"https://www.worldtimezone.com/4g.html"),
        h4(tags$b("5G PLANS:"),"https://www.lifewire.com/5g-availability-world-4156244"),
        h4(tags$b("POP. 2019:"),"https://www.populationpyramid.net/population-size-per-country/2019/"),
        h3(tags$b("3. Assumptions")),
        h4(paste0("Due to the lack of finer details in the data sets, several assumptions",
                  "were made to perform the analysis:")),
        h4(paste0("a. The number of U.S. travellers by country can be used as the ",
                  "proportion overall for hypothesis testing")),
        h4(paste0("b. The distribution of languages can be modelled as a normal",
                  "distribution, with National/Official Languages falling within",
                  "one standard deviation (~64% of encounters), Widely-spoken/Regional",
                  "Languages falling within two standards deviations (~95.2% of ",
                  "encounters), and Minority languages complete the distribution ",
                  "(filling in the tails of the curve). ")),
        h4(paste0("c. Google Talk/Offline availability are a good indication of the ",
                  "complexity related to processing, and can be used as a mechanism",
                  "to assign a probability when combined with 4G/5G availability")),
        h4(paste0("d. The data used (mostly from 2019) is a good indicator of ",
                  "future performance (note: COVID-19 in 2020-2021 significantly",
                  " impacted travel and should not be used for future predictions)"))
      )
    })
  })

}


GenerateAnalysis <- function(input, output, session, fipsData, mergedData){
  
}





#' Perform Statistical Analysis of Delphi Translation Success
#'
#' Using Loaded Data or User-updated data, perform a statistical analysis of the
#' likelihood of success of Near-Real-Time Translation. 
#'
#' @param file.path string; Full Path to the CSV file to be loaded
#' @return A Data Frame containing the raw data, and error message
#' @export
#' 
#' Note: Use 4G LTE Penetration as the primary factor...for countries without
#'       available data, assume 0.4% (since all other countries go to 50%, 
#'       take the median of the ones without data)
TEST_CalculateRanking <- function(country.data,
                                  seed = 93,
                                  # Binomial Probabilities:      
                                  rank_talk_5g_common=0.99,  #5G augmented common language
                                  rank_talk_4g_common=0.98, #4G augmented common language 
                                  rank_talk_5g_uncommon=0.95, #5G augmented non-common language
                                  rank_talk_4g_uncommon = 0.93, #4G augmented non-common language
                                  rank_offline=0.9, # Google Offline supported
                                  rank_unsupported=0, # Google offline not available
                                  scaling_no_4g=0.4, #No/Unknown network support for language
                                  el_1_2 = 0.68, # Encounter likelihood 1/2 (Official/National): 1st SD 68%
                                  el_3_4 = 0.272, # Encounter likelihood 3/4(Regional/Widely-spoken): 2nd SD 27.2%
                                  el_5 = 0.048, # Encounter likelihood 5 (Minority):3rd SD 4.2% + 0.6 leftover
                                  number_sim = 100,
                                  number_sample = 1000
)
{
  # Proceed if no errors with data:
  if (is.na(country.data$error)){
    # Set RNG Seed: 
    set.seed(seed)
    
    # Group data by Country Federal Information Processing Standards (FIPS) Codes:
    grouped.data <- country.data$data %>% group_by(FIPS)
    
    # OVERALL: 
    #' 1. Calculate the probability of success (binomial) for each line-item
    #' 2. Distribute the population based on the ranking of the languages (population 2019)
    #' 3. Run simulation for expected number of U.S. travellers, randomly selecting from the population (monte carlo simulation)
    #' 
    #
    fips.data <- grouped.data %>% mutate(
      prob_theory = case_when(
        # Cat0: No network needed (English - Assumption user speaks English)
        `Language`=="English" ~ 1.0,
        # Cat1: No 4G/5G Data Available (Google Talk unavailable, but offline may be)
        `4G_Penetration`==0 & `5G_Planned`=="No" & Google_Offline==F  ~ rank_unsupported,
        `4G_Penetration`==0 & `5G_Planned`=="No" & Google_Offline==T  ~ rank_offline*scaling_no_4g,
        #Cat2: No 4G known/ but possibly 5G Data Available in future (Assume 4G for now...)
        `4G_Penetration`==0 & `5G_Planned`=="Yes" & Google_Offline==F & Google_Talk==F ~ rank_talk_4g_uncommon*scaling_no_4g,
        `4G_Penetration`==0 & `5G_Planned`=="Yes" & Google_Offline==T & Google_Talk==F ~ rank_offline*scaling_no_4g,
        `4G_Penetration`==0 & `5G_Planned`=="Yes" & Google_Offline==F & Google_Talk==T ~ rank_talk_4g_common*scaling_no_4g,
        `4G_Penetration`==0 & `5G_Planned`=="Yes" & Google_Offline==T & Google_Talk==T ~ rank_talk_4g_common*scaling_no_4g,
        #Cat3: No 5G known/ but 4G Available:
        `4G_Penetration`>0 & `5G_Planned`=="No" & Google_Offline==F & Google_Talk==F ~ rank_talk_4g_uncommon*as.numeric(`4G_Penetration`)/100,
        `4G_Penetration`>0 & `5G_Planned`=="No" & Google_Offline==T & Google_Talk==F ~ rank_offline*as.numeric(`4G_Penetration`)/100,
        `4G_Penetration`>0 & `5G_Planned`=="No" & Google_Offline==F & Google_Talk==T ~ rank_talk_4g_common*as.numeric(`4G_Penetration`)/100,
        `4G_Penetration`>0 & `5G_Planned`=="No" & Google_Offline==T & Google_Talk==T ~ rank_talk_4g_common*as.numeric(`4G_Penetration`)/100,
        #Cat4: 4G and 5G planned: 
        `4G_Penetration`>0 & `5G_Planned`=="Yes" & Google_Offline==F & Google_Talk==F ~ rank_talk_5g_uncommon*as.numeric(`4G_Penetration`)/100,
        `4G_Penetration`>0 & `5G_Planned`=="Yes" & Google_Offline==T & Google_Talk==F ~ rank_offline*as.numeric(`4G_Penetration`)/100,
        `4G_Penetration`>0 & `5G_Planned`=="Yes" & Google_Offline==F & Google_Talk==T ~ rank_talk_5g_common*as.numeric(`4G_Penetration`)/100,
        `4G_Penetration`>0 & `5G_Planned`=="Yes" & Google_Offline==T & Google_Talk==T ~ rank_talk_5g_common*as.numeric(`4G_Penetration`)/100,
        TRUE ~ 0)
    )
    
    # Perform Simulation for Each Country:  
     sim <- data.frame(FIPS = unique(fips.data$FIPS))
    
    # Break up index of max number of people in percentile, by proportion of languages in each SD grouping:
    for (i in 1:nrow(sim)){
      dat <- fips.data %>% filter(FIPS==sim$FIPS[i]) %>% mutate(
        sd_group = case_when(grepl("1|2", Encounter_Likelihood) ~ 1,
                             grepl("3|4", Encounter_Likelihood) ~ 2,
                             TRUE ~ 3)
      )
      # Identify unique values for standard deviations groups:
      unique.sd <- unique(dat$sd_group)
      # Determine probability that language is encountered:
      dat <- dat %>% group_by(sd_group) %>% mutate(
        prob = case_when(sd_group==1 & all(c(2,3) %in% unique.sd) ~ 0.682/n(),
                         sd_group==2 & all(c(2,3) %in% unique.sd) ~ 0.272/n(),
                         sd_group==3 & all(c(2,3) %in% unique.sd) ~ 0.046/n(),
                         sd_group==1 & 2 %in% unique.sd ~ 0.682/n(),
                         sd_group==2 & 2 %in% unique.sd ~ (0.272+0.046)/n(),
                         sd_group==1 ~ 1.0)
      )
      # Define a cumulative sum of probabilities for use in RNG: 
      dat$cum_prob <- cumsum(dat$prob)
      dat$min_val <- c(0,dat$cum_prob[1:nrow(dat)-1])
      
      # Randomly select 1000 people for monte carlo simulation: 
      test_results <- data.frame(ID = 1:number_sample, rand_val=runif(number_sample)) %>% rowwise() %>% mutate(
          Key = dat$Key[which(dat$cum_prob >= rand_val & dat$min_val < rand_val)],
          Prob_Success = dat$prob_theory[which(dat$cum_prob >= rand_val & dat$min_val < rand_val)],
          Rand_Success = runif(1),
          Success = case_when(Rand_Success <= Prob_Success ~ 1, 
                              Rand_Success > Prob_Success ~ 0)
        )
      
      # Update fips.data with simulation results: 
      sim$prob_sim[[i]] <- mean(test_results$Success)
      sim$var_sim[[i]] <- var(test_results$Success)

    }
     
    # Merge the simulation data with the original FIPS data: 
     sim$prob_sim <- unlist(sim$prob_sim)
     sim$var_sim <- unlist(sim$var_sim)
     output <- merge(fips.data, sim, by="FIPS")

    return(output)
    
  }
}
