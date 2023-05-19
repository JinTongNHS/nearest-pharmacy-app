library(tidyverse)
#https://docs.ropensci.org/PostcodesioR/
#install.packages("PostcodesioR")
library(PostcodesioR)
library(DBI)
library(odbc)
library(readxl)
#install.packages("shinythemes")
library(sf) 
library(tmap)

#read in data
pharmlist <- readRDS("pharmlist.rds")
Smoking_Cessation_Service_Data <- read_excel("Smoking Cessation Service Data Feb23.xlsx", sheet = "Pharmacy Data", skip = 2)
Smoking_Cessation_Service_Data <- Smoking_Cessation_Service_Data %>%
  select(ODS.CODE = `Pharmacy Code`)

################################################################################
pull_pharm_list <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql = "SELECT *
  FROM [NHSE_Sandbox_DispensingReporting].[dbo].[Ref_PharmaceuticalList]"
  result <- dbSendQuery(con,sql)
  pharm_list <- dbFetch(result)
  dbClearResult(result)
  
  names(pharm_list) <- names(pharm_list) %>% make.names()
  
  #fix miscoded snapshot month
  pharm_list <- pharm_list %>%
    mutate(SnapshotMonth = as.Date(SnapshotMonth)) %>%
    mutate(SnapshotMonth = if_else(SnapshotMonth == as.Date("2022-10-01"), 
                                   as.Date("2022-09-01"), 
                                   SnapshotMonth)) %>%
    rename(ODS.CODE = Pharmacy.ODS.Code..F.Code., 
           postcode = Post.Code, 
           ICB.Name = STP.Name, 
           EPS.Indicator = EPS.Enabled)
}

################################################################################
#update pharm list with postcode coords
update_pharm_list <- function(){
  
  #get recent pharm list
  full_pharmlist <- pull_pharm_list()
  
  pharmlist <- full_pharmlist %>%
    filter(SnapshotMonth == max(SnapshotMonth))
  
  #make sure all ODS codes and postcodes are clean
  pharmlist <- pharmlist %>%
    mutate(postcode = str_replace_all(postcode, " ", "")) %>%
    mutate(postcode = toupper(postcode)) %>% 
    mutate(ODS.CODE = str_replace_all(ODS.CODE, " ", "")) %>%
    mutate(ODS.CODE = toupper(ODS.CODE)) 
  
  #look up eastings and northings for each postcode
  pharmlist <- pharmlist %>%
    rowwise() %>%
    mutate(x_pharm = postcode_lookup(postcode)$eastings,
           y_pharm = postcode_lookup(postcode)$northings)
  
}

################################################################################
#function to find nearest straight line pharmacy to GP
get_nearest_pharmacies <- function(search_postcode, 
                                   pharm_df = pharmlist,
                                   num_pharms = 5,
                                   smokingPharms = Smoking_Cessation_Service_Data,
                                   onlySmokingPharms){
  
  x_postcode <- postcode_lookup(search_postcode)$eastings
  y_postcode <- postcode_lookup(search_postcode)$northings
  
  #filter if only looking for smoking 
  pharm_df <- pharm_df %>%
    mutate(`Signed up to SCS` = if_else(ODS.CODE %in% smokingPharms$ODS.CODE, TRUE, FALSE))
  
  if(onlySmokingPharms == TRUE){

    pharm_df <- pharm_df %>%
      filter(`Signed up to SCS` == TRUE)

  }

  #creates a separate dataframe of distances to all pharmacies
  pharm_df <- pharm_df %>%
    mutate(distance_metres = sqrt((y_pharm - y_postcode)^2 + (x_pharm - x_postcode)^2)) %>%
    arrange(distance_metres) %>%
    head(num_pharms) %>%
    mutate(distance_miles = distance_metres * 0.000621371) %>%
    mutate(distance_metres = round(distance_metres),
           distance_miles = round(distance_miles, 3)) %>%
    select(`Distance to pharmacy (m)` = distance_metres,
           `Distance to pharmacy (miles)` = distance_miles,
           `Pharmacy ODS Code` = ODS.CODE,
           `Signed up to SCS`,
           `Pharmacy Name` = Pharmacy.Trading.Name,
           `Pharmacy Organisation Name` = Organisation.Name,
           `Pharmacy Address 1` = Address.Field.1,
           `Pharmacy Address 2` = Address.Field.2,
           `Pharmacy Address 3` = Address.Field.3,
           `Pharmacy postcode` = postcode,
           `Pharmacy Opening Hours Monday` = Pharmacy.Opening.Hours.Monday,
           `Pharmacy Opening Hours Tuesday` = Pharmacy.Opening.Hours.Tuesday,
           `Pharmacy Opening Hours Wednesday` = Pharmacy.Opening.Hours.Wednesday,
           `Pharmacy Opening Hours Thursday` = Pharmacy.Opening.Hours.Thursday,
           `Pharmacy Opening Hours Friday` = Pharmacy.Opening.Hours.Friday,
           `Pharmacy Opening Hours Saturday` = Pharmacy.Opening.Hours.Saturday,
           `Pharmacy Opening Hours Sunday` = Pharmacy.Opening.Hours.Sunday)
  
}

################################################################################
get_latest_pharm_list_date <- function(pharm_list = pharmlist){
  
  as.character(format(as.Date(max(pharm_list$SnapshotMonth)), "%B %Y"))
}

################################################################################
create_map <- function(pharm_list = pharmlist,
                       smokingPharms = Smoking_Cessation_Service_Data){

  pharm_list <- pharm_list %>%
    mutate(`Signed up to SCS` = if_else(ODS.CODE %in% smokingPharms$ODS.CODE, TRUE, FALSE)) %>%
    mutate(colour = if_else(`Signed up to SCS` == TRUE, "Green", "Blue")) %>%
    filter(!is.na(x_pharm) & !is.na(y_pharm))
  
  #make into shape file
  scs_sf <- sf::st_as_sf(x = pharm_list, coords = c("x_pharm", "y_pharm"), crs = 27700)
  
  #bring red dots to the front
  scs_sf$colour <- factor(scs_sf$colour, levels = c("Green", "Blue"))
  scs_sf <- dplyr::arrange(scs_sf, colour)
  
  # #get data just for SCS pharms
  # `SCS pharmacies only` <- scs_sf %>%
  #   filter(`Signed up to SCS` == TRUE)
  

  #get boundaries
  ICB_boundaries <- sf::st_read("~/R-Projects/Unplanned-pharmacy-closures/data/geo_spatial_data/GI2740_Download_ICB_Boundaries/GI2740_Download_ICB_Boundaries.shp", quiet = TRUE)
  region_boundaries <- sf::st_read("~/R-Projects/Unplanned-pharmacy-closures/data/geo_spatial_data/GI2740_Download_NHSE_Region_Boundaries/GI2740_Download_NHSE_Region_Boundaries.shp", quiet = TRUE)
  `ICB boundaries` <- st_make_valid(ICB_boundaries)
  
  #plot map
  tmap::tmap_mode("view")
  #tmap::tmap_options(check.and.fix = TRUE)
  
  tm_shape(`scs_sf`,
           bbox = st_bbox(c(xmin =-7.57216793459, xmax = 1.68153079591, ymax = 58.6350001085, ymin = 49.959999905), crs = st_crs(4326))) +
    tm_dots(col = "colour",
            palette = c(Blue='blue',Green='green'),
            labels = c("Pharmacy not signed up to deliver SCS", "Pharmacy signed up to deliver SCS"),
            title = "Colour",
            popup.vars = c(`Distance to pharmacy (m)`,
                           `Distance to pharmacy (miles)`,
                           `Pharmacy ODS Code`,
                           `Signed up to SCS`,
                           `Pharmacy Name`,
                           `Pharmacy Organisation Name`,
                           `Pharmacy Address 1`,
                           `Pharmacy Address 2`,
                           `Pharmacy Address 3`,
                           `Pharmacy postcode`,
                           `Pharmacy Opening Hours Monday`,
                           `Pharmacy Opening Hours Tuesday`,
                           `Pharmacy Opening Hours Wednesday`,
                           `Pharmacy Opening Hours Thursday`,
                           `Pharmacy Opening Hours Friday`,
                           `Pharmacy Opening Hours Saturday`,
                           `Pharmacy Opening Hours Sunday`),
            legend.show = FALSE) +
    tm_shape(`scs_sf`,
             bbox = st_bbox(c(xmin =-7.57216793459, xmax = 1.68153079591, ymax = 58.6350001085, ymin = 49.959999905), crs = st_crs(4326))) +
    tm_shape(`ICB boundaries`,
             labels = "ICB boundaries") +
    tm_borders(col = "grey40", lwd = 2, lty = "solid", alpha = 0.5) +
    tm_layout(title = 'Pharmacies in England') +
    tm_scale_bar(position =c("left", "bottom"))
  
}
