


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
                                   smokingPharms = smoking_registrations,
                                   onlySmokingPharms,
                                   forMap = FALSE){
  
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
           distance_miles = round(distance_miles, 3)) 
  
  if(forMap == FALSE){
    data <- pharm_df %>%
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
  }else{
    data <- pharm_df %>%
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
             `Pharmacy Opening Hours Sunday` = Pharmacy.Opening.Hours.Sunday,
             x_pharm,
             y_pharm)
  }
  
  data
}

################################################################################
get_latest_pharm_list_date <- function(pharm_list = pharmlist){
  
  as.character(format(as.Date(max(pharm_list$SnapshotMonth)), "%B %Y"))
}



# create_example_map <- function(){
#   
# 
#   
#   ## creates a basic dataframe with some teams and postcodes
#   label <- c('Team A', 'Team A', 'Team B', 'Team B', 'Team C')
#   postcode <- c('EX16 7FL', 'EX39 5EN', 'PL13 2WP', 'PL15 8RZ', 'PL30 4PX')
#   df <- data.frame(label,postcode)
#   
#   ## This is the magic bit that uses the tidygeocoder package to find longatudes and latitudes
#   df <- df %>% mutate( geo(address = df$postcode, method = 'osm'))
#   
#   ## Filters cohort into three lists, one for each iconset
#   cohort_filter1 <- df %>%
#     filter(df$label == "Team A")
#   cohort_filter2 <- df %>%
#     filter(df$label == "Team B")
#   cohort_filter3 <- df %>%
#     filter(df$label == "Team C")
#   
#   ##  Create awesome icon sets for colours
#   iconSet <- awesomeIconList(
#     "Team A"  = makeAwesomeIcon( icon = 'male', lib = 'fa', iconColor = "black", markerColor = "red"   , spin = FALSE ) ,
#     "Team B"             = makeAwesomeIcon( icon = 'male', lib = 'fa', iconColor = "black", markerColor = "orange", spin = FALSE ) ,
#     "Team C"       = makeAwesomeIcon( icon = 'male', lib = 'fa', iconColor = "black", markerColor = "beige" , spin = FALSE ) )
#   
#   ## Creates layors for map, each for the three iconset 'Teams'
#   map <- leaflet(df) %>%  
#     addTiles() %>%
#     addProviderTiles(providers$OpenStreetMap) %>% 
#     addAwesomeMarkers( lng = cohort_filter1$long,
#                        lat = cohort_filter1$lat,
#                        group = "Team A",
#                        icon = iconSet[cohort_filter1$label],
#                        label = paste(sep = " - ",
#                                      cohort_filter1$label ) ) %>%
#     addAwesomeMarkers( lng = cohort_filter2$long,
#                        lat = cohort_filter2$lat,
#                        group = "Team B",
#                        icon = iconSet[cohort_filter2$label],
#                        label = paste(sep = " - ",
#                                      cohort_filter2$label ) ) %>%
#     addAwesomeMarkers( lng = cohort_filter3$long,
#                        lat = cohort_filter3$lat,
#                        group = "Team C",
#                        icon = iconSet[cohort_filter3$label],
#                        label = paste(sep = " - ",
#                                      cohort_filter3$label ) ) %>% 
#     addLayersControl(overlayGroups = c("Team A", "Team B", "Team C"),    ##this bit adds the controls
#                      options = layersControlOptions(collapsed = FALSE) ) 
#   
#   map
# }


################################################################################
create_leaflet <- function(search_postcode, 
                           pharm_df = pharmlist,
                           num_pharms = 5,
                           smokingPharms = smoking_registrations,
                           onlySmokingPharms){
  
  df <- get_nearest_pharmacies(search_postcode = search_postcode, 
                                 pharm_df = pharm_df,
                                 num_pharms = num_pharms,
                                 smokingPharms = smokingPharms,
                                 onlySmokingPharms = onlySmokingPharms,
                                 forMap = TRUE)
  
  df <- df %>%
    mutate(`Signed up to SCS` = if_else(`Pharmacy ODS Code` %in% smokingPharms$ODS.CODE, "Smoking", "Non-smoking")) %>%
    mutate(colour = if_else(`Signed up to SCS` == "Smoking", "Green", "Blue")) %>%
    filter(!is.na(x_pharm) & !is.na(y_pharm)) %>%
    mutate(label = `Signed up to SCS`) %>%
    ungroup() %>%
    add_row(`Pharmacy postcode` = search_postcode, label = "Input") %>%
    mutate(postcode = str_replace_all(`Pharmacy postcode`, " ", ""))

  ## This is the magic bit that uses the tidygeocoder package to find longatudes and latitudes
  df <- df %>%
    mutate( geo(address = postcode, method = 'osm'))

  ## Filters cohort into three lists, one for each iconset
  cohort_filter1 <- df %>%
    filter(label == "Smoking")
  cohort_filter2 <- df %>%
    filter(label == "Non-smoking")
  input_postcode <- df %>%
    filter(df$label == "Input")

  ##  Create awesome icon sets for colours
  iconSet <- awesomeIconList(
    "Smoking" = makeAwesomeIcon( icon = 'medkit', lib = 'fa', iconColor = "black", markerColor = "green"   , spin = FALSE ) ,
    "Non-smoking" = makeAwesomeIcon( icon = 'medkit', lib = 'fa', iconColor = "black", markerColor = "orange", spin = FALSE ) ,
    "Input" = makeAwesomeIcon( icon = 'male', lib = 'fa', iconColor = "black", markerColor = "red" , spin = FALSE )
    )

  ## Creates layors for map, each for the three iconset 'Teams'
  map <- leaflet(df) %>%
    addTiles() %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    addAwesomeMarkers( lng = cohort_filter1$long,
                       lat = cohort_filter1$lat,
                       group = "Smoking",
                       icon = iconSet[cohort_filter1$label],
                       label = paste0(df$`Pharmacy ODS Code`, "\n",df$`Pharmacy Name`)
                       ) %>%
    addAwesomeMarkers( lng = cohort_filter2$long,
                       lat = cohort_filter2$lat,
                       group = "Non-smoking",
                       icon = iconSet[cohort_filter2$label],
                       label = paste0(df$`Pharmacy ODS Code`, "\n",df$`Pharmacy Name`)
                       ) %>%
    addAwesomeMarkers( lng = input_postcode$long,
                       lat = input_postcode$lat,
                       group = "Input",
                       icon = iconSet["Input"],
                       label = "You are here"
    ) %>%
    # addAwesomeMarkers( lng = cohort_filter3$long,
    #                    lat = cohort_filter3$lat,
    #                    group = "Team C",
    #                    icon = iconSet[cohort_filter3$label],
    #                    label = paste(sep = " - ",
    #                                  cohort_filter3$label ) ) %>%
    addLayersControl(overlayGroups = c("Smoking", "Non-smoking"),    ##this bit adds the controls
                     options = layersControlOptions(collapsed = FALSE) ) %>%
    flyToBounds(lng1 = min(df$long),
                lat1 = min(df$lat),
                lng2 = max(df$long),
                lat2 = max(df$lat))

  map
}