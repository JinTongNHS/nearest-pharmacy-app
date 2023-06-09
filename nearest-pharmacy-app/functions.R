#read in data
pharmlist <- readRDS("pharmlist.rds")
smoking_registrations <- readRDS("smoking_registrations.rds")
blood_pressure_check_registrations <- readRDS("blood_pressure_check_registrations.rds")
contraception_registrations <- readRDS("contraception_registrations.rds")
cpcs_registrations <- readRDS("cpcs_registrations.rds")
nms_registrations <- readRDS("nms_registrations.rds")



################################################################################
#function to find nearest straight line pharmacy to GP
get_nearest_pharmacies <- function(search_postcode, 
                                   pharm_df = pharmlist,
                                   num_pharms = 5,
                                   smokingPharms = smoking_registrations,
                                   serviceType,
                                   forMap = FALSE){
  
  x_postcode <- postcode_lookup(search_postcode)$eastings
  y_postcode <- postcode_lookup(search_postcode)$northings
  

  if(serviceType == "smoking"){

    pharm_df <- pharm_df %>%
      filter(`Signed up to SCS` == TRUE)

  }else if(serviceType == "cpcs"){
    
    pharm_df <- pharm_df %>%
      filter(`Signed up to CPCS` == TRUE)
    
  }else if(serviceType == "contraception"){
    
    pharm_df <- pharm_df %>%
      filter(`Signed up to contraception services` == TRUE)
    
  }else if(serviceType == "bp"){
    
    pharm_df <- pharm_df %>%
      filter(`Signed up to BP checks` == TRUE)
    
  }else if(serviceType == "nms"){
    
    pharm_df <- pharm_df %>%
      filter(`Signed up to NMS` == TRUE)
    
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
             `Signed up to CPCS`,
             `Signed up to contraception services`,
             `Signed up to BP checks`,
             `Signed up to NMS`,
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
                           serviceType){
  
  df <- get_nearest_pharmacies(search_postcode = search_postcode, 
                                 pharm_df = pharm_df,
                                 num_pharms = num_pharms,
                                 smokingPharms = smokingPharms,
                                 serviceType = serviceType,
                                 forMap = TRUE)
  
  df <- df %>%
    mutate(`Signed up to SCS` = if_else(`Pharmacy ODS Code` %in% smokingPharms$ODS.CODE, "Smoking", "Non-smoking")) %>%
    #mutate(colour = if_else(`Signed up to SCS` == "Smoking", "Green", "Blue")) %>%
    filter(!is.na(x_pharm) & !is.na(y_pharm)) %>%
    #mutate(label = `Signed up to SCS`) %>%
    mutate(label = "pharm") %>%
    ungroup() %>%
    add_row(`Pharmacy postcode` = search_postcode, label = "Input") %>%
    mutate(postcode = str_replace_all(`Pharmacy postcode`, " ", ""))

  ## This is the magic bit that uses the tidygeocoder package to find longatudes and latitudes
  df <- df %>%
    rowwise() %>%
    #mutate( geo(address = postcode, method = 'osm')) %>%
    mutate(address = postcode,
           lat = postcode_lookup(postcode)$latitude,
           long = postcode_lookup(postcode)$longitude) %>%
    ungroup()

  # ## Filters cohort into three lists, one for each iconset
  # cohort_filter1 <- df %>%
  #   filter(label == "Smoking")
  # cohort_filter2 <- df %>%
  #   filter(label == "Non-smoking")
  input_postcode <- df %>%
    filter(df$label == "Input")

  ##  Create awesome icon sets for colours
  iconSet <- awesomeIconList(
    "pharm" = makeAwesomeIcon( icon = 'medkit', lib = 'fa', iconColor = "black", markerColor = "green"   , spin = FALSE ) ,
    "Input" = makeAwesomeIcon( icon = 'male', lib = 'fa', iconColor = "black", markerColor = "red" , spin = FALSE )
    )

  ## Creates layors for map, each for the three iconset 'Teams'
  map <- leaflet(df) %>%
    addTiles() %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    addAwesomeMarkers( lng = df$long,
                       lat = df$lat,
                       group = "pharm",
                       icon = iconSet[df$label],
                       label = paste0(df$`Pharmacy ODS Code`, "\n",df$`Pharmacy Name`)
                       ) %>%
    addAwesomeMarkers( lng = input_postcode$long,
                       lat = input_postcode$lat,
                       group = "Input",
                       icon = iconSet["Input"],
                       label = "You are here"
    ) %>%
    flyToBounds(lng1 = min(df$long),
                lat1 = min(df$lat),
                lng2 = max(df$long),
                lat2 = max(df$lat))

  map
}