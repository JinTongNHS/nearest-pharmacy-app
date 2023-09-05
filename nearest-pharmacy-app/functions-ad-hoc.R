
####ad-hoc -- current list of pharmacies able to deliver SCS in the Shropshire, Telford and Wrekin (STW) area

#read in data
pharmlist <- readRDS("pharmlist.rds")
smoking_registrations <- readRDS("smoking_registrations.rds")
blood_pressure_check_registrations <- readRDS("blood_pressure_check_registrations.rds")
contraception_registrations <- readRDS("contraception_registrations.rds")
cpcs_registrations <- readRDS("cpcs_registrations.rds")
nms_registrations <- readRDS("nms_registrations.rds")
tlhc_registrations <- readRDS("tlhc_registrations.rds")



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
      filter(`Signed up to SCS` == 'YES')
    
  }else if(serviceType == "cpcs"){
    
    pharm_df <- pharm_df %>%
      filter(`Signed up to CPCS` == 'YES')
    
  }else if(serviceType == "contraception"){
    
    pharm_df <- pharm_df %>%
      filter(`Signed up to contraception services` == 'YES')
    
  }else if(serviceType == "bp"){
    
    pharm_df <- pharm_df %>%
      filter(`Signed up to BP checks` == 'YES')
    
  }else if(serviceType == "nms"){
    
    pharm_df <- pharm_df %>%
      filter(`Signed up to NMS` == 'YES')
    
  }else if(serviceType == "tlhc"){
    
    pharm_df <- pharm_df %>%
      filter(`Signed up to TLHC` == 'YES')
    
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
             `Pharmacy Name` = Name,
             `Pharmacy Address 1` = `Address Line 1`,
             `Pharmacy Address 2` = `Address Line 2`,
             `Pharmacy Address 3` = `Address Line 3`,
             `Pharmacy Address 4` = `Address Line 4`,
             `Pharmacy postcode` = Postcode,
             `ICB` = `ICB Code`,
             `Pharmacy Phone Number` = `F-code Phone Number`)
  }else{
    data <- pharm_df %>%
      select(`Distance to pharmacy (m)` = distance_metres,
             `Distance to pharmacy (miles)` = distance_miles,
             `Pharmacy ODS Code` = ODS.CODE,
             `Signed up to SCS`,
             `Pharmacy Name` = Name,
             `Pharmacy Address 1` = `Address Line 1`,
             `Pharmacy Address 2` = `Address Line 2`,
             `Pharmacy Address 3` = `Address Line 3`,
             `Pharmacy Address 4` = `Address Line 4`,
             `Pharmacy postcode` = Postcode,
             `ICB` = `ICB Code`,
             `Pharmacy Phone Number` = `F-code Phone Number`,
             x_pharm,
             y_pharm)
  }
  
  data
}

################################################################################
get_latest_pharm_list_date <- function(pharm_list = pharmlist){
  
  as.character(format(as.Date(max(pharm_list$Date)), "%B %Y"))
}


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
    filter(!is.na(x_pharm) & !is.na(y_pharm)) %>%
    mutate(label = "pharm") %>%
    ungroup() %>%
    add_row(`Pharmacy postcode` = search_postcode, label = "Input") %>%
    mutate(postcode = str_replace_all(`Pharmacy postcode`, " ", ""))
  
  df <- df %>%
    rowwise() %>%
    mutate(address = postcode,
           lat = postcode_lookup(postcode)$latitude,
           long = postcode_lookup(postcode)$longitude) %>%
    ungroup()
  
  input_postcode <- df %>%
    filter(df$label == "Input")
  
  iconSet <- awesomeIconList(
    "pharm" = makeAwesomeIcon( icon = 'medkit', lib = 'fa', iconColor = "black", markerColor = "green"   , spin = FALSE ) ,
    "Input" = makeAwesomeIcon( icon = 'male', lib = 'fa', iconColor = "black", markerColor = "red" , spin = FALSE )
  )
  
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