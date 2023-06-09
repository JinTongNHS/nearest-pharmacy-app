library(tidyverse)
#https://docs.ropensci.org/PostcodesioR/
#install.packages("PostcodesioR")
library(PostcodesioR)
library(DBI)
library(odbc)
library(readxl)


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

#clean and save latest service data
smoking_registrations <- read_excel("N:/_Everyone/Primary Care Group/registrations_data_for_app/smoking_registrations.xlsx")
smoking_registrations <- smoking_registrations %>%
  select(ODS.CODE = `F-Code`)

blood_pressure_check_registrations <- read_excel("N:/_Everyone/Primary Care Group/registrations_data_for_app/blood_pressure_check_registrations.xlsx")
blood_pressure_check_registrations <- blood_pressure_check_registrations %>%
  select(ODS.CODE = `F-Code`)

contraception_registrations <- read_excel("N:/_Everyone/Primary Care Group/registrations_data_for_app/contraception_registrations.xlsx")
contraception_registrations <- contraception_registrations %>%
  select(ODS.CODE = `F-Code`)

cpcs_registrations <- read_excel("N:/_Everyone/Primary Care Group/registrations_data_for_app/cpcs_registrations.xlsx")
cpcs_registrations <- cpcs_registrations %>%
  select(ODS.CODE = `FCode`)

nms_registrations <- read_excel("N:/_Everyone/Primary Care Group/registrations_data_for_app/nms_registrations.xlsx")
nms_registrations <- nms_registrations %>%
  select(ODS.CODE = `ODS Code`)

saveRDS(smoking_registrations, "nearest-pharmacy-app/smoking_registrations.rds")
saveRDS(blood_pressure_check_registrations, "nearest-pharmacy-app/blood_pressure_check_registrations.rds")
saveRDS(contraception_registrations, "nearest-pharmacy-app/contraception_registrations.rds")
saveRDS(cpcs_registrations, "nearest-pharmacy-app/cpcs_registrations.rds")
saveRDS(nms_registrations, "nearest-pharmacy-app/nms_registrations.rds")

#get latest pharm list - this will take a while as it's getting the coordinates for every pharmacy on the list
#N.B. this also saves the pharm list data in the R project so that the app can access it once it's deployed
#pharmlist <- update_pharm_list()
pharmlist <- pharmlist %>%
  mutate(`Signed up to SCS` = if_else(ODS.CODE %in% smoking_registrations$ODS.CODE, TRUE, FALSE),
         `Signed up to CPCS` = if_else(ODS.CODE %in% cpcs_registrations$ODS.CODE, TRUE, FALSE),
         `Signed up to contraception services` = if_else(ODS.CODE %in% contraception_registrations$ODS.CODE, TRUE, FALSE),
         `Signed up to BP checks` = if_else(ODS.CODE %in% blood_pressure_check_registrations$ODS.CODE, TRUE, FALSE),
         `Signed up to NMS` = if_else(ODS.CODE %in% nms_registrations$ODS.CODE, TRUE, FALSE),
         )

saveRDS(pharmlist, "nearest-pharmacy-app/pharmlist.rds")