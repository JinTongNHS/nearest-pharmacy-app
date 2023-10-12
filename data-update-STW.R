library(tidyverse)
#https://docs.ropensci.org/PostcodesioR/
#install.packages("PostcodesioR")
library(PostcodesioR)
library(DBI)
library(odbc)
library(readxl)


####ad-hoc -- current list of pharmacies able to deliver SCS in the Shropshire, Telford and Wrekin (STW) area
pharmlist <- read_excel("N:/_Everyone/Primary Care Group/registrations_data_for_app/STW Active list.xlsx")
#make sure all ODS codes and postcodes are clean
pharmlist <- pharmlist %>%
  mutate(Postcode = str_replace_all(Postcode, " ", "")) %>%
  mutate(Postcode = toupper(Postcode)) %>% 
  mutate(ODS.CODE = str_replace_all(`F-Code`, " ", "")) %>%
  mutate(ODS.CODE = toupper(ODS.CODE)) 

#look up eastings and northings for each postcode
pharmlist <- pharmlist %>%
  rowwise() %>%
  mutate(x_pharm = postcode_lookup(Postcode)$eastings,
         y_pharm = postcode_lookup(Postcode)$northings)

################################################################################
smoking_registrations <- read_excel("N:/_Everyone/Primary Care Group/registrations_data_for_app/STW Active list.xlsx")
smoking_registrations <- smoking_registrations %>%
  select(ODS.CODE = `F-Code`)



#blood_pressure_check_registrations <- read_excel("N:/_Everyone/Primary Care Group/registrations_data_for_app/blood_pressure_check_registrations.xlsx")
blood_pressure_check_registrations <- function(){
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql=" 
  SELECT [Month]=[Month(claim)]
      ,[Fcode]=[Pharmacy Code]  
      ,[ICB Code]=[STP Code] 
      ,[SetupFee]=[Set up fee]+[Set up fee adjustment]
  FROM [NHSE_Sandbox_DispensingReporting].[Load].[BloodPressureService_BSA_claims]
   "
  result<-dbSendQuery(con,sql)
  CVDclaims<-dbFetch(result)
  dbClearResult(result)
  CVDclaims
}
blood_pressure_check_registrations=blood_pressure_check_registrations()
blood_pressure_check_registrations <- blood_pressure_check_registrations %>%
  select(ODS.CODE = `Fcode`)

#contraception_registrations <- read_excel("N:/_Everyone/Primary Care Group/registrations_data_for_app/contraception_registrations.xlsx")
contraception_registrations<-function(){
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql="select  distinct [Service], [FCode],[DateReported]
  FROM [NHSE_Sandbox_DispensingReporting].[dbo].[Service_Registrations]
  where [Service]='Oral Contraception Tier 1 Service'"
  result<-dbSendQuery(con,sql)
  OCT1_reg<-dbFetch(result)
  dbClearResult(result)
  OCT1_reg
}
contraception_registrations=contraception_registrations()
contraception_registrations <- contraception_registrations %>%
  filter(DateReported==max(DateReported))%>%
  select(ODS.CODE = `FCode`)

#cpcs_registrations <- read_excel("N:/_Everyone/Primary Care Group/registrations_data_for_app/cpcs_registrations.xlsx")
cpcs_registrations <- function(){
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql=" select a.FCode from
  (SELECT FCode   
  FROM [NHSE_Sandbox_DispensingReporting].[dbo].[Service_Registrations]
  where Service = 'Community Pharmacy Consultation Service' and 
  DateReported = (select max([DateReported]) FROM [NHSE_Sandbox_DispensingReporting].[dbo].[Service_Registrations] 
  where Service = 'Community Pharmacy Consultation Service') ) a
  left join 
  (SELECT [FCode], [deReg]=1
  FROM [NHSE_Sandbox_DispensingReporting].[dbo].[Service_Deregistrations]
  where [Service]= 'Community Pharmacy Consultation Service' and [DateReported]= (select max([DateReported]) FROM [NHSE_Sandbox_DispensingReporting].[dbo].[Service_Deregistrations] where [Service] = 'Community Pharmacy Consultation Service')
  and [ReRegistrationDate] is NULL) b
   on a.[FCode]=b.[FCode] where b.[deReg] is NULL"
  
  result<-dbSendQuery(con,sql)
  cpcs_new<-dbFetch(result)
  dbClearResult(result)
  
  cpcs_new
}
cpcs_registrations=cpcs_registrations()

#nms_registrations <- read_excel("N:/_Everyone/Primary Care Group/registrations_data_for_app/nms_registrations.xlsx")
nms_registrations <- function(){
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql="select  distinct [Service], [FCode],[DateReported] 
  FROM [NHSE_Sandbox_DispensingReporting].[dbo].[Service_Registrations]
where [Service]='NMS expansions pilot'"
  result<-dbSendQuery(con,sql)
  Master1<-dbFetch(result)
  dbClearResult(result)
  
  Master1
  
}
nms_registrations<-nms_registrations()
nms_registrations <- nms_registrations %>%
  filter(DateReported==max(DateReported))%>%
  select(ODS.CODE = `FCode`)

tlhc_registrations <- read_excel("N:/_Everyone/Primary Care Group/registrations_data_for_app/tlhc_registrations.xlsx")
tlhc_registrations <- tlhc_registrations %>%
  select(ODS.CODE = `ODS Code`)

saveRDS(smoking_registrations, "nearest-pharmacy-app/smoking_registrations.rds")
saveRDS(blood_pressure_check_registrations, "nearest-pharmacy-app/blood_pressure_check_registrations.rds")
saveRDS(contraception_registrations, "nearest-pharmacy-app/contraception_registrations.rds")
saveRDS(cpcs_registrations, "nearest-pharmacy-app/cpcs_registrations.rds")
saveRDS(nms_registrations, "nearest-pharmacy-app/nms_registrations.rds")
saveRDS(tlhc_registrations, "nearest-pharmacy-app/tlhc_registrations.rds")




pharmlist <- pharmlist %>%
  mutate(`Signed up to SCS` = if_else(ODS.CODE %in% smoking_registrations$ODS.CODE, 'YES', 'NO'),
         `Signed up to CPCS` = if_else(ODS.CODE %in% cpcs_registrations$ODS.CODE, 'YES', 'NO'),
         `Signed up to contraception services` = if_else(ODS.CODE %in% contraception_registrations$ODS.CODE, 'YES', 'NO'),
         `Signed up to BP checks` = if_else(ODS.CODE %in% blood_pressure_check_registrations$ODS.CODE, 'YES', 'NO'),
         `Signed up to NMS` = if_else(ODS.CODE %in% nms_registrations$ODS.CODE, 'YES', 'NO'),
         `Signed up to TLHC` = if_else(ODS.CODE %in% tlhc_registrations$ODS.CODE, 'YES', 'NO'),)

saveRDS(pharmlist, "nearest-pharmacy-app/pharmlist.rds")
