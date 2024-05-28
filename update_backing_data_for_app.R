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

#phone number
phone_number<- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql="select  distinct [ODS.CODE]=[FCode], [FcodePhone]
  FROM [NHSE_Sandbox_DispensingReporting].[dbo].[Service_Registrations]"
  result<-dbSendQuery(con,sql)
  phone_number<-dbFetch(result)
  dbClearResult(result)
  phone_number
  
}
phone_number=phone_number()



#clean and save latest service data
smoking_registrations <- read_excel("N:/_Everyone/Primary Care Group/registrations_data_for_app/smoking_registrations.xlsx")
smoking_registrations <- smoking_registrations %>%
  select(ODS.CODE = `F-Code`)
#smoking_registrations<- function(){
  
#  con <- dbConnect(odbc::odbc(), "NCDR")
#  sql="select  distinct [Service], [FCode] ,[DateReported]
#  FROM [NHSE_Sandbox_DispensingReporting].[dbo].[Service_Registrations]
#where [Service]='Smoking Cessation Advanced Service'"
#  result<-dbSendQuery(con,sql)
#  smoking_reg<-dbFetch(result)
#  dbClearResult(result)
#  smoking_reg

#}
#smoking_registrations=smoking_registrations()
#smoking_registrations <- smoking_registrations %>%
#  filter(DateReported==max(DateReported))%>%
#  select(ODS.CODE = `FCode`)



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
  sql="select  distinct [Service], [FCode], [RegistrationDate], [DateReported] FROM [NHSE_Sandbox_DispensingReporting].[dbo].[Service_Registrations]
where [Service]='Oral Contraception Tier 1 Service'"
  result<-dbSendQuery(con,sql)
  OCT1_reg<-dbFetch(result)
  dbClearResult(result)
  
  OCT1_reg1<- OCT1_reg%>%
    filter(`DateReported`<'2023-12-01')%>%
    filter(`DateReported`==max(`DateReported`))%>%
    mutate(`RegistrationDate`=as.character(`RegistrationDate`))%>%
    mutate(`Month`= paste0(substr(`RegistrationDate`,1,8), "01"))%>%
    mutate(`Month`= as.Date(`Month`, "%Y-%m-%d"))%>%
    select(`Fcode`=`FCode`,`DateReported`)%>% collect()
  
  OCT1_reg2<- OCT1_reg%>%
    filter(`DateReported`>='2023-12-01')%>%
    mutate(`RegistrationDate`=as.character(`RegistrationDate`))%>%
    mutate(`Month`= paste0(substr(`RegistrationDate`,1,8), "01"))%>%
    mutate(`Month`= as.Date(`Month`, "%Y-%m-%d"))%>%
    select(`Fcode`=`FCode`,`DateReported`)%>% collect()
  
  OCT1_reg=rbind(OCT1_reg2,OCT1_reg1)  
}
contraception_registrations=contraception_registrations()
contraception_registrations <- contraception_registrations %>%
  filter(DateReported==max(DateReported))%>%
  select(ODS.CODE = `Fcode`)

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

#get latest pharm list - this will take a while as it's getting the coordinates for every pharmacy on the list
#N.B. this also saves the pharm list data in the R project so that the app can access it once it's deployed
pharmlist <- update_pharm_list()

pharmlist<- pharmlist %>% left_join(phone_number,by='ODS.CODE')


pharmlist <- pharmlist %>%
  mutate(`Signed up to SCS` = if_else(ODS.CODE %in% smoking_registrations$ODS.CODE, 'YES', 'NO'),
         `Signed up to CPCS` = if_else(ODS.CODE %in% cpcs_registrations$ODS.CODE, 'YES', 'NO'),
         `Signed up to contraception services` = if_else(ODS.CODE %in% contraception_registrations$ODS.CODE, 'YES', 'NO'),
         `Signed up to BP checks` = if_else(ODS.CODE %in% blood_pressure_check_registrations$ODS.CODE, 'YES', 'NO'),
         `Signed up to NMS` = if_else(ODS.CODE %in% nms_registrations$ODS.CODE, 'YES', 'NO'),
         `Signed up to TLHC` = if_else(ODS.CODE %in% tlhc_registrations$ODS.CODE, 'YES', 'NO'),)

saveRDS(pharmlist, "nearest-pharmacy-app/pharmlist.rds")
