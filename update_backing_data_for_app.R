library(tidyverse)
#https://docs.ropensci.org/PostcodesioR/
#install.packages("PostcodesioR")
library(PostcodesioR)
library(DBI)
library(odbc)
library(readxl)

con <- dbConnect(odbc::odbc(), dsn="prodtest", timeout = 10)


################################################################################
pull_pharm_list <- function(){
  
  sql = "SELECT *
  FROM [CommunityPharmacy_Public].[Ref_PharmaceuticalList]"
  result <- dbSendQuery(con,sql)
  pharm_list <- dbFetch(result)
  dbClearResult(result)
  
  names(pharm_list) <- names(pharm_list) %>% make.names()
  
  #fix miscoded snapshot month
  pharm_list <- pharm_list %>%
    mutate(SnapshotMonth = as.Date(SnapshotMonth)) %>%
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

  sql="select  [ODS.CODE]=[FCode], MAX([FcodePhone]) AS [FcodePhone]
  FROM [CommunityPharmacy_Restricted].[Service_Registrations]
  GROUP BY 
    [FCode]"
  result<-dbSendQuery(con,sql)
  phone_number<-dbFetch(result)
  dbClearResult(result)
  phone_number
  
}
phone_number=phone_number()



#clean and save latest service data
smoking_registrations <- read_excel("smoking_registrations.xlsx")
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
  
  sql=" 
  SELECT [Month]=[Month(claim)]
      ,[Fcode]=[Pharmacy Code]  
      ,[ICB Code]=[STP Code] 
      ,[SetupFee]=[Set up fee]+[Set up fee adjustment]
  FROM [CommunityPharmacy_Public].[BloodPressureService_BSA_claims]
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

  sql="select  distinct [Service], [FCode], [RegistrationDate], [DateReported] 
  FROM [CommunityPharmacy_Restricted].[Service_Registrations]
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

#clean and save latest service data
pf <- function(){
  
  sql="SELECT [FCode]=[Pharmacy ODS Code (F-Code)]
      ,[PF_Flg]
  FROM [CommunityPharmacy_Restricted].[Ref_PharmList_for_PharmFirstReporting]
where [PF_Flg]='IN'"
  result<-dbSendQuery(con,sql)
  Master1<-dbFetch(result)
  dbClearResult(result)
  
  Master1
  
}
pf_registrations<-pf()
pf_registrations <- pf_registrations %>%
  select(ODS.CODE = `FCode`)

#nms_registrations <- read_excel("N:/_Everyone/Primary Care Group/registrations_data_for_app/nms_registrations.xlsx")
nms_registrations <- function(){
  
  sql="select  distinct [Service], [FCode],[DateReported] 
  FROM [CommunityPharmacy_Restricted].[Service_Registrations]
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


saveRDS(smoking_registrations, "nearest-pharmacy-app/smoking_registrations.rds")
saveRDS(blood_pressure_check_registrations, "nearest-pharmacy-app/blood_pressure_check_registrations.rds")
saveRDS(contraception_registrations, "nearest-pharmacy-app/contraception_registrations.rds")
saveRDS(pf_registrations, "nearest-pharmacy-app/pf_registrations.rds")
saveRDS(nms_registrations, "nearest-pharmacy-app/nms_registrations.rds")


#get latest pharm list - this will take a while as it's getting the coordinates for every pharmacy on the list
#N.B. this also saves the pharm list data in the R project so that the app can access it once it's deployed
pharmlist <- update_pharm_list()

pharmlist<- pharmlist %>% left_join(phone_number,by='ODS.CODE')


pharmlist <- pharmlist %>%
  mutate(`Signed up to SCS` = if_else(ODS.CODE %in% smoking_registrations$ODS.CODE, 'YES', 'NO'),
         `Signed up to PF` = if_else(ODS.CODE %in% pf_registrations$ODS.CODE, 'YES', 'NO'),
         `Signed up to contraception services` = if_else(ODS.CODE %in% contraception_registrations$ODS.CODE, 'YES', 'NO'),
         `Signed up to BP checks` = if_else(ODS.CODE %in% blood_pressure_check_registrations$ODS.CODE, 'YES', 'NO'),
         `Signed up to NMS` = if_else(ODS.CODE %in% nms_registrations$ODS.CODE, 'YES', 'NO'))

saveRDS(pharmlist, "nearest-pharmacy-app/pharmlist.rds")
