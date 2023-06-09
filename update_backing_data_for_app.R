library(tidyverse)
#https://docs.ropensci.org/PostcodesioR/
#install.packages("PostcodesioR")
library(PostcodesioR)
library(DBI)
library(odbc)
library(readxl)

source("nearest-pharmacy-app/functions.R")

#get latest pharm list - this will take a while as it's getting the coordinates for every pharmacy on the list
#N.B. this also saves the pharm list data in the R project so that the app can access it once it's deployed
pharmlist <- update_pharm_list()
saveRDS(pharmlist, "nearest-pharmacy-app/pharmlist.rds")

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