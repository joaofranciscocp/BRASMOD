library(tidyverse)
library(readxl)
library(data.table)
library(haven)

#SET WORKING DIRECTORY

#Set it to where the BRASMOD folder is

setwd("C:\\Users\\joao.perez\\Downloads\\brasmod\\brasmod")

#CHOOSE YEAR FOR PNAD SURVEY
year = 2015

pnad <- read_dta(paste0("Old PNAD data\\pes", as.character(year), ".dta"))

#PART 1: ADD MANDATORY VARIABLES

#IDs

#Create household and individual IDs from PNAD variables
base_ids <- pnad %>% 
  mutate(idorighh = paste0(v0102, v0103),
         idorigperson = paste0(idorighh, "0", v0301))
