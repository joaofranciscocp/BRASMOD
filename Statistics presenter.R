#PACKAGES USED
library(tidyverse)
library(readxl)
library(PNADcIBGE)
library(srvyr)
library(data.table)

#SET WORKING DIRECTORY (usually the Output folder)
setwd("C:\\Users\\joao.perez\\Downloads\\brasmod\\brasmod\\Output")

#READ OUTPUT FILES

#Select year
year = "2021"

#Microdata output
std <- fread(paste0("bra_", year, "_std.txt"), header = TRUE) %>%
  mutate(across(everything(), ~ as.numeric(gsub(",", ".", .)))) %>% 
  as_survey_design(ids = idperson, strata = dst, weights = dwt, nest = TRUE)

#Household output
std_hh <- fread(paste0("bra_", year, "_std_hh.txt"), header = TRUE) %>% 
  mutate(across(everything(), ~ as.numeric(gsub(",", ".", .)))) %>% 
  as_survey_design(ids = idhh, strata = dst, weights = dwt, nest = TRUE)

statistics_summary <- std %>%
  summarise(total_se_income = sum(yse))
