#PACKAGES USED
library(tidyverse)
library(readxl)
library(PNADcIBGE)
library(data.table)

#SET WORKING DIRECTORY (usually the Output folder)
setwd("C:\\Users\\joao.perez\\Downloads\\brasmod\\brasmod\\Output")

#READ OUTPUT FILES

#Select year
year = "2020"

#Microdata output
std <- fread(paste0("bra_", year, "_std.txt"), header = TRUE)
#Household output
std_hh <- fread(paste0("bra_", year, "_std_hh.txt"), header = TRUE)
