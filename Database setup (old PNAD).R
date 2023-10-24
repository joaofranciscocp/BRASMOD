library(tidyverse)
library(readxl)
library(data.table)
library(haven)

#SET WORKING DIRECTORY

#Set it to where the BRASMOD folder is

setwd("C:\\Users\\joao.perez\\Downloads\\brasmod\\brasmod")

#CHOOSE YEAR FOR PNAD SURVEY
year = 2015

pnad <- read_dta(paste0("C:\\Users\\joao.perez\\Desktop\\Old PNAD data\\pes", as.character(year), ".dta"))

#PART 1: ADD MANDATORY VARIABLES

#IDs

#Create household and individual IDs from PNAD variables
base_ids <- pnad %>% 
  mutate(idorighh = paste0(v0102, v0103),
         idorigperson = paste0(idorighh, "0", v0301)) #For old PNADs, that doesn't really create a unique idperson, but it's good enough

base_ids <- base_ids %>% 
  group_by(idorighh) %>% #we group by household
  mutate(idhh = cur_group_id(), #the new household ID is the group's ID
         #the individual ID is just the new idhh + "0" + the row number within the household
         #so first individual in HH 1 is 101, second individual is 102, etc
         idperson = paste0(idhh, "0", row_number())) %>% 
  ungroup(idorighh)%>% 
  arrange(as.numeric(idhh)) #arrange in order


#Create the variable idhead, being equal to idperson if the individual is the 
#household head, and a huge number if not; we then group by household and give the head's
#idperson as idhead to everyone. #NOTE: some households have more than one head in PNAD.
#We consider the "true" head to be the one with the lowest idperson
#"VD2002" is the condition within household variable

base_ids <- base_ids %>% 
  mutate(idhead = ifelse(base_ids$v0401 == 1,
                         yes = as.numeric(base_ids$idperson),
                         no = 999999999999999999)) %>% #just so it's larger than any other ID
  group_by(idhh) %>% 
  mutate(idhead = as.character(min(idhead))) %>% 
  ungroup(idhh)


#Create two binary variables to identify male and female heads (considering there might be both),
#then group by household and identify if the household has a male or female head

base_ids <- base_ids %>% 
  mutate(malehead = ifelse(v0302 == 2 & 
                             v0401 == 1, #identifies male heads
                           yes = 1,
                           no = 0),
         femalehead = ifelse(v0302 == 4 & 
                               v0401 == 1, #identifies female heads
                             yes = 1,
                             no = 0)) %>% 
  group_by(idhh) %>% 
  mutate(malehead = max(malehead), femalehead = max(femalehead))


#Create idmalehead and idfemalehead in the same manner as idhead above
base_ids <- base_ids %>% 
  mutate(idmalehead = ifelse(v0302 == 2 & 
                               v0401 == 1,
                             yes = as.numeric(idperson),
                             no = 0),
         idfemalehead = ifelse(v0302 == 4 & 
                                 v0401 == 1,
                               yes = as.numeric(idperson),
                               no = 0)) %>% 
  group_by(idhh) %>% 
  mutate(idmalehead = as.character(max(idmalehead)),
         idfemalehead = as.character(max(idfemalehead))) %>% 
  ungroup(idhh)

#Identify household members with respect to head
base_ids <- base_ids %>% 
  mutate(idpartner = ifelse(v0401 == 2, #if head's partner
                            yes = idhead,
                            no = "0"),
         idfather = ifelse(
           malehead == 1 & 
             (v0401 == 3), #if head's child and head is a man
           yes = idmalehead,
           no = "0"),
         idmother = ifelse(
           femalehead == 1 & 
             (v0401 == 3), #if head's child and head is a woman
           yes = idfemalehead,
           no  = "0"))

#Make sure that the true head's idpartner be the ID of the individual whose partner
#is the head (it's ugly, but it works)
base_ids <- base_ids %>% 
  mutate(idpartnerofhead = ifelse(idpartner == idhead, #if the individual is partner of head
                                  yes = idperson, #idpartnerofhead is their ID
                                  no = "0")) %>% 
  group_by(idhh) %>%
  #give idpartnerofhead to everyone in household
  mutate(idpartnerofhead = as.character(max(as.numeric(idpartnerofhead)))) %>% 
  ungroup(idhh) %>%
  mutate(idpartner = ifelse(idperson == idhead, #if the individual is head
                            yes = idpartnerofhead, #their idpartner is idpartnerofhead
                            no = idpartner)) #if not, keep it as it is


#COUNTRY, WEIGHTS, AGE, GENDER, URBAN AND MARITAL STATUS

#Create country variable (dct) according to ISO-3166 (Brazil is 76)
base_ids$dct <- "76"


#Create sample weight (dwt), age (dag), urban (drgur) and gender (gdn) variables
base_ids <- base_ids %>% 
  mutate(dwt = v4729,
         dag = v8005,
         dgn = v0302) %>% 
  mutate(dgn = case_when(dgn == 2 ~ 1,
                         dgn == 4 ~ 2),
         drgur = ifelse(v4728 <= 3,
                        yes = 1,
                        no = 0))

#Create marital status (dms) variable. With the PNADc, we're only able to capture 
#if the individual has a partner or not
base_ids$dms <- ifelse(base_ids$idpartner != 0, #has a partner
                       yes = 2, #"married"
                       no = 1) #single

#Create region NUTS level 1 (drgn1) and 2 (drgn2) variables
#We consider that level 1 regions are the IBGE-defined regions in BR (North, Northeast, South, Southeast and Central-West),
#and that level 2 is the states

base_drgn <- base_ids %>% 
  mutate(drgn1 = substr(UF, 1, 1),
         drgn2 = substr(UF, 1, 2))


#EDUCATION

#Create education status (dec) variable:
#0: Not in education 
#1: Pre-primary education 
#2: Primary education  
#3: Lower secondary education 
#4: Upper-secondary education  
#5: Post-secondary education 
#6: Tertiary education 

base_dec <- base_drgn %>% 
  mutate(dec = case_when(is.na(v6003) ~ 0,
                         v6003 == 7 ~ 1,
                         v6003 == 9 ~ 1,
                         v6003 == 6 ~ 2,
                         v6003 == 8 ~ 2,
                         (v6003 == 1 & dag < 12) ~ 2,
                         (v6003 == 1 & dag >= 12) ~ 3,
                         v6003 == 2 ~ 4,
                         v6003 == 5 ~ 5,
                         v6003 == 11 ~ 6,
                         v6003 == 3 ~ 3,
                         v6003 == 4 ~ 4,
                         v6003 == 10 ~ 4))

#Create years of education (dey) variable 

base_dey <- base_dec %>% 
  mutate(dey = ifelse(!is.na(v4803),
                      yes = v4803,
                      no = 0))

#Create highest education status (deh) variable

base_deh <- base_dey %>% 
  mutate(deh = case_when((is.na(v4745) & dec == 0) ~ 0,
                         (is.na(v4745) & dec != 0) ~ dec,
                         v4745 == 1 ~ 0,
                         (v4745 == 2 & dey < 5) ~ 1,
                         (v4745 == 2 & dey >= 5) ~ 2,
                         v4745 == 3 ~ 3,
                         v4745 == 4 ~ 3,
                         v4745 == 5 ~ 4,
                         v4745 == 6 ~ 4,
                         (v4745 == 7 & dec <= 5) ~ 5,
                         (v4745 == 7 & dec > 5) ~ 6,
                         (v4745 == 8 & dec == 0) ~ 0,
                         (v4745 == 8 & dec != 0) ~ dec))
