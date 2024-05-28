#PACKAGES USED
library(tidyverse)
library(readxl)

#SET WORKING DIRECTORY

#Set it to where the BRASMOD folder is

file_directory <- dirname(rstudioapi::getSourceEditorContext()$path)

BRASMOD_directory <- gsub("/Database setup", "", file_directory)

setwd(BRASMOD_directory)

#CHOOSE YEAR AND FORMAL/INFORMAL SECTOR

year <- 2008

#This will be important for determining taxes on consumption
#If set to "no", informal sector expenditures will be "hidden"
#This is done to induce them not paying taxes
include_informal_sector <- "no" 

#Read tables from POF data folder

MORADOR <- readRDS(paste0("Database setup\\POF data\\", as.character(year), "\\MORADOR_", 
                          as.character(year), ".rds"))

RENDIMENTO_TRABALHO <- readRDS(paste0("Database setup\\POF data\\", as.character(year), "\\RENDIMENTO_TRABALHO_", 
                                      as.character(year), ".rds"))

OUTROS_RENDIMENTOS <- readRDS(paste0("Database setup\\POF data\\", as.character(year), "\\OUTROS_RENDIMENTOS_", 
                                     as.character(year), ".rds"))


#DATABASE SETUP

#PART 1: ADD MANDATORY VARIABLES

#IDs


#We create household and individual IDs based on POF variables,
#then sort them
base_ids <- MORADOR %>% 
  mutate(idorighh = paste0(COD_UPA, NUM_DOM, NUM_UC),
         idorigperson = paste0(idorighh, COD_INFORMANTE)) %>% 
  group_by(idorighh) %>% 
  mutate(idhh = cur_group_id(), #the new household ID is the group's ID
         #the individual ID is just the new idhh + "0" + the row number within the household
         #so first individual in HH 1 is 101, second individual is 102, etc
         idperson = paste0(idhh, "0", row_number())) %>% 
  ungroup(idorighh) %>% 
  arrange(as.numeric(idhh)) %>% 
  select(idhh, idperson, everything())


#Create the variable idhead, being equal to idperson if the individual is the 
#household head, and 0 if not; we then group by household and give the head's
#idperson as idhead to everyone. 

base_ids <- base_ids %>% 
  mutate(idhead = ifelse(COND_UNIDADE_CONSUMO == 1,
                         yes = as.numeric(base_ids$idperson),
                         no = 0)) %>%
  group_by(idhh) %>% 
  mutate(idhead = as.character(max(idhead))) %>% 
  ungroup(idhh)



base_ids <- base_ids %>% 
  mutate(malehead = ifelse(V0405 == 1 & COND_UNIDADE_CONSUMO == 1, #identifies male heads
                           yes = 1,
                           no = 0),
         femalehead = ifelse(V0405 == 2 & COND_UNIDADE_CONSUMO == 1, #dentifies female heads
                             yes = 1,
                             no = 0)) %>% 
  group_by(idhh) %>% 
  mutate(malehead = max(malehead), femalehead = max(femalehead))

#Just to check, the following cross product should be 0
#print(as.vector(base_ids$malehead) %*% as.vector(base_ids$femalehead))

base_ids <- base_ids %>% 
  mutate(idmalehead = ifelse(V0405 == 1 & 
                               COND_UNIDADE_CONSUMO == 1,
                             yes = as.numeric(idperson),
                             no = 0),
         idfemalehead = ifelse(V0405 == 2 & 
                                 COND_UNIDADE_CONSUMO == 1,
                               yes = as.numeric(idperson),
                               no = 0)) %>% 
  group_by(idhh) %>% 
  mutate(idmalehead = as.character(max(idmalehead)),
         idfemalehead = as.character(max(idfemalehead))) %>% 
  ungroup(idhh)


#Identify household members with respect to head
base_ids <- base_ids %>% 
  mutate(idpartner = ifelse(COND_UNIDADE_CONSUMO == 2, #if head's partner
                            yes = idhead,
                            no = "0"),
         idfather = ifelse(
           malehead == 1 & COND_UNIDADE_CONSUMO == 3, #if head's child and head is a man
           yes = idmalehead,
           no = "0"),
         idmother = ifelse(
           femalehead == 1 & COND_UNIDADE_CONSUMO == 3, #if head's child and head is a woman
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

#Create sample weight (dwt), age (dag), gender (gdn), race (dra),
#and urban region (drgur) variables

base_ids <- base_ids %>% 
  rename(dwt   = PESO_FINAL,
         dag   = IDADE_ANOS,
         drgur = TIPO_SITUACAO_REG,
         dra   = V0429) %>% 
  mutate(dgn   = ifelse(V0405 == 1,
                        yes = 1,
                        no  = 0))

#Create marital status (dms) variable. With the PNADc, we're only able to capture 
#if the individual has a partner or not
base_ids$dms <- ifelse(base_ids$idpartner != 0, #has a partner
                       yes = 2, #"married"
                       no = 1) #single

#Create region NUTS level 1 (drgn1) and 2 (drgn2) variables
#We consider that level 1 regions are the IBGE-defined regions in BR (North, Northeast, South, Southeast and Central-West),
#and that level 2 is the states

base_drgn <- base_ids %>% 
  mutate(drgn1 = substr(COD_UF, 1, 1),
         drgn2 = substr(COD_UF, 1, 2))

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
  mutate(dec = case_when(is.na(V0420) ~ 0,
                         V0420 == 0 ~ 0,
                         V0420 == 1 ~ 1,
                         V0420 == 2 ~ 1,
                         V0420 == 3 ~ 2,
                         V0420 == 4 ~ 2,
                         (V0420 == 5 & dag < 12) ~ 2,
                         (V0420 == 5 & dag >= 12) ~ 3,
                         V0420 == 6 ~ 3,
                         V0420 == 7 ~ 4,
                         V0420 == 8 ~ 4,
                         V0420 == 9 ~ 5,
                         V0420 == 10 ~ 4,
                         V0420 == 11 ~ 6,
                         V0420 == 12 ~ 6,
                         V0420 == 13 ~ 6))

#Create years of education (dey) variable 

base_dey <- base_dec %>% 
  mutate(dey = ANOS_ESTUDO)

#Create highest education status (deh) variable

base_deh <- base_dey %>% 
  mutate(deh = case_when((is.na(V0425) & dec == 0) ~ 0,
                         (is.na(V0425) & dec != 0) ~ dec,
                         V0425 == 0 ~ 0,
                         V0425 == 1 ~ 0,
                         V0425 == 2 ~ 1,
                         V0425 == 3 ~ 2,
                         V0425 == 4 ~ 2,
                         V0425 == 5 ~ 2,
                         V0425 == 6 ~ 3,
                         V0425 == 7 ~ 4,
                         V0425 == 8 ~ 3,
                         V0425 == 9 ~ 3,
                         V0425 == 10 ~ 4,
                         V0425 == 11 ~ 4,
                         V0425 == 12 ~ 5,
                         V0425 == 13 ~ 4,
                         V0425 == 14 ~ 6,
                         V0425 == 15 ~ 6,
                         V0425 == 16 ~ 6))

#LABOUR

#Now we have to merge our current table with 
#labour status information from RENDIMENTO_TRABALHO


#Codes for labour earnings
EMPLOYED <- c(53001, 53002, 53003, 53004, 54015, 54018, 54020, 54036, 54037, 
              54038, 55001, 55002, 55011, 55026, 55035, 55037, 55038, 55039,
              55040, 55041, 55042, 55043, 55045, 55062, 55063)
SELF_EMPLOYED <- c(53005, 55003)
EMPLOYER <- 53006


LABOUR_CODES <- c(EMPLOYED, SELF_EMPLOYED, EMPLOYER)

#Take only first 5 digits of codes
RENDIMENTO_TRABALHO$V9001 <- substr(RENDIMENTO_TRABALHO$V9001, 1, 5)

labour_status <- aggregate(
  (RENDIMENTO_TRABALHO %>% filter(COD_TIPO_OCUP == 1))$V5303[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES],
  by = list(COD_UPA = (RENDIMENTO_TRABALHO %>% filter(COD_TIPO_OCUP == 1))$COD_UPA[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES],
            NUM_DOM = (RENDIMENTO_TRABALHO %>% filter(COD_TIPO_OCUP == 1))$NUM_DOM[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES],
            NUM_UC = (RENDIMENTO_TRABALHO %>% filter(COD_TIPO_OCUP == 1))$NUM_UC[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES],
            COD_INFORMANTE = (RENDIMENTO_TRABALHO %>% filter(COD_TIPO_OCUP == 1))$COD_INFORMANTE[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES]),
  FUN = function(x) paste(x[!is.na(x)], collapse = ""))

base_les <- merge(base_deh,
                  labour_status,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T) %>% 
  rename(les = x)

#Public sector workers

base_lpb <- base_les %>% 
  mutate(lpb = ifelse(!is.na(les) & les == 2,
                      yes = 1,
                      no = 0))

#Domestic workers

base_ldt <- base_lpb %>% 
  mutate(ldt = ifelse(!is.na(les) & (les == 3),
                      yes = 1,
                      no = 0))

#Entrepreneurs

base_los <- base_ldt %>% 
  mutate(los = ifelse(!is.na(les) & (les == 5),
                      yes = 1,
                      no = 0))

#Self-employed

base_lse <- base_los %>% 
  mutate(lse = ifelse(!is.na(les) & (les == 6),
                      yes = 1,
                      no  = 0))

#Now we adjust the "les" variable to the categorization in EUROMOD's DRD

#Create labour status (les) variable:
#0: Pre-school 
#1: Farmer 
#2: Employer or self-employed 
#3: Employee 
#4: Pensioner 
#5: Unemployed 
#6: Student 
#7: Inactive 
#8: Sick or disabled 
#9: Other 

base_les2 <- base_lse %>% 
  mutate(les = case_when(les == 3 ~ 3,
                         les == 4 ~ 1,
                         les == 6 ~ 2,
                         les == 2 ~ 3,
                         les == 5 ~ 2,
                         les == 1 ~ 3,
                         les == 7 ~ 6,
                         les == 8 ~ 9,
                         les == 9 ~ 1,
                         (is.na(les) & dec > 1) ~ 6,
                         (is.na(les) & dag >= 14 & dec <= 0) ~ 7,
                         (is.na(les) & dag < 14 & dec <= 0) ~ 9,
                         (is.na(les) & dec == 1) ~ 0))


#Get ISCO occupation codes and create labour ocupation (loc) variable based on 1st digit

labour_ocupation <- RENDIMENTO_TRABALHO %>% 
  filter(COD_TIPO_OCUP == 1 & V9001 %in% LABOUR_CODES) %>%
  mutate(loc = as.numeric(substr(V53011, 1,1))) %>% 
  select(c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE", loc))

base_loc <- merge(base_les2,
                  labour_ocupation,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T) %>% 
  mutate(loc = ifelse(!is.na(loc),
                      yes = loc,
                      no  = 0))

#Get weekly hours worked

labour_hours <- RENDIMENTO_TRABALHO %>% 
  filter(V53041 < 999 & COD_TIPO_OCUP == 1) %>% #999 means no reporting
  mutate(weekly_hours_times_months = V53041*V9011) %>% 
  group_by(COD_UPA,NUM_DOM,NUM_UC, COD_INFORMANTE) %>% 
  summarise(lhw = sum(weekly_hours_times_months)/12) #maximum possible value reported is 120h/w

base_lhw <- merge(base_loc,
                  labour_hours,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T)


#INCOME

#Substitute NAs in "# of months earned" for 1 
RENDIMENTO_TRABALHO$V9011[is.na(RENDIMENTO_TRABALHO$V9011)] <- 1

#Annualize and correct earnings for inflation
RENDIMENTO_TRABALHO[, c('V8500','V8500_DEFLA')] <- apply(
  RENDIMENTO_TRABALHO[, c('V8500','V8500_DEFLA')],2,
  function(vetor) (vetor * RENDIMENTO_TRABALHO$V9011 * RENDIMENTO_TRABALHO$FATOR_ANUALIZACAO)/12)

#Aggregate employment earnings by individuals

labour_employed <- aggregate(
  RENDIMENTO_TRABALHO$V8500_DEFLA[RENDIMENTO_TRABALHO$V9001 %in% EMPLOYED],
  by = list(COD_UPA = RENDIMENTO_TRABALHO$COD_UPA[RENDIMENTO_TRABALHO$V9001 %in% EMPLOYED],
            NUM_DOM = RENDIMENTO_TRABALHO$NUM_DOM[RENDIMENTO_TRABALHO$V9001 %in% EMPLOYED],
            NUM_UC = RENDIMENTO_TRABALHO$NUM_UC[RENDIMENTO_TRABALHO$V9001 %in% EMPLOYED],
            COD_INFORMANTE = RENDIMENTO_TRABALHO$COD_INFORMANTE[RENDIMENTO_TRABALHO$V9001 %in% EMPLOYED]),
  FUN = sum, 
  na.rm = T) %>% 
  rename(yem = x) #yem for "income" (y) and "employment"

base_yem <- merge(base_lhw,
                  labour_employed,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T)


#Aggregate self-employment earnings by individual

labour_self_employed <- aggregate(
  RENDIMENTO_TRABALHO$V8500_DEFLA[RENDIMENTO_TRABALHO$V9001 %in% c(SELF_EMPLOYED, EMPLOYER)],
  by = list(COD_UPA = RENDIMENTO_TRABALHO$COD_UPA[RENDIMENTO_TRABALHO$V9001 %in% c(SELF_EMPLOYED, EMPLOYER)],
            NUM_DOM = RENDIMENTO_TRABALHO$NUM_DOM[RENDIMENTO_TRABALHO$V9001 %in% c(SELF_EMPLOYED, EMPLOYER)],
            NUM_UC = RENDIMENTO_TRABALHO$NUM_UC[RENDIMENTO_TRABALHO$V9001 %in% c(SELF_EMPLOYED, EMPLOYER)],
            COD_INFORMANTE = RENDIMENTO_TRABALHO$COD_INFORMANTE[RENDIMENTO_TRABALHO$V9001 %in% c(SELF_EMPLOYED, EMPLOYER)]),
  FUN = sum, 
  na.rm = T) %>% 
  rename(yse = x) #yse for "income" (y) and "self-employment"

base_yse <- merge(base_yem,
                  labour_self_employed,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T)



#Aggregate investment earnings by individuals

#Codes from the translator for investment
INVESTMENTS <- c(55005, 55006, 55008, 55010, 55014, 55044)

#Substitute NAs in "# of months earned" for 1 
OUTROS_RENDIMENTOS$V9011[is.na(OUTROS_RENDIMENTOS$V9011) | OUTROS_RENDIMENTOS$V9011 == 0] <- 1

#Get only first 5 digits of codes
OUTROS_RENDIMENTOS$V9001 <- substr(OUTROS_RENDIMENTOS$V9001, 1, 5)

#Annualize and correct earnings for inflation
OUTROS_RENDIMENTOS[, c('V8500','V8500_DEFLA')] <- apply(
  OUTROS_RENDIMENTOS[, c('V8500','V8500_DEFLA')],2,
  function(vetor) (vetor * OUTROS_RENDIMENTOS$V9011 * OUTROS_RENDIMENTOS$FATOR_ANUALIZACAO)/12)

investiment_earnings <- aggregate(
  OUTROS_RENDIMENTOS$V8500_DEFLA[OUTROS_RENDIMENTOS$V9001 %in% INVESTMENTS],
  by = list(COD_UPA = OUTROS_RENDIMENTOS$COD_UPA[OUTROS_RENDIMENTOS$V9001 %in% INVESTMENTS],
            NUM_DOM = OUTROS_RENDIMENTOS$NUM_DOM[OUTROS_RENDIMENTOS$V9001 %in% INVESTMENTS],
            NUM_UC = OUTROS_RENDIMENTOS$NUM_UC[OUTROS_RENDIMENTOS$V9001 %in% INVESTMENTS],
            COD_INFORMANTE = OUTROS_RENDIMENTOS$COD_INFORMANTE[OUTROS_RENDIMENTOS$V9001 %in% INVESTMENTS]),
  FUN = sum, 
  na.rm = T) %>% 
  rename(yiy = x) #yiy for "income" (y) and "investment"


base_yiy <- merge(base_yse,
                  investiment_earnings,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T)


#Household total  income;

base_yhh <- base_yiy %>% 
  group_by(idhh) %>% 
  mutate(yhh = sum(as.numeric(yse), #total household income
                   as.numeric(yem), 
                   as.numeric(yiy), na.rm = T)) %>% 
  ungroup(idhh)

#OTHER INCOME SOURCES

#Add unemployment benefit (bun), old age pension/retirement (poa),
#poor elderly/disabled benefit (bdioa), private transfers (ypt) and
#income from rental of property (yprrt)

#Codes for all sort of old age pensions
PENSIONS_OLD_AGE <- c(54001, 54002, 55022, 55023, 55064,
                      54003, 54004, 55065, 54005, 54023, 55025,
                      55033, 55066)

pensions_old_age_aggregate <- aggregate(
  OUTROS_RENDIMENTOS$V8500_DEFLA[OUTROS_RENDIMENTOS$V9001 %in% PENSIONS_OLD_AGE],
  by = list(COD_UPA = OUTROS_RENDIMENTOS$COD_UPA[OUTROS_RENDIMENTOS$V9001 %in% PENSIONS_OLD_AGE],
            NUM_DOM = OUTROS_RENDIMENTOS$NUM_DOM[OUTROS_RENDIMENTOS$V9001 %in% PENSIONS_OLD_AGE],
            NUM_UC = OUTROS_RENDIMENTOS$NUM_UC[OUTROS_RENDIMENTOS$V9001 %in% PENSIONS_OLD_AGE],
            COD_INFORMANTE = OUTROS_RENDIMENTOS$COD_INFORMANTE[OUTROS_RENDIMENTOS$V9001 %in% PENSIONS_OLD_AGE]),
  FUN = sum, 
  na.rm = T) %>% 
  rename(poa = x) #poa for "pension old age"

base_poa <- merge(base_yhh,
                  pensions_old_age_aggregate,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T)

#Codes for unemployment benefits
UNEMPLOYMENT <-  c(55017, 55017)

unemployment_aggregate <- aggregate(
  OUTROS_RENDIMENTOS$V8500_DEFLA[OUTROS_RENDIMENTOS$V9001 %in% UNEMPLOYMENT],
  by = list(COD_UPA = OUTROS_RENDIMENTOS$COD_UPA[OUTROS_RENDIMENTOS$V9001 %in% UNEMPLOYMENT],
            NUM_DOM = OUTROS_RENDIMENTOS$NUM_DOM[OUTROS_RENDIMENTOS$V9001 %in% UNEMPLOYMENT],
            NUM_UC = OUTROS_RENDIMENTOS$NUM_UC[OUTROS_RENDIMENTOS$V9001 %in% UNEMPLOYMENT],
            COD_INFORMANTE = OUTROS_RENDIMENTOS$COD_INFORMANTE[OUTROS_RENDIMENTOS$V9001 %in% UNEMPLOYMENT]),
  FUN = sum, 
  na.rm = T) %>% 
  rename(bun = x)

base_bun <- merge(base_poa,
                  unemployment_aggregate,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T)


#BPC is an old age/disabled benefit stands for "Benefício de Prestação Continuada"
BPC <- c(54011)

bpc_aggregate <- aggregate(
  OUTROS_RENDIMENTOS$V8500_DEFLA[OUTROS_RENDIMENTOS$V9001 %in% BPC],
  by = list(COD_UPA = OUTROS_RENDIMENTOS$COD_UPA[OUTROS_RENDIMENTOS$V9001 %in% BPC],
            NUM_DOM = OUTROS_RENDIMENTOS$NUM_DOM[OUTROS_RENDIMENTOS$V9001 %in% BPC],
            NUM_UC = OUTROS_RENDIMENTOS$NUM_UC[OUTROS_RENDIMENTOS$V9001 %in% BPC],
            COD_INFORMANTE = OUTROS_RENDIMENTOS$COD_INFORMANTE[OUTROS_RENDIMENTOS$V9001 %in% BPC]),
  FUN = sum, 
  na.rm = T) %>% 
  rename(bdioa = x)

base_bdioa <- merge(base_bun,
                    bpc_aggregate,
                    by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                    by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                    all.x = T)

#Private transfers (donations)

DONATIONS <- c(54032)

donations_aggregate <- aggregate(
  OUTROS_RENDIMENTOS$V8500_DEFLA[OUTROS_RENDIMENTOS$V9001 %in% DONATIONS],
  by = list(COD_UPA = OUTROS_RENDIMENTOS$COD_UPA[OUTROS_RENDIMENTOS$V9001 %in% DONATIONS],
            NUM_DOM = OUTROS_RENDIMENTOS$NUM_DOM[OUTROS_RENDIMENTOS$V9001 %in% DONATIONS],
            NUM_UC = OUTROS_RENDIMENTOS$NUM_UC[OUTROS_RENDIMENTOS$V9001 %in% DONATIONS],
            COD_INFORMANTE = OUTROS_RENDIMENTOS$COD_INFORMANTE[OUTROS_RENDIMENTOS$V9001 %in% DONATIONS]),
  FUN = sum, 
  na.rm = T) %>% 
  rename(ypt = x)

base_ypt <- merge(base_bdioa,
                  donations_aggregate,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T)


#Income from rental of property

RENT <- c(54008)

rent_aggregate <- aggregate(
  OUTROS_RENDIMENTOS$V8500_DEFLA[OUTROS_RENDIMENTOS$V9001 %in% RENT],
  by = list(COD_UPA = OUTROS_RENDIMENTOS$COD_UPA[OUTROS_RENDIMENTOS$V9001 %in% RENT],
            NUM_DOM = OUTROS_RENDIMENTOS$NUM_DOM[OUTROS_RENDIMENTOS$V9001 %in% RENT],
            NUM_UC = OUTROS_RENDIMENTOS$NUM_UC[OUTROS_RENDIMENTOS$V9001 %in% RENT],
            COD_INFORMANTE = OUTROS_RENDIMENTOS$COD_INFORMANTE[OUTROS_RENDIMENTOS$V9001 %in% RENT]),
  FUN = sum, 
  na.rm = T) %>% 
  rename(yprrt = x)

base_yprrt <- merge(base_ypt,
                    rent_aggregate,
                    by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                    by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                    all.x = T)

#OTHER VARIABLES

#Disabilities

#There is no information regarding disabled persons in POF
#The best we can do is use BPC's eligibility criteria to identify
#some disabled persons: if someone is less than 65 and received BPC,
#they have to be disabled

base_ddi <- base_yprrt %>% 
  mutate(ddi = ifelse(!is.na(bdioa) & bdioa > 0 & dag < 65,
                      yes = 1,
                      no = 0))

#Formal employment

#The old POF (2008-2009) has no information on formal employment.
#We use SICs > 0 as a proxy for that

SIC <- c(53501, 53502, 53503, 53504, 53505)

formal_employment <- RENDIMENTO_TRABALHO %>% 
  group_by(COD_UPA,NUM_DOM,NUM_UC, COD_INFORMANTE) %>% 
  summarise(lem = ifelse(any(V53061 > 0),
                         yes = 1,
                         no = 0))

base_lem <- merge(base_ddi,
                  formal_employment,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T)

#EXPENDITURE VARIABLES

#Read expenditure-related tables

DESPESA_INDIVIDUAL <- readRDS(paste0("Database setup\\POF data\\", as.character(year), "\\DESPESA_INDIVIDUAL_", 
                                     as.character(year), ".rds"), col_character()) %>% 
  mutate(V9001 = str_sub(V9001, 1, -3)) %>%
  mutate(V9004 = case_when(nchar(V9004) == 3 ~ paste0("00", V9004),
                           nchar(V9004) == 4 ~ paste0("0", V9004),
                           V9004        == 0 ~ as.character(999),
                           TRUE ~ as.character(V9004))) %>% 
  mutate(idorighh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>% 
  filter(as.numeric(V9002) <= 6) %>% 
  mutate(V8000_DEFLA_new = (V8000_DEFLA*FATOR_ANUALIZACAO)/12) %>% 
  mutate(TOR = substr(V9004, 1, 3)) %>% 
  select(idorighh, V9001, V8000_DEFLA_new, TOR)

DESPESA_90DIAS <- readRDS(paste0("Database setup\\POF data\\", as.character(year), "\\DESPESA_90DIAS_", 
                             as.character(year), ".rds")) %>% 
  mutate(V9001 = str_sub(V9001, 1, -3)) %>%
  mutate(V9004 = case_when(nchar(V9004) == 3 ~ paste0("00", V9004),
                           nchar(V9004) == 4 ~ paste0("0", V9004),
                           V9004        == 0 ~ as.character(999),
                           TRUE ~ as.character(V9004))) %>% 
  mutate(idorighh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>% 
  filter(as.numeric(V9002) <= 6) %>% 
  mutate(V8000_DEFLA_new = (V8000_DEFLA*FATOR_ANUALIZACAO)/12) %>% 
  mutate(TOR = substr(V9004, 1, 3)) %>% 
  select(idorighh, V9001, V8000_DEFLA_new, TOR)

DESPESA_12MESES <- readRDS(paste0("Database setup\\POF data\\", as.character(year), "\\DESPESA_12MESES_", 
                                 as.character(year), ".rds")) %>% 
  mutate(V9001 = str_sub(V9001, 1, -3)) %>% 
  mutate(V9004 = case_when(nchar(V9004) == 3 ~ paste0("00", V9004),
                           nchar(V9004) == 4 ~ paste0("0", V9004),
                           V9004        == 0 ~ as.character(999),
                           TRUE ~ as.character(V9004))) %>% 
  mutate(idorighh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>% 
  mutate(V9011 = ifelse(is.na(V9011) | V9011 == 0, 1, V9011)) %>% 
  filter(as.numeric(V9002) <= 6) %>% 
  mutate(V8000_DEFLA_new = (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12) %>% 
  mutate(TOR = substr(V9004, 1, 3)) %>% 
  select(idorighh, V9001, V8000_DEFLA_new, TOR)

CADERNETA_COLETIVA <- readRDS(paste0("Database setup\\POF data\\", as.character(year), "\\CADERNETA_COLETIVA_", 
                                  as.character(year), ".rds")) %>% 
  mutate(V9001 = str_sub(V9001, 1, -3)) %>% 
  mutate(V9004 = case_when(nchar(V9004) == 3 ~ paste0("00", V9004),
                           nchar(V9004) == 4 ~ paste0("0", V9004),
                           V9004        == 0 ~ as.character(999),
                           TRUE ~ as.character(V9004))) %>% 
  mutate(idorighh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>% 
  filter(as.numeric(V9002) <= 6) %>% 
  mutate(V8000_DEFLA_new = (V8000_DEFLA*FATOR_ANUALIZACAO)/12) %>%
  mutate(TOR = substr(V9004, 1, 3)) %>% 
  select(idorighh, V9001, V8000_DEFLA_new, TOR)

#Using the same classification as in Bachas, Gadenne and Jensen (2023)
#See https://github.com/pierrebachas/Informality_Taxes_Redistribution

crosswalk_TOR <- read_xlsx("Database setup\\POF data\\2008\\crosswalk_pof_TOR_2008.xlsx") %>% 
  mutate(TOR_final = case_when(TOR_original %in% c(18,7,17,16)                ~ 1,   #Non-market
                               TOR_original %in% c(29,26,14,6,27)             ~ 2,   #No store front
                               TOR_original %in% c(8)                         ~ 3,   #Convenience and corner shops
                               TOR_original %in% c(28,33,19)                  ~ 4,   #Specialized shops
                               TOR_original %in% c(30,4)                      ~ 5,   #Large stores
                               TOR_original %in% c(21,22,15,13,5,1,23,9,3,12) ~ 6,   #Institutions
                               TOR_original %in% c(20)                        ~ 7,   #Service from individual
                               TOR_original %in% c(25,10)                     ~ 8,   #Entertainement
                               TOR_original %in% c(2,24)                      ~ 9,   #Informal entertainement
                               TOR_original %in% c(31,32)                     ~ 99))  %>% #Unspecified
  select(local_3dig, TOR_final)

tables_pof <- do.call(rbind,
                      list(DESPESA_INDIVIDUAL,
                           DESPESA_90DIAS,
                           DESPESA_12MESES,
                           CADERNETA_COLETIVA)) 

tables_pof <- merge(tables_pof,
                    crosswalk_TOR,
                    by.x = "TOR",
                    by.y = "local_3dig",
                    all.x = T) %>% 
  mutate(formal = ifelse(TOR_final %in% c(1,2,3,7,9),
                         yes = 0,
                         no  = 1))

#Health expenditures

#Health expenditure codes
SAUDE <- c(29001:29049, 29300, 29999,
           42001:42003, 42044, 42045,
           42023, 42024,
           42009:42012,
           42004:42008, 42020:42022, 42025, 42028:42034, 42047,
           42005,
           42006,
           42013:42019, 42043, 42026,
           29301:29303, 29305, 29309, 29311, 29312, 42039:42031,
           29304, 29310, 29313, 42007, 42026, 42027, 42035:42038, 42042, 42999)

pof_xhl <- tables_pof %>% 
  filter(V9001 %in% SAUDE) %>% 
  group_by(idorighh) %>% 
  summarise(xhl = sum(V8000_DEFLA_new))

base_xhl <- merge(base_lem,
                  pof_xhl,
                  by.x = "idorighh",
                  by.y = "idorighh",
                  all.x = T) %>% 
  group_by(idhh) %>% #To avoid double counting, we assign all expenditures to head
  mutate(xhl = ifelse(idperson == idhead,      
                      yes = replace_na(xhl, 0),
                      no = 0))

#Education expenditure codes
EDUCACAO <- c(49001, 49031, 49032,
              49033,
              28055, 49002, 49003, 49011, 49015, 49022, 49034, 49041, 49043, 49044,
              49047, 49049, 49052, 49054, 49084, 49086:49092,
              49006:49008, 49045,
              32001, 32002, 32999, 49019, 49021, 49025, 49029,
              48003, 48013, 48036, 49003, 49005, 49009, 49010, 49012:49014, 49016:49018,
              49024, 49027, 49028, 49030, 49042, 49046, 49048, 49053, 49085, 49093, 49999)

pof_xed <- tables_pof %>% 
  filter(V9001 %in% EDUCACAO) %>% 
  group_by(idorighh) %>% 
  summarise(xed = sum(V8000_DEFLA_new))

base_xed <- merge(base_xhl,
                  pof_xed,
                  by.x = "idorighh",
                  by.y = "idorighh",
                  all.x = T) %>% 
  group_by(idhh) %>% #To avoid double counting, we assign all expenditures to head
  mutate(xed = ifelse(idperson == idhead,      
                      yes = replace_na(xed, 0),
                      no = 0))

#Private pension expenditure codes
PREVIDENCIA_PRIV <- c(48006)

pof_xpp <- tables_pof %>% 
  filter(V9001 %in% PREVIDENCIA_PRIV) %>% 
  group_by(idorighh) %>% 
  summarise(xpp = sum(V8000_DEFLA_new))

base_xpp <- merge(base_xed,
                  pof_xpp,
                  by.x = "idorighh",
                  by.y = "idorighh",
                  all.x = T) %>% 
  group_by(idhh) %>% #To avoid double counting, we assign all expenditures to head
  mutate(xpp = ifelse(idperson == idhead,      
                      yes = replace_na(xpp, 0),
                      no = 0))

#Alimony expenditure codes
PENSAO_ALIM <- c(48005)

pof_xmp <- tables_pof %>% 
  filter(V9001 %in% PENSAO_ALIM) %>% 
  group_by(idorighh) %>% 
  summarise(xmp = sum(V8000_DEFLA_new))

base_xmp <- merge(base_xpp,
                  pof_xmp,
                  by.x = "idorighh",
                  by.y = "idorighh",
                  all.x = T) %>% 
  group_by(idhh) %>% #To avoid double counting, we assign all expenditures to head
  mutate(xmp = ifelse(idperson == idhead,      
                      yes = replace_na(xmp, 0),
                      no = 0))

#Get expenditure variables categorized by the System of National Accounts
#We'll use those to apply effective tax rates based on Siqueira et al. (2010)

crosswalk_pof_nat_acc <- read_xls("Database setup\\POF data\\2008\\crosswalk_pof_national-accounts_2008.xls") %>% 
  mutate(code_pof = substr(code_pof, 1, 5))

codes_nat_acc <- unique(crosswalk_pof_nat_acc$code_nat_acc_2005)

#We'll create a dataframe that will have a column for idhh,
#and then one for every expenditure category in the National Accounts
expenditures_nat_acc <- base_xmp %>% 
  ungroup(idhh) %>% 
  select(idorighh) %>% 
  distinct()


#If informal sector should be ignored, such expenditures are set to 0 for the
#purposes of taxation
if(include_informal_sector == "no"){
  tables_pof <<- tables_pof %>% 
    mutate(V8000_DEFLA_new = V8000_DEFLA_new*formal)
}

#This loops goes through the National Account categories
#and aggregates total household expenditure in each category.
#We then add the result as a column in the dataframe above
for(code in codes_nat_acc){
  codes_pof_list <- unique(crosswalk_pof_nat_acc %>%   #Get POF codes from the crosswalk
                             filter(code_nat_acc_2005 == code) %>% 
                             pull(code_pof)) 
  
  expenditure_values <- tables_pof %>%       #Aggregate expenditures by household
    filter(V9001 %in% codes_pof_list) %>% 
    group_by(idorighh) %>% 
    summarise(x = sum(V8000_DEFLA_new))
  
  
  expenditures_nat_acc <- merge(expenditures_nat_acc, #Join aggregated expenditure values into dataframe created above
                                expenditure_values,
                                by.x = "idorighh",
                                by.y = "idorighh",
                                all.x = T)
  
  colnames(expenditures_nat_acc)[ncol(expenditures_nat_acc)] <- paste0("x", as.character(code)) #Rename column
}

#Merge expenditure variables with the rest of the data

base_nat_acc <- merge(base_xmp,
                      expenditures_nat_acc,
                      by.x = "idorighh",
                      by.y = "idorighh",
                      all.x = T)

#To avoid double counting, we assign all expenditures to head

base_nat_acc <- base_nat_acc %>% 
  mutate(across(starts_with("x"), ~ ifelse(idperson == idhead,      
                                           yes = replace_na(., 0),
                                           no = 0)))

#Select only variables for simulation

mandatory_vars <- c("idhh", "idperson", "idorighh", "idorigperson", "idfather", "idmother", "idpartner",
                    "dct", "dgn", "drgn1", "drgn2", "drgur", "dwt", "dag", "dms", "dec", "dey", "deh", "ddi", "dra",
                    "les", "lem", "lpb", "ldt", "los", "lse", "yem", "lhw", "loc",
                    "yse", "yiy", "yprrt", "poa", "bun", "bdioa", "ypt", "yhh",
                    "xmp", "xhl", "xpp", "xed")

expenditure_vars <- as.vector(colnames(base_nat_acc %>% select(starts_with("x"))))


base_final_pof <- base_nat_acc %>% 
  select(all_of(c(mandatory_vars, expenditure_vars))) %>% 
  arrange(idhh, idperson) %>% 
  mutate(across(everything(), as.character),
         across(everything(), ~replace_na(.x, "0")))


version_folder <- list.files(pattern = "^BRASMOD")

#Save base as a tab separated .txt 
if(include_informal_sector == "no"){
  write.table(base_final_pof, file=paste0(version_folder, "\\Input\\BR_2008_b2.txt"),
              quote=FALSE, sep='\t', row.names=FALSE)
} else{
  write.table(base_final_pof, file=paste0(version_folder, "\\Input\\BR_2008_b1.txt"),
              quote=FALSE, sep='\t', row.names=FALSE)
}









