#PACKAGES USED
library(tidyverse)
library(readxl)

#SET WORKING DIRECTORY

#Set it to where the BRASMOD folder is

file_directory <- dirname(rstudioapi::getSourceEditorContext()$path)

BRASMOD_directory <- gsub("/Database setup", "", file_directory)

setwd(BRASMOD_directory)

#GET POF DATA

#Read tables from POF data folder

MORADOR <- readRDS("Expenditure imputation\\POF data\\MORADOR.rds")

RENDIMENTO_TRABALHO <- readRDS("Expenditure imputation\\POF data\\RENDIMENTO_TRABALHO.rds")

OUTROS_RENDIMENTOS <- readRDS("Expenditure imputation\\POF data\\OUTROS_RENDIMENTOS.rds")

#DATA TRANSLATORS 
#(CODES FOR PRODUCTS, CATEGORIES OF EXPENDITURES AND EARNINGS, ETC)

translator_earnings <- read_xls("Expenditure imputation\\earnings_translator.xls")
translator_earnings <- translator_earnings[-nrow(translator_earnings),]

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
  mutate(idhead = ifelse(V0306 == 1,
                         yes = as.numeric(base_ids$idperson),
                         no = 0)) %>%
  group_by(idhh) %>% 
  mutate(idhead = as.character(max(idhead))) %>% 
  ungroup(idhh)



base_ids <- base_ids %>% 
  mutate(malehead = ifelse(V0404 == 1 & V0306 == 1, #identifica heads homens
                           yes = 1,
                           no = 0),
         femalehead = ifelse(V0404 == 2 & V0306 == 1, #identifica heads mulheres
                             yes = 1,
                             no = 0)) %>% 
  group_by(idhh) %>% 
  mutate(malehead = max(malehead), femalehead = max(femalehead))

#Just to check, the following cross product should be 0
#print(as.vector(base_ids$malehead) %*% as.vector(base_ids$femalehead))

base_ids <- base_ids %>% 
  mutate(idmalehead = ifelse(V0404 == 1 & 
                               V0306 == 1,
                             yes = as.numeric(idperson),
                             no = 0),
         idfemalehead = ifelse(V0404 == 2 & 
                                 V0306 == 1,
                               yes = as.numeric(idperson),
                               no = 0)) %>% 
  group_by(idhh) %>% 
  mutate(idmalehead = as.character(max(idmalehead)),
         idfemalehead = as.character(max(idfemalehead))) %>% 
  ungroup(idhh)


#Identify household members with respect to head
base_ids <- base_ids %>% 
  mutate(idpartner = ifelse(V0306 == 2 |
                              V0306 == 3, #if head's partner
                            yes = idhead,
                            no = "0"),
         idfather = ifelse(
           malehead == 1 & 
             (V0306 == 4 | V0306 == 5 | V0306 == 6), #if head's child and head is a man
           yes = idmalehead,
           no = "0"),
         idmother = ifelse(
           femalehead == 1 & 
             (V0306 == 4 | V0306 == 5 | V0306 == 6), #if head's child and head is a woman
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

#Create sample weight (dwt), age (dag),gender (gdn), and urban region (drgur) variables
base_ids <- base_ids %>% 
  rename(dwt = PESO_FINAL,
         dag = V0403,
         dgn = V0404,
         drgur = TIPO_SITUACAO_REG)

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
  mutate(dec = case_when(is.na(V0419) ~ 0,
                         V0419 == 1 ~ 1,
                         V0419 == 2 ~ 1,
                         V0419 == 3 ~ 2,
                         (V0419 == 4 & dag < 12) ~ 2,
                         (V0419 == 4 & dag >= 12) ~ 3,
                         V0419 == 6 ~ 4,
                         V0419 == 8 ~ 5,
                         V0419 == 9 ~ 5,
                         V0419 == 10 ~ 6,
                         V0419 == 11 ~ 6,
                         V0419 == 5 ~ 3,
                         V0419 == 7 ~ 4))

#Create years of education (dey) variable 

base_dey <- base_dec %>% 
  mutate(dey = ANOS_ESTUDO)

#Create highest education status (deh) variable

base_deh <- base_dey %>% 
  mutate(deh = case_when((is.na(V0425) & dec == 0) ~ 0,
                         (is.na(V0425) & dec != 0) ~ dec,
                         V0425 == 1 ~ 0,
                         V0425 == 2 ~ 1,
                         V0425 == 3 ~ 2,
                         V0425 == 4 ~ 2,
                         V0425 == 5 ~ 2,
                         V0425 == 6 ~ 3,
                         V0425 == 7 ~ 3,
                         V0425 == 8 ~ 3,
                         V0425 == 9 ~ 4,
                         V0425 == 10 ~ 4,
                         V0425 == 11 ~ 4,
                         V0425 == 12 ~ 5,
                         V0425 == 13 ~ 5,
                         V0425 == 14 ~ 6,
                         V0425 == 15 ~ 6))

#LABOUR

#Now we have to merge our current table with 
#labour status information from RENDIMENTO_TRABALHO


#Codes from the translator for labour earnings
EMPLOYED <- translator_earnings$Codigo[translator_earnings$Descricao_3 == 'Empregado']
SELF_EMPLOYED <- translator_earnings$Codigo[translator_earnings$Descricao_3 == 'Conta propria']
EMPLOYER <- 53005


LABOUR_CODES <- c(EMPLOYED, SELF_EMPLOYED, EMPLOYER)

#Take only first 5 digits of codes
RENDIMENTO_TRABALHO$V9001 <- substr(RENDIMENTO_TRABALHO$V9001, 1, 5)

labour_status <- aggregate(
  (RENDIMENTO_TRABALHO %>% filter(SUB_QUADRO == 1))$V5302[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES],
  by = list(COD_UPA = (RENDIMENTO_TRABALHO %>% filter(SUB_QUADRO == 1))$COD_UPA[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES],
            NUM_DOM = (RENDIMENTO_TRABALHO %>% filter(SUB_QUADRO == 1))$NUM_DOM[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES],
            NUM_UC = (RENDIMENTO_TRABALHO %>% filter(SUB_QUADRO == 1))$NUM_UC[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES],
            COD_INFORMANTE = (RENDIMENTO_TRABALHO %>% filter(SUB_QUADRO == 1))$COD_INFORMANTE[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES]),
  FUN = function(x) paste(x[!is.na(x)], collapse = ""))

base_les <- merge(base_deh,
                  labour_status,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T) %>% 
  rename(les = x)

#Public sector workers

base_lpb <- base_les %>% 
  mutate(lpb = ifelse(!is.na(les) & (les == 2 | les == 4),
                      yes = 1,
                      no = 0))

#Domestic workers

base_ldt <- base_lpb %>% 
  mutate(ldt = ifelse(!is.na(les) & (les == 1),
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
                         les == 4 ~ 3,
                         les == 6 ~ 2,
                         les == 2 ~ 3,
                         les == 5 ~ 2,
                         les == 1 ~ 3,
                         les == 7 ~ 9,
                         (is.na(les) & dec > 1) ~ 6,
                         (is.na(les) & dag >= 18 & dec <= 0) ~ 7,
                         (is.na(les) & dec == 1) ~ 0))


#Get ISCO occupation codes
labour_ocupation <- aggregate(
  RENDIMENTO_TRABALHO$V53011[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES],
  by = list(COD_UPA = RENDIMENTO_TRABALHO$COD_UPA[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES],
            NUM_DOM = RENDIMENTO_TRABALHO$NUM_DOM[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES],
            NUM_UC = RENDIMENTO_TRABALHO$NUM_UC[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES],
            COD_INFORMANTE = RENDIMENTO_TRABALHO$COD_INFORMANTE[RENDIMENTO_TRABALHO$V9001 %in% LABOUR_CODES]),
  FUN = function(x) paste(x[!is.na(x)], collapse = "")) %>% 
  mutate(loc = ifelse(x != "",
                      yes = gsub('[^0-9]', '', x),
                      no = 0))


base_loc <- merge(base_les2,
                  labour_ocupation,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T) 

#Create labour ocupation (loc) variable based on 1st digit of ISCO
base_loc <- base_loc %>% 
  mutate(loc = substr(loc, 1,1))

#Get weekly hours worked

labour_hours <- RENDIMENTO_TRABALHO %>% 
  filter(V5314 < 999 & SUB_QUADRO == 1) %>% #999 means no reporting
  mutate(weekly_hours_times_months = V5314*V9011) %>% 
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
INVESTMENTS <- c(5401401, 5501201, 5501301, 5501601, 5501602, 5594401,
                   5504801, 5506001, 5506101, 5600101, 5600201, 5600301,
                   5600401, 5700101, 5700201, 5700301, 5700401)

#Substitute NAs in "# of months earned" for 1 
OUTROS_RENDIMENTOS$V9011[is.na(OUTROS_RENDIMENTOS$V9011)] <- 1

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
#and poor elderly/disabled benefit (bdioa)

#Codes for all sort of old age pensions
PENSIONS_OLD_AGE <- c(5400401, 5400501, 5400601, 5400701,
                    5403101, 5503301, 5504601, 5505001, 5506201,
                    5506401, 5500301, 5500401, 5500501, 5500601, 5500701)

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
UNEMPLOYMENT <-  c(5501701, 5501702)

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
BPC <- c(5400201)

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

DONATIONS <- c(5401301)

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

#OTHER VARIABLES

#Disabilities

#There is no information regarding disabled persons in POF
#The best we can do is use BPC's eligibility criteria to identify
#some disabled persons: if someone is less than 65 and received BPC,
#they have to be disabled

base_ddi <- base_ypt %>% 
  mutate(ddi = ifelse(!is.na(bdioa) & bdioa > 0 & dag < 65,
                      yes = 1,
                      no = 0))

#Formal employment

formal_employment <- RENDIMENTO_TRABALHO %>% 
  group_by(COD_UPA,NUM_DOM,NUM_UC, COD_INFORMANTE) %>% 
  summarise(lem = ifelse(any(V5304 == 1),
                         yes = 1,
                         no = 0))

base_lem <- merge(base_ddi,
                  formal_employment,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T)
#Pension membership

pension_membership <- RENDIMENTO_TRABALHO %>% 
  group_by(COD_UPA,NUM_DOM,NUM_UC, COD_INFORMANTE) %>% 
  summarise(lpm = ifelse(any(V5305 == 1),
                         yes = 1,
                         no = 0))

base_lpm <- merge(base_lem,
                  pension_membership,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T)

base <- base_lpm %>% 
  mutate(sgl_s = 2018)

#Select only variables for simulation

base_final_pof <- base %>% 
  select(idhh, idperson, idorighh, idorigperson, idfather, idmother, idpartner, sgl_s,
         dct, drgn1, drgn2, drgur, dwt, dag,
         dec, dey, deh, les, lem, lpb, ldt, los, lse, yem, dgn, lhw, dms, loc, 
         yse, yiy, ddi, poa, bun, bdioa, ypt, yhh, lpm) %>% 
  arrange(idhh) %>% 
  mutate(across(everything(), as.character),
         across(everything(), ~replace_na(.x, "0")))


#Save base as a tab separated .txt 
write.table(base_final_pof, file=paste0("Input\\BR_2018_b1.txt"),
            quote=FALSE, sep='\t', row.names=FALSE)



  

