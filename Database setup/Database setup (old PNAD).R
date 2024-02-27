library(tidyverse)
library(readxl)
library(data.table)

#SET WORKING DIRECTORY

#Set it to where the BRASMOD folder is

file_directory <- dirname(rstudioapi::getSourceEditorContext()$path)

BRASMOD_directory <- gsub("/Database setup", "", file_directory)

setwd(BRASMOD_directory)

#CHOOSE YEAR FOR PNAD SURVEY

year <- 2015

pnad <- readRDS(paste0("Database setup\\Old PNAD data\\pnad", as.character(year), ".RDS"))

#PART 1: ADD MANDATORY VARIABLES

#IDs

#Create household and individual IDs from PNAD variables
base_ids <- pnad %>% 
  filter(v0401 <= 6) %>%  #Exclude domestic workers and their relatives living in the household
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

#LABOUR

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

base_les <- base_deh %>% 
  mutate(les = case_when(v9029 == 1 ~ 3,
                         v9029 == 2 ~ 3,
                         v9029 == 3 ~ 2,
                         v9029 == 4 ~ 2,
                         v9029 == 4 ~ 2,
                         v9029 == 5 ~ 9,
                         (is.na(v9029) & dec > 1) ~ 6,
                         (is.na(v9029) & dag >= 18 & dec == 0) ~ 7,
                         (is.na(v9029) & dec == 1) ~ 0))

#Identify public sector workers
base_lpb <- base_les %>% 
  mutate(lpb = ifelse(v9032 == 4,
                      yes = 1,
                      no  = 0))

#Identify domestic workers
base_ldt <- base_lpb %>% 
  mutate(ldt = ifelse(v9029 == 2,
                      yes = 1,
                      no  = 0))

#Identify entrepreneurs
base_los <- base_ldt %>% 
  mutate(los = ifelse(!is.na(v9048),
                      yes = 1,
                      no = 0))


#Identify self-employed workers

base_lse <- base_los %>% 
  mutate(lse = ifelse(v9029 == 3,
                      yes = 1,
                      no  = 0))

#Create labour ocupation (loc) variable based on 1st digit of ISCO
base_loc <- base_lse %>%
  mutate(loc = ifelse(!is.na(v9906),
                      yes = substr(v9906, 1,1),
                      no = 0))

#Create hours worked weekly (lhw) variable
base_lhw <- base_loc %>%
  mutate(lhw = ifelse(!is.na(v9058),
                      yes = v9068,
                      no = 0))

#Create labour economic status for civil servants 
base_lescs <- base_lhw %>%
  mutate(lescs = case_when(v9033 == 1 ~ 1,
                           v9033 == 3 ~ 2,
                           v9033 == 5 ~ 3,
                           is.na(v9033) ~ 0))

#Create unemployment benefit variable (binary: received? yes or no)
#The old PNAD has no information on unemployment benefit values, so we have
#to simulate them with BRASMOD based on individual response
base_bun <- base_lescs %>% 
  mutate(bunyn = ifelse((!is.na(v9066) & v9066 == 2) | (!is.na(v9084) & v9084 == 2),
                        yes = 1,
                        no  = 0))

#INCOME

#Create income variables: yem (employment income), yse (self-employment income),
#yiy (investiment income)

base_yem <- base_bun %>% 
  mutate(yem1 = ifelse(les == 3 & v9532 < 999999999, #if works as employee in main job (999999999 means NA)
                       yes = as.numeric(v9532), #yem1 is main job's income
                       no = 0), #0 otherwise
         yem2 = ifelse((v9092 == 1 | v9092 == 2) & v9982 < 999999999,  #if works as employee in second job
                       yes = as.numeric(v9982), #yem2 is second job's income
                       no = 0)) %>%  #otherwise
  rowwise() %>% 
  mutate(yem = sum(yem1, yem2, na.rm = TRUE)) #yem is final row-wise sum of yem1 and yem2


base_yse <- base_yem %>% 
  mutate(yse1 = ifelse(les == 2 & v9532 < 999999999, #if works as self-employed in main job
                       yes = as.numeric(v9532), #yse1 is main job's income
                       no = 0),#0 otherwise
         yse2 = ifelse((v9092 == 3 | v9092 == 4) & v9982 < 999999999, #if works as self-employed in second job
                       yes = as.numeric(v9982), #yse2 is second job's income
                       no = 0)) %>% #0 otherwise
  rowwise() %>% 
  mutate(yse = sum(yse1, yse2, na.rm = TRUE)) #yse is final sum of yse1 and yse2

#PUBLIC PENSIONS AND OTHERS

#Add  old age pension/retirement (poa)
#and private donations (ypt)

base_poa <- base_yse %>% 
  mutate(poa1 = ifelse(!is.na(v1252) & v1252 < 9999999,
                       yes = v1252,
                       no = 0),
         poa2 = ifelse(!is.na(v1258) & v1258 < 9999999,
                       yes = v1258,
                       no = 0),
         poa = poa1 + poa2)

base_ypt <- base_poa %>% 
  mutate(ypt = ifelse(!is.na(v1270) & v1270 < 9999999,
                      yes = v1270,
                      no = 0))


#Add poor elderly/disabled benefit (bdioa) unemployment benefit (bun)

#In the old PNADs, there is no question asking if the person has received the 
#poor elderly/disabled benefit BPC 

#What we have is a "other sources of income" variable (v1273), which aggregates income
#from means-tested social programs like BPC, investment income, etc

#To identify individuals who have received the BPC, we use the "typical values method"
#The value of the BPC benefit is exactly a monthly minimum wage

min_wage <- case_when(
  year == 2015 ~ 788,
  year == 2014 ~ 724,
  year == 2013 ~ 678,
  year == 2012 ~ 622,
  year == 2011 ~ 545
)

#We then consider a margin of error of 2%, and assume that
#values within 0.98*min_wage and 1.02*min_wage correspond to the BPC benefit
#Besides that, we also consider that values which match exactly 
#v1273 = min_wage + (some possible PBF benefit value) means that the person has received the BPC

#Baseline PBF benefit
bpbf_bb <- case_when(
  year == 2015 ~ 77,
  year == 2014 ~ 70,
  year == 2013 ~ 70,
  year == 2012 ~ 70,
  year == 2011 ~ 70
)

#Variable PBF benefit (per child)
bpbf_bv <- case_when(
  year == 2015 ~ 35,
  year == 2014 ~ 32,
  year == 2013 ~ 32,
  year == 2012 ~ 32,
  year == 2011 ~ 32
)

#Variable PBF benefit (per adolescent)
bpbf_bj <- case_when(
  year == 2015 ~ 42,
  year == 2014 ~ 38,
  year == 2013 ~ 38,
  year == 2012 ~ 38,
  year == 2011 ~ 38
)
  
#Possible combinations
typical_pbf_and_bpc_values <- list(
  min_wage + bpbf_bb,
  min_wage + bpbf_bb + bpbf_bv,
  min_wage + bpbf_bb + 2*bpbf_bv,
  min_wage + bpbf_bb + 3*bpbf_bv,
  min_wage + bpbf_bb + 4*bpbf_bv,
  min_wage + bpbf_bb + 5*bpbf_bv,
  min_wage + bpbf_bb + 6*bpbf_bv,
  min_wage + bpbf_bb + bpbf_bj,
  min_wage + bpbf_bb + 2*bpbf_bj,
  min_wage + bpbf_bb + 3*bpbf_bj,
  min_wage + bpbf_bb + 4*bpbf_bj,
  min_wage + bpbf_bb + 5*bpbf_bj,
  min_wage + bpbf_bb + 6*bpbf_bj,
  min_wage + bpbf_bb + bpbf_bv + bpbf_bj,
  min_wage + bpbf_bb + 2*bpbf_bv + bpbf_bj,
  min_wage + bpbf_bb + 3*bpbf_bv + bpbf_bj,
  min_wage + bpbf_bb + bpbf_bv + 2*bpbf_bj,
  min_wage + bpbf_bb + bpbf_bv + 3*bpbf_bj,
  min_wage + bpbf_bb + bpbf_bv + 4*bpbf_bj,
  min_wage + bpbf_bb + 2*bpbf_bv + 2*bpbf_bj,
  min_wage + bpbf_bb + 2*bpbf_bv + 3*bpbf_bj,
  min_wage + bpbf_bb + 3*bpbf_bv + 2*bpbf_bj
)


#If v1273 value satisfies one of the conditions above, we assume that the person
#has received the BPC benefit equal to min_wage
base_bdioa <- base_ypt %>% 
  mutate(bdioa = ifelse(!is.na(v1273) & ((v1273 %in% typical_pbf_and_bpc_values) | 
                          (0.98 *min_wage <= v1273 & v1273 <= 1.02*min_wage)),
         yes = min_wage,
         no = 0))

#If the person has received the BPC and they're not over 65,
#this means they can only be disabled

base_ddi <- base_bdioa %>% 
  mutate(ddi = ifelse(bdioa > 0 & dag < 65,
                      yes = 1,
                      no = 0))

#The other sources of income are assumed to consist of investment income

base_yiy <- base_ddi %>% 
  mutate(yiy = ifelse(!is.na(v1273) & v1273 < 9999999 & !((0.98 *min_wage <= v1273 & v1273 <= 1.02*min_wage) |
                                                            v1273 %in% typical_pbf_and_bpc_values),
         yes = v1273,
         no = 0))

base_yprrt <- base_yiy %>% 
  mutate(yprrt = ifelse(!is.na(v1267) & v1267 < 9999999,
                        yes = as.numeric(v1267),
                        no = 0)) #income from rental of property 

#Total household income
base_yhh <- base_yprrt %>% 
  group_by(idhh) %>% 
  mutate(yhh = sum(yem) + sum(yse) + sum(yiy))


#Formal employment
base_lem <- base_yhh %>% 
  mutate(lem = ifelse((!is.na(v9042) & v9042 == 2) | (!is.na(v9097) & v9097 == 2),
                      yes = 1,
                      no = 0))

#Contributed to social insurance
base_lpm <- base_lem %>% 
  mutate(lpm = ifelse(!is.na(v4711) & v4711 == 1,
                      yes = 1,
                      no = 0))

base <- base_lpm

#Select mandatory variables for Euromod and make final adjustments
base_final_pnad <- base %>% 
  select(idhh, idperson, idorighh, idorigperson, idfather, idmother, idpartner, dct, drgn1, drgn2, drgur, dwt, dag,
         dec, dey, deh, les, lem, lpb, ldt, los, lse, bunyn, yem, dgn, lhw, dms, loc, 
         yse, yiy, ddi, poa, bdioa, ypt, yhh, lpm) %>% 
  mutate(across(everything(), as.character),
         across(everything(), ~replace_na(.x, "0")))

#Save base as a tab separated .txt 
write.table(base_final_pnad, file=paste0("Input\\BR_", as.character(year), "_a1.txt"),
            quote=FALSE, sep='\t', row.names=FALSE)
