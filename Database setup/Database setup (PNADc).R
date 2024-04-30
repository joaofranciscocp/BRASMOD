#PACKAGES USED
library(tidyverse)
library(readxl)
library(data.table)
library(PNADcIBGE)


#SET WORKING DIRECTORY

#Set it to where the BRASMOD folder is

file_directory <- dirname(rstudioapi::getSourceEditorContext()$path)

BRASMOD_directory <- gsub("/Database setup", "", file_directory)

setwd(BRASMOD_directory)

#CHOOSE YEAR FOR PNAD SURVEY
year <- 2019

pnad_raw <- get_pnadc(year = year,
                      interview = 5)

pnad <- pnad_raw$variables

#PART 1: ADD MANDATORY VARIABLES

#IDs

#Create household and individual IDs from PNAD variables
base_ids <- pnad %>% 
  mutate(idorighh = paste0(UPA, V1008, V1014),
         idorigperson = paste0(idorighh, V2003))

#We create new sorted household and individual IDs

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
  mutate(idhead = ifelse(base_ids$V2005 == "Pessoa responsável pelo domicílio",
                         yes = as.numeric(base_ids$idperson),
                         no = 999999999999999999)) %>% #just so it's larger than any other ID
  group_by(idhh) %>% 
  mutate(idhead = as.character(min(idhead))) %>% 
  ungroup(idhh)


#Create two binary variables to identify male and female heads (considering there might be both),
#then group by household and identify if the household has a male or female head

base_ids <- base_ids %>% 
  mutate(malehead = ifelse(V2007 == "Homem" & 
                             V2005 == "Pessoa responsável pelo domicílio", #identifies male heads
                           yes = 1,
                           no = 0),
         femalehead = ifelse(V2007 == "Mulher" & 
                               V2005 == "Pessoa responsável pelo domicílio", #identifies female heads
                             yes = 1,
                             no = 0)) %>% 
  group_by(idhh) %>% 
  mutate(malehead = max(malehead), femalehead = max(femalehead))


#Create idmalehead and idfemalehead in the same manner as idhead above
base_ids <- base_ids %>% 
  mutate(idmalehead = ifelse(V2007 == "Homem" & 
                               V2005 == "Pessoa responsável pelo domicílio",
                             yes = as.numeric(idperson),
                             no = 0),
         idfemalehead = ifelse(V2007 == "Mulher" & 
                                 V2005 == "Pessoa responsável pelo domicílio",
                               yes = as.numeric(idperson),
                               no = 0)) %>% 
  group_by(idhh) %>% 
  mutate(idmalehead = as.character(max(idmalehead)),
         idfemalehead = as.character(max(idfemalehead))) %>% 
  ungroup(idhh)

#Identify household members with respect to head
base_ids <- base_ids %>% 
  mutate(idpartner = ifelse(V2005 == "Cônjuge ou companheiro(a) de sexo diferente" |
                              V2005 == "Cônjuge ou companheiro(a) do mesmo sexo", #if head's partner
                            yes = idhead,
                            no = "0"),
         idfather = ifelse(
           malehead == 1 & 
             (V2005 == "Filho(a) somente do responsável" |
                V2005 == "Filho(a) do responsável e do cônjuge"), #if head's child and head is a man
           yes = idmalehead,
           no = "0"),
         idmother = ifelse(
           femalehead == 1 & 
             (V2005 == "Filho(a) somente do responsável" |
                V2005 == "Filho(a) do responsável e do cônjuge"), #if head's child and head is a woman
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
  mutate(dwt = V1032,
         dag = V2009,
         dgn = V2007) %>% 
  mutate(dgn = case_when(dgn == "Homem"  ~ 1,
                         dgn == "Mulher" ~ 2),
         drgur = ifelse(V1022 == "Urbana",
                        yes = 1,
                        no = 0),
         dra = case_when(V2010 == "Branca"   ~ 1,
                         V2010 == "Preta"    ~ 2,
                         V2010 == "Amarela"  ~ 3,
                         V2010 == "Parda"    ~ 4,
                         V2010 == "Indígena" ~ 5,
                         V2010 == "Ignorado" ~ 9))

#Create marital status (dms) variable. With the PNADc, we're only able to capture 
#if the individual has a partner or not
base_ids$dms <- ifelse(base_ids$idpartner != 0, #has a partner
                                   yes = 2, #"married"
                                   no = 1) #single

#Create region NUTS level 1 (drgn1) and 2 (drgn2) variables
#We consider that level 1 regions are the IBGE-defined regions in BR (North, Northeast, South, Southeast and Central-West),
#and that level 2 is the states

base_drgn <- base_ids %>% 
  mutate(drgn1 = substr(UPA, 1, 1),
         drgn2 = substr(UPA, 1, 2))

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
    mutate(dec = case_when(is.na(V3003A) ~ 0,
                         V3003A == "Pré-escola" ~ 1,
                         V3003A == "Alfabetização de jovens e adultos" ~ 2,
                         (V3003A == "Regular do ensino fundamental" & dag < 12) ~ 2,
                         (V3003A == "Regular do ensino fundamental" & dag >= 12) ~ 3,
                         V3003A == "Regular do ensino médio" ~ 4,
                         V3003A == "Superior - graduação" ~ 5,
                         V3003A == "Especialização de nível superior" ~ 5,
                         V3003A == "Mestrado" ~ 6,
                         V3003A == "Doutorado" ~ 6,
                         V3003A == "Educação de jovens e adultos (EJA) do ensino fundamental" ~ 3,
                         V3003A == "Educação de jovens e adultos (EJA) do ensino médio" ~ 4))

#Create years of education (dey) variable 

base_dey <- base_dec %>% 
  mutate(dey = ifelse(!is.na(VD3005),
                      yes = as.numeric(gsub('[^0-9]', '', VD3005)),
                      no = 0))

#Create highest education status (deh) variable

base_deh <- base_dey %>% 
  mutate(deh = case_when((is.na(VD3004) & dec == 0) ~ 0,
                         (is.na(VD3004) & dec != 0) ~ dec,
                         VD3004 == "Sem instrução e menos de 1 ano de estudo" ~ 0,
                         (VD3004 == "Fundamental incompleto ou equivalente" & dey < 5) ~ 1,
                         (VD3004 == "Fundamental incompleto ou equivalente" & dey >= 5) ~ 2,
                         VD3004 == "Fundamental completo ou equivalente" ~ 3,
                         VD3004 == "Médio incompleto ou equivalente" ~ 3,
                         VD3004 == "Médio completo ou equivalente" ~ 4,
                         VD3004 == "Médio completo ou equivalente" ~ 4,
                         VD3004 == "Superior incompleto ou equivalente" ~ 4,
                         (VD3004 == "Superior completo" & dec <= 5) ~ 5,
                         (VD3004 == "Superior completo" & dec > 5) ~ 6))

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
  mutate(les = case_when(V4012 == "Empregado do setor privado" ~ 3,
                         V4012 == "Empregado do setor público (inclusive empresas de economia mista)" ~ 3,
                         V4012 == "Conta própria" ~ 2,
                         V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" ~ 3,
                         V4012 == "Empregador" ~ 2,
                         V4012 == "Trabalhador doméstico" ~ 3,
                         V4012 == "Trabalhador familiar não remunerado" ~ 9,
                         (is.na(V4012) & dec > 1) ~ 6,
                         (is.na(V4012) & dag >= 18 & dec == 0) ~ 7,
                         (is.na(V4012) & dec == 1) ~ 0))

#Identify public sector workers
base_lpb <- base_les %>% 
  mutate(lpb = ifelse(V4012 == "Empregado do setor público (inclusive empresas de economia mista)" |
                        V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar",
         yes = 1,
         no  = 0))

#Identify domestic workers
base_ldt <- base_lpb %>% 
  mutate(ldt = ifelse(V4012 == "Trabalhador doméstico",
                      yes = 1,
                      no  = 0))

#Identify entrepreneurs
base_los <- base_ldt %>% 
  mutate(los = ifelse(V4012 == "Empregador",
                      yes = 1,
                      no = 0))


#Identify self-employed workers

base_lse <- base_los %>% 
  mutate(lse = ifelse(V4012 == "Conta própria",
                      yes = 1,
                      no  = 0))

#Create labour ocupation (loc) variable based on 1st digit of ISCO
base_loc <- base_lse %>%
  mutate(loc = ifelse(!is.na(V4010),
                      yes = substr(V4010, 1,1),
                      no = 0))

#Create hours worked weekly (lhw) variable
base_lhw <- base_loc %>%
  mutate(lhw = V4039)

#Create labour economic status for civil servants 
base_lescs <- base_lhw %>%
  mutate(lescs = case_when(V4045 == "Federal" ~ 1,
                           V4045 == "Estadual" ~ 2,
                           V4045 == "Municipal" ~ 3,
                           is.na(V4045) ~ 0))

#INCOME

#Create income variables: yem (employment income), yse (self-employment income),
#yiy (investiment income)


base_yem <- base_lescs%>% 
  mutate(yem1 = ifelse(les == 3, #if works as employee in main job
                      yes = as.numeric(V403312), #yem1 is main job's income
                      no = 0), #0 otherwise
         yem2 = ifelse(V4043 %in% c("Empregado do setor privado", #if works as employee in second job
                                    "Empregado do setor público (inclusive empresas de economia mista)",
                                    "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar",
                                    "Trabalhador doméstico"),
                       yes = as.numeric(V405012), #yem2 is second job's income
                       no = 0)) %>%  #otherwise
  rowwise() %>% 
  mutate(yem = sum(yem1, yem2, na.rm = TRUE)) #yem is final row-wise sum of yem1 and yem2


base_yse <- base_yem %>% 
  mutate(yse1 = ifelse(les == 2, #if works as self-employed in main job
                      yes = as.numeric(V403312), #yse1 is main job's income
                      no = 0),#0 otherwise
         yse2 = ifelse(V4043 %in% c("Conta própria", "Empregador"), #if works as self-employed in second job
                      yes = as.numeric(V405012), #yse2 is second job's income
                      no = 0)) %>% #0 otherwise
  rowwise() %>% 
  mutate(yse = sum(yse1, yse2, na.rm = TRUE)) #yse is final sum of yse1 and yse2
         

base_yiy <- base_yse %>% 
  mutate(yiy = ifelse(!is.na(V5008A2),
         yes = as.numeric(V5008A2),
         no = 0)) #income from investment 

#Household total income

base_yhh <- base_yiy %>% 
  group_by(idhh) %>% 
  mutate(yhh = sum(as.numeric(yse), #total household income
                   as.numeric(yem), 
                   as.numeric(yiy))) %>% #per capita labour income
  ungroup(idhh)


#Household total and per capita reported income (for social programs eligibility)
#Defition of reported income comes from Cadastro Único (platform for social programs in Brazil)

# base_yre <- base_yhh %>% 
#   group_by(idhh) %>% 
#   mutate(yre = sum(as.numeric(replace_na(VD4020, 0)), #Total income from work
#                    as.numeric(replace_na(V5005A2, 0)), #Unemployment benefits
#                    as.numeric(replace_na(V5004A2, 0)), #Old age or other form of pensions
#                    as.numeric(replace_na(V5006A2, 0))), #Donations
#          yre00 = yre/n()) %>% 
#   ungroup(idhh)

#Old PBF

# yhh = sum(as.numeric(replace_na(VD4020, 0)), 
#           as.numeric(replace_na(V5005A2, 0)),
#           as.numeric(replace_na(V5004A, 0)),
#           as.numeric(replace_na(V5008A, 0)),
#           as.numeric(replace_na(V5001A2, 0)),
#           as.numeric(replace_na(V5007A, 0)))

#PUBLIC PENSIONS AND OTHERS

#Add unemployment benefit (bun), old age pension/retirement (poa),
#poor elderly/disabled benefit (bdioa) and private donations (ypt)

base_bun <- base_yhh %>% 
  mutate(bun = as.numeric(replace_na(V5005A2, 0)), #Unemployment benefits
         poa = as.numeric(replace_na(V5004A2, 0)), #Old age or other form of pensions
         bdioa = as.numeric(replace_na(V5001A2, 0)), #Disabled or poor elderly benefit (BPC)
         ypt = as.numeric(replace_na(V5006A2, 0))) #Donations (private transfers)


#Disabilities: PNADc doesn't have information on disabled persons, so we
#give "-1" to everyone (Euromod's recommendation on missing variables)
base_ddi <- base_bun %>% 
  mutate(ddi = ifelse(V5001A == "Sim" & dag < 65,
                      yes = "1",
                      no = "0"))


#Formal employment
base_lem <- base_ddi %>% 
  mutate(lem = case_when(V4029 == "Sim" ~ "1",
                         V4029 == "Não" ~ "2"))

#In work history (length in months)
base_liwwh <- base_lem %>% 
  rowwise() %>% 
  mutate(liwwh = ifelse(!(is.na(V40401) & is.na(V40402) & is.na(V40403)),
                        yes = sum(V40401, 12 + V40402, 24 + V40403, na.rm = TRUE),
                        no  = 0))

#Contributed to social insurance
base_lpm <- base_liwwh %>% 
  mutate(lpm = case_when(VD4012 == "Contribuinte" ~ 1,
                         VD4012 == "Não contribuinte" ~ 0))


#Select mandatory variables for Euromod and make final adjustments
base_final_pnad <- base_lpm %>% 
  select(idhh, idperson, idorighh, idorigperson, idfather, idmother, idpartner, 
         dct, drgn1, drgn2, drgur, dwt, dra, dag,
         dec, dey, deh, les, lem, lpb, ldt, los, lse, yem, dgn, lhw, dms, loc, 
         yse, yiy, ddi, poa, bun, bdioa, ypt, yhh, lpm) %>% 
  mutate(across(everything(), as.character),
         across(everything(), ~replace_na(.x, "0")))

#Save base as a tab separated .txt 
write.table(base_final_pnad, file=paste0("Input\\BR_", as.character(year), "_a1.txt"),
            quote=FALSE, sep='\t', row.names=FALSE)





