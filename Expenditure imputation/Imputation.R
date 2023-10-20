#PACKAGES USED
library(tidyverse)
library(readxl)
library(data.table)
library(MatchIt)

#SET WORKING DIRECTORY

#Set it to where the BRASMOD folder is

setwd("C:\\Users\\joao.perez\\Downloads\\brasmod\\brasmod")

#Set year for imputation

year = 2022

#READING THE DATA

#PNAD

#Outputs from BRASMOD
pnad_individual <- fread(paste0("Expenditure imputation\\PNAD data\\bra_", as.character(year), "_std.txt"), 
                         sep = "\t", 
                         header = T, dec = ",") %>% 
  mutate(across(everything(), as.numeric))

pnad_hh <- fread(paste0("Expenditure imputation\\PNAD data\\bra_", as.character(year), "_std_hh.txt"), sep = "\t", 
                 header = T, dec = ",") %>% 
  mutate(across(everything(), as.numeric))

#POF

#Outputs from BRASMOD
pof_individual <- fread(paste0("Expenditure imputation\\POF data\\bra_", as.character(year), "_std.txt"), 
                               sep = "\t", 
                        header = T, dec = ",") %>% 
  mutate(across(everything(), as.numeric))

pof_hh <- fread(paste0("Expenditure imputation\\POF data\\bra_", as.character(year), "_std_hh.txt"), 
                sep = "\t", 
                header = T, dec = ",") %>% 
  mutate(across(everything(), as.numeric))

#Correction for inflation

cpi <- read_xls("Expenditure imputation\\CPI (IPCA).xls")
inflation_correction <- cpi$price_level[cpi$year == year]

#Expenditure data

DESPESA_COLETIVA <- readRDS("Expenditure imputation\\POF data\\DESPESA_COLETIVA.rds") %>%  
  mutate(V9001 = str_sub(V9001, 1, -3)) %>% 
  mutate(idorighh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>%
  mutate(V9011 = ifelse(is.na(V9011), 1, V9011)) %>% 
  filter(str_sub(V9001,-3) != "999" &
           as.numeric(V9002) <= 6) %>% 
  mutate(V8000_DEFLA_new = (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)*inflation_correction/12) %>% 
  select(idorighh, V9001, V8000_DEFLA_new, UF)

DESPESA_INDIVIDUAL <- readRDS("Expenditure imputation\\POF data\\DESPESA_INDIVIDUAL.rds") %>% 
  mutate(V9001 = str_sub(V9001, 1, -3)) %>% 
  mutate(idorighh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>% 
  mutate(V9011 = ifelse(is.na(V9011), 1, V9011)) %>% 
  filter(str_sub(V9001,-3) != "999" &
           as.numeric(V9002) <= 6) %>% 
  mutate(V8000_DEFLA_new = (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)*inflation_correction/12) %>% 
  select(idorighh, V9001, V8000_DEFLA_new, UF)

CADERNETA_COLETIVA <- readRDS("Expenditure imputation\\POF data\\CADERNETA_COLETIVA.rds") %>%
  mutate(V9001 = substr(V9001, 1, 5)) %>%
  mutate(idorighh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>%
  filter(str_sub(V9001,-3) != "999" &
           as.numeric(V9002) <= 6) %>% 
  mutate(V8000_DEFLA_new = (V8000_DEFLA*FATOR_ANUALIZACAO)*inflation_correction/12) %>% 
  select(idorighh, V9001, V8000_DEFLA_new, UF)

tables_pof <- do.call(rbind,
                    list(DESPESA_COLETIVA, 
                         DESPESA_INDIVIDUAL,
                         CADERNETA_COLETIVA)) %>% 
  mutate(across(everything(), as.numeric))

#Codes for expenditure categories (already taken from POF's own categorization)

tradutor <- read_xls("Expenditure imputation\\general_expenditure_translator.xls", )

habitacao          <- tradutor$Codigo[tradutor$Descricao_3_novo == "Habitacao"]
despesas_diversas  <- tradutor$Codigo[tradutor$Descricao_3_novo == "Despesas diversas"]
prestacao          <- tradutor$Codigo[tradutor$Descricao_3_novo == "Prestação de imóvel"]
impostos           <- tradutor$Codigo[tradutor$Descricao_3_novo == "Impostos"]
imovel             <- tradutor$Codigo[tradutor$Descricao_3_novo == "Imóvel (reforma)" | 
                                        tradutor$Descricao_3_novo == "Imóvel (aquisição)"]
outras             <- tradutor$Codigo[tradutor$Descricao_3_novo == "Outras"]
investimentos      <- tradutor$Codigo[tradutor$Descricao_3_novo == "Outros investimentos"]
cultura            <- tradutor$Codigo[tradutor$Descricao_3_novo == "Recreação e cultura"]
contribuicoes_trab <- tradutor$Codigo[tradutor$Descricao_3_novo == "Contribuições trabalhistas"]
fumo               <- tradutor$Codigo[tradutor$Descricao_3_novo == "Fumo"]
transporte         <- tradutor$Codigo[tradutor$Descricao_3_novo == "Transporte"]
alimentacao        <- tradutor$Codigo[tradutor$Descricao_3_novo == "Alimentacao"]
serv_banc          <- tradutor$Codigo[tradutor$Descricao_3_novo == "Serviços bancários"]
serv_pess          <- tradutor$Codigo[tradutor$Descricao_3_novo == "Serviços pessoais"]
assist_saude       <- tradutor$Codigo[tradutor$Descricao_3_novo == "Assistencia a saude"]
higiene            <- tradutor$Codigo[tradutor$Descricao_3_novo == "Higiene e cuidados pessoais"]
educacao           <- tradutor$Codigo[tradutor$Descricao_3_novo == "Educacao"]
vestuario          <- tradutor$Codigo[tradutor$Descricao_3_novo == "Vestuario"]
emprestimo         <- tradutor$Codigo[tradutor$Descricao_3_novo == "Empréstimo"]
prev_priv          <- tradutor$Codigo[tradutor$Descricao_3_novo == "Previdência privada"]
pensoes            <- tradutor$Codigo[tradutor$Descricao_3_novo == "Pensões, mesadas e doações"]

expenditures_pof <- tables_pof %>%
  group_by(idorighh) %>% 
  summarise(
    habitacao            = sum(V8000_DEFLA_new[V9001 %in% habitacao]),       
    despesas_diversas    = sum(V8000_DEFLA_new[V9001 %in% despesas_diversas]),
    prestacao            = sum(V8000_DEFLA_new[V9001 %in% prestacao]),        
    impostos             = sum(V8000_DEFLA_new[V9001 %in% impostos]),           
    imovel               = sum(V8000_DEFLA_new[V9001 %in% imovel]),           
    outras               = sum(V8000_DEFLA_new[V9001 %in% outras]),
    investimentos        = sum(V8000_DEFLA_new[V9001 %in% investimentos]),
    cultura              = sum(V8000_DEFLA_new[V9001 %in% cultura]),            
    contribuicoes_trab   = sum(V8000_DEFLA_new[V9001 %in% contribuicoes_trab]),
    fumo                 = sum(V8000_DEFLA_new[V9001 %in% fumo]),
    transporte           = sum(V8000_DEFLA_new[V9001 %in% transporte]),       
    alimentacao          = sum(V8000_DEFLA_new[V9001 %in% alimentacao]),    
    serv_banc            = sum(V8000_DEFLA_new[V9001 %in% serv_banc]),        
    serv_pess            = sum(V8000_DEFLA_new[V9001 %in% serv_pess]),        
    assist_saude         = sum(V8000_DEFLA_new[V9001 %in% assist_saude]),   
    higiene              = sum(V8000_DEFLA_new[V9001 %in% higiene]),           
    educacao             = sum(V8000_DEFLA_new[V9001 %in% educacao]),          
    vestuario            = sum(V8000_DEFLA_new[V9001 %in% vestuario]),        
    emprestimo           = sum(V8000_DEFLA_new[V9001 %in% emprestimo]),       
    prev_priv            = sum(V8000_DEFLA_new[V9001 %in% prev_priv]),        
    pensoes              = sum(V8000_DEFLA_new[V9001 %in% pensoes])) %>% 
  mutate(across(everything(), as.numeric))

base_pof <- merge(pof_individual,
                  expenditures_pof,
                  by.x = "idorighh",
                  by.y = "idorighh",
                  all.x = TRUE) %>% 
  na.omit()

base_shares <- base_pof %>%
  group_by(idhh) %>%
  filter(sum(ils_dispy) > 0) %>% 
  mutate(share_habitacao            = mean(habitacao)/sum(ils_dispy),            
         share_despesas_diversas    = mean(despesas_diversas)/sum(ils_dispy),    
         share_prestacao            = mean(prestacao)/sum(ils_dispy),            
         share_impostos             = mean(impostos)/sum(ils_dispy),             
         share_imovel               = mean(imovel)/sum(ils_dispy),               
         share_outras               = mean(outras)/sum(ils_dispy),               
         share_investimentos        = mean(investimentos)/sum(ils_dispy),        
         share_cultura              = mean(cultura)/sum(ils_dispy),              
         share_contribuicoes_trab   = mean(contribuicoes_trab)/sum(ils_dispy),   
         share_fumo                 = mean(fumo)/sum(ils_dispy),                 
         share_transporte           = mean(transporte)/sum(ils_dispy),           
         share_alimentacao          = mean(alimentacao)/sum(ils_dispy),          
         share_serv_banc            = mean(serv_banc)/sum(ils_dispy),            
         share_serv_pess            = mean(serv_pess)/sum(ils_dispy),            
         share_assist_saude         = mean(assist_saude)/sum(ils_dispy),         
         share_higiene              = mean(higiene)/sum(ils_dispy),              
         share_educacao             = mean(educacao)/sum(ils_dispy),             
         share_vestuario            = mean(vestuario)/sum(ils_dispy),            
         share_emprestimo           = mean(emprestimo)/sum(ils_dispy),           
         share_prev_priv            = mean(prev_priv)/sum(ils_dispy),            
         share_pensoes              = mean(pensoes)/sum(ils_dispy))

base_shares_filtered <- base_shares %>% 
  filter(!(share_habitacao             > 1 |
             share_despesas_diversas   > 1 |
             share_prestacao           > 1 |
             share_impostos            > 1 |
             share_imovel              > 1 |
             share_outras              > 1 |
             share_investimentos       > 1 |
             share_cultura             > 1 |
             share_contribuicoes_trab  > 1 |
             share_fumo                > 1 |
             share_transporte          > 1 |
             share_alimentacao         > 1 |
             share_serv_banc           > 1 |
             share_serv_pess           > 1 |
             share_assist_saude        > 1 |
             share_higiene             > 1 |
             share_educacao            > 1 |
             share_vestuario           > 1 |
             share_emprestimo          > 1 |
             share_prev_priv           > 1 |
             share_pensoes             > 1))

base_shares_final <- base_shares_filtered %>%
  group_by(idhh) %>% 
  mutate(idhead = first(idperson)) %>% 
  ungroup(idhh) %>% 
  mutate(ismalehead = ifelse(idperson == idhead & dgn == 1,
                             yes = 1,
                             no = 0)) %>% 
  group_by(idhh) %>% 
  summarise(idhh                       = mean(idhh),
            idorighh                   = mean(idorighh),
            urban                      = ifelse(any(drgur == 1),
                                                yes = 1,
                                                no = 0),
            n_male_over_14             = sum(dgn == 1 & dag >= 14),
            n_under_14                 = sum(dag <= 14),
            n_between_15_29            = sum(dag %in% 15:29),
            n_between_30_44            = sum(dag %in% 30:44),
            n_between_45_59            = sum(dag %in% 45:59),
            n_over_60                  = sum(dag >= 60),
            n_employed                 = sum(les == 2 | les == 3),
            n_students                 = sum(les == 6),
            n_higher_education         = sum(deh >= 5),
            avg_years_educ             = mean(dey),
            malehead                   = max(ismalehead),
            weights                    = sum(dwt),
            region                     = as.factor(mean(drgn1)),
            share_habitacao            = mean(share_habitacao         ),
            share_despesas_diversas    = mean(share_despesas_diversas ),
            share_prestacao            = mean(share_prestacao         ),
            share_impostos             = mean(share_impostos          ),
            share_imovel               = mean(share_imovel            ),
            share_outras               = mean(share_outras            ),
            share_investimentos        = mean(share_investimentos     ),
            share_cultura              = mean(share_cultura           ),
            share_contribuicoes_trab   = mean(share_contribuicoes_trab),
            share_fumo                 = mean(share_fumo              ),
            share_transporte           = mean(share_transporte        ),
            share_alimentacao          = mean(share_alimentacao       ),
            share_serv_banc            = mean(share_serv_banc         ),
            share_serv_pess            = mean(share_serv_pess         ),
            share_assist_saude         = mean(share_assist_saude      ),
            share_higiene              = mean(share_higiene           ),
            share_educacao             = mean(share_educacao          ),
            share_vestuario            = mean(share_vestuario         ),
            share_emprestimo           = mean(share_emprestimo        ),
            share_prev_priv            = mean(share_prev_priv         ),
            share_pensoes              = mean(share_pensoes           ))

#REGRESSIONS

#Create vector of dependent variables

share_vars <- c("share_habitacao"         ,
                "share_despesas_diversas" ,
                "share_prestacao"         ,
                "share_impostos"          ,
                "share_imovel"            ,
                "share_outras"            ,
                "share_investimentos"     ,
                "share_cultura"           ,
                "share_contribuicoes_trab",
                "share_fumo"              ,
                "share_transporte"        ,
                "share_alimentacao"       ,
                "share_serv_banc"         ,
                "share_serv_pess"         ,
                "share_assist_saude"      ,
                "share_higiene"           ,
                "share_educacao"          ,
                "share_vestuario"         ,
                "share_emprestimo"        ,
                "share_prev_priv"         ,
                "share_pensoes"           )


#Create vector of independent variables

independent_vars <- base_shares_final %>% 
  select(-contains("share"), -weights, -idhh, -idorighh) %>% 
  names()

#Loop to create and run models for each share variable

probit_models <- list()
ols_models <- list()

for(var in share_vars){
  model_number <- which(share_vars == var)
  
  model <- reformulate(termlabels =  independent_vars,
                       response = var)
  
  probit <- glm(model, family = binomial(link = "probit"),
                data = base_shares_final)
  
  probit_models[[model_number]] <- probit
  
  ols <- lm(model, data = base_shares_final%>% 
              filter(get(var) > 0))
  
  ols_models[[model_number]] <- ols
}


#Prediciton for POF

pof_shares_hat <- base_shares_final %>% 
  select(idhh)


for(var in share_vars){
  model_number <- which(share_vars == var)
  
  share_hat <- ols_models[[model_number]]$fitted.values
  
  prob_hat <- probit_models[[model_number]]$fitted.values
  
  pof_shares_hat[, ncol(pof_shares_hat) + 1] <- prob_hat*share_hat
  
  colnames(pof_shares_hat)[ncol(pof_shares_hat)] <- paste0(var, "_hat")
}


#Prediction for PNAD

covariates_pnad <- pnad_individual %>% 
  group_by(idhh) %>% 
  mutate(idhead = first(idperson)) %>% 
  ungroup(idhh) %>% 
  mutate(ismalehead = ifelse(idperson == idhead & dgn == 1,
                             yes = 1,
                             no = 0)) %>%
  group_by(idhh) %>% 
  summarise(
    idhh                      = mean(idhh),
    urban                     = ifelse(any(drgur == 1),
                                       yes = 1,
                                       no = 0),
    n_male_over_14            = sum(dgn == 1 & dag >= 14),
    n_under_14                = sum(dag <= 14),
    n_between_15_29           = sum(dag %in% 15:29),
    n_between_30_44           = sum(dag %in% 30:44),
    n_between_45_59           = sum(dag %in% 45:59),
    n_over_60                 = sum(dag >= 60),
    n_employed                = sum(les == 2 | les == 3),
    n_students                = sum(les == 6),
    n_higher_education        = sum(deh >= 5),
    avg_years_educ            = mean(dey),
    malehead                  = max(ismalehead),
    weights                   = sum(dwt),
    region                    = as.factor(mean(drgn1))) %>% 
  select(-idhh, -weights)

pnad_shares_hat <- pnad_hh %>% 
  select(idhh) %>% 
  as_tibble()

for(var in share_vars){
  model_number <- which(share_vars == var)
  
  share_hat <- predict(ols_models[[model_number]],
                       newdata = covariates_pnad)
  
  prob_hat <- predict(probit_models[[model_number]], 
                      newdata = covariates_pnad, type="response")
  
  pnad_shares_hat[, ncol(pnad_shares_hat) + 1] <- prob_hat*share_hat
  
  colnames(pnad_shares_hat)[ncol(pnad_shares_hat)] <- paste0(var, "_hat")
}

pnad_shares_hat <- pnad_shares_hat %>% 
  mutate(pnad = 1)

pof_shares_hat <- pof_shares_hat %>% 
  mutate(pnad = 0) 

base_matching <- rbind(pnad_shares_hat,
                    pof_shares_hat)

matching_formula <- reformulate(termlabels =  colnames(pnad_shares_hat %>% select(-idhh, -pnad)),
                                response = "pnad")

matching <- matchit(formula = matching_formula,
                    distance = "mahalanobis",
                    method = "nearest",
                    data = base_matching,
                    replace = TRUE) #there are more "treated" than "control" observations

matching_matrix <- as.data.frame(matching$match.matrix) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  mutate(idhh_pnad = row_number(),
         idhh_match = base_matching$idhh[V1]) %>% 
  select(idhh_pnad, idhh_match)

#Constructing PNAD expenditure data by getting expenditure at the lowest aggregation level
#of matched households

matching_orighh <- merge(matching_matrix,
                           pof_hh %>% select(idhh, idorighh),
                           by.x = "idhh_match",
                           by.y = "idhh",
                           all.x = TRUE)

tables_pof_share <- merge(tables_pof,
                          pof_hh %>% select(idorighh, ils_dispy),
                          by = "idorighh",
                          all.x = TRUE) %>%
  filter(ils_dispy >0) %>% 
  mutate(value_share = V8000_DEFLA_new/ils_dispy)

tables_pnad_shares <- inner_join(matching_orighh,
                          tables_pof_share,
                          by= "idorighh",
                          ) %>% 
  select(idhh_pnad, V9001, value_share) %>% 
  rename(product_code = V9001) %>% 
  arrange(idhh_pnad)

tables_pnad_totals <- merge(tables_pnad_shares,
                            pnad_hh %>% select(idhh, ils_dispy),
                            by.x = "idhh_pnad",
                            by.y = "idhh",
                            all.x = T) %>% 
  mutate(value = value_share*ils_dispy)


#Save imputed expenditure values at the lowest level of aggregation
#This whole process takes a long time, so it's worth saving the result

saveRDS(tables_pnad_totals, 
        paste0("Expenditure imputation\\tables_pnad_totals_", as.character(year), ".rds"))

#Impute values into input file

#Aggregate by expenditure category

#Healthcare expenditures
pnad_xhl <- tables_pnad_totals %>% 
  filter(product_code %in% assist_saude) %>% 
  group_by(idhh_pnad) %>% 
  summarise(xhl = sum(value))

#Education expenditures
pnad_xed <- tables_pnad_totals %>% 
  filter(product_code %in% educacao) %>% 
  group_by(idhh_pnad) %>% 
  summarise(xed = sum(value))

#Private pension expenditures
pnad_xpp <- tables_pnad_totals %>% 
  filter(product_code %in% prev_priv) %>% 
  group_by(idhh_pnad) %>% 
  summarise(xpp = sum(value))

#Alimony expenditures
pnad_xmp <- tables_pnad_totals %>% 
  filter(product_code == 48009) %>% 
  group_by(idhh_pnad) %>% 
  summarise(xmp = sum(value))


#Get PNAD input database for BRASMOD
pnad_input <- fread(paste0("Input\\BR_", as.character(year), "_a1.txt"))

base_xhl<- merge(pnad_input,
                 pnad_xhl,
                 by.x = "idhh",
                 by.y = "idhh_pnad",
                 all.x = T) %>% 
  mutate(xhl = ifelse(xhl > 0,
                      yes = replace_na(xhl, 0),
                      no = 0))


base_xed <- merge(base_xhl,
                  pnad_xed,
                  by.x = "idhh",
                  by.y = "idhh_pnad",
                  all.x = T) %>% 
  mutate(xed = ifelse(xed > 0,
                      yes = replace_na(xed, 0),
                      no = 0))

base_xpp <- merge(base_xed,
                  pnad_xpp,
                  by.x = "idhh",
                  by.y = "idhh_pnad",
                  all.x = T) %>% 
  mutate(xpp = ifelse(xpp > 0,
                      yes = replace_na(xpp, 0),
                      no = 0))

base_xmp <- merge(base_xpp,
                  pnad_xmp,
                  by.x = "idhh",
                  by.y = "idhh_pnad",
                  all.x = T) %>% 
  mutate(xmp = ifelse(xmp > 0 & !is.na(xmp),
                      yes = replace_na(xmp, 0),
                      no = 0))

base_final_pnad_expenditures <- base_xmp %>% 
  mutate_all(~replace(., is.na(.), 0))


#Save new input database as a tab separated .txt 
write.table(base_final_pnad_expenditures, file=paste0("Input\\BR_", as.character(year), "_a2.txt"),
            quote=FALSE, sep='\t', row.names=FALSE)

