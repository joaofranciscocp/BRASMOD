#PACKAGES USED
library(tidyverse)
library(readxl)
library(data.table)
library(fastmatrix)
library(xgboost)
library(neuralnet)
library(caret)
library(stats)
library(MatchIt)

#READING THE DATA

#PNAD

setwd("C:\\Users\\joao.perez\\Desktop\\Projetos\\PNAD")

#Outputs
pnad_individual <- fread("output pnad2018 individual.txt", sep = "\t", 
                         header = T, dec = ",") %>% 
  mutate(across(everything(), as.numeric))

pnad_hh <- fread("output pnad2018 hh.txt", sep = "\t", 
                 header = T, dec = ",") %>% 
  mutate(across(everything(), as.numeric))

#POF

setwd("C:\\Users\\joao.perez\\Desktop\\Projetos\\POF")

#Outputs
pof_individual <- fread("output pof2018 individual.txt", sep = "\t", 
                        header = T, dec = ",") %>% 
  mutate(across(everything(), as.numeric))

pof_hh <- fread("output pof2018 hh.txt", sep = "\t", 
                header = T, dec = ",") %>% 
  mutate(across(everything(), as.numeric))

#Expenditure data

DESPESA_COLETIVA <- readRDS("Dados_20221226\\DESPESA_COLETIVA.rds") %>%  
  mutate(V9001 = str_sub(V9001, 1, -3)) %>% 
  mutate(idhh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>%
  mutate(V9011 = ifelse(is.na(V9011), 1, V9011)) %>% 
  filter(str_sub(V9001,-3) != "999" &
           as.numeric(V9002) <= 6) %>% 
  mutate(V8000_DEFLA_new = (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12) %>% 
  select(idhh, V9001, V8000_DEFLA_new, UF)

DESPESA_INDIVIDUAL <- readRDS("Dados_20221226\\DESPESA_INDIVIDUAL.rds") %>% 
  mutate(V9001 = str_sub(V9001, 1, -3)) %>% 
  mutate(idhh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>% 
  mutate(V9011 = ifelse(is.na(V9011), 1, V9011)) %>% 
  filter(str_sub(V9001,-3) != "999" &
           as.numeric(V9002) <= 6) %>% 
  mutate(V8000_DEFLA_new = (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12) %>% 
  select(idhh, V9001, V8000_DEFLA_new, UF)

CADERNETA_COLETIVA <- readRDS("Dados_20221226\\CADERNETA_COLETIVA.rds") %>%
  mutate(V9001 = substr(V9001, 1, 5)) %>%
  mutate(idhh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>%
  filter(str_sub(V9001,-3) != "999" &
           as.numeric(V9002) <= 6) %>% 
  mutate(V8000_DEFLA_new = (V8000_DEFLA*FATOR_ANUALIZACAO)/12) %>% 
  select(idhh, V9001, V8000_DEFLA_new, UF)

base_pof <- do.call(rbind,
                    list(DESPESA_COLETIVA, 
                         DESPESA_INDIVIDUAL,
                         CADERNETA_COLETIVA))

#Codes for expenditure categories (already taken from POF's own categorization)

tradutor <- readxl::read_xls("Tradutores_20210304\\Tradutor_Despesa_Geral_novo.xls", )

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

expenditures_pof <- base_pof %>%
  group_by(idhh) %>% 
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
    pensoes              = sum(V8000_DEFLA_new[V9001 %in% pensoes]),
    region               = as.factor(mean(as.numeric(substr(UF, 1,1)))))


#Individual data (just to be able to match BRASMOD idhhs to old unsorted idhh)

MORADOR <- readRDS("Dados_20221226\\MORADOR.rds") %>% 
  mutate(idhh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>%
  group_by(idhh) %>%
  mutate(idhh_sorted = cur_group_id(),
            idperson_sorted = paste0(idhh_sorted, "0", row_number())) %>% 
  select(idhh_sorted, idperson_sorted) %>% 
  mutate(across(everything(), as.numeric))

pof_individual_ids <- merge(pof_individual,
                            MORADOR,
                            by.x = "idperson",
                            by.y = "idperson_sorted",
                            all.x = T) %>% 
  rename(idhh_old = idhh.y)

expenditures_idhh <- merge(x = pof_individual_ids,
                           y = expenditures_pof,
                           by.x = "idhh_old",
                           by.y = "idhh",
                           all.x = T)

shares_idhh <- expenditures_idhh %>%
  group_by(idhh_old) %>%
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


shares_idhh_filtered <- shares_idhh %>% 
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
             share_pensoes             > 1)) %>% 
  group_by(idhh_old) %>% 
  summarise(idhh = mean(idhh.x),
              share_habitacao            =mean(share_habitacao         ),
              share_despesas_diversas    =mean(share_despesas_diversas ),
              share_prestacao            =mean(share_prestacao         ),
              share_impostos             =mean(share_impostos          ),
              share_imovel               =mean(share_imovel            ),
              share_outras               =mean(share_outras            ),
              share_investimentos        =mean(share_investimentos     ),
              share_cultura              =mean(share_cultura           ),
              share_contribuicoes_trab   =mean(share_contribuicoes_trab),
              share_fumo                 =mean(share_fumo              ),
              share_transporte           =mean(share_transporte        ),
              share_alimentacao          =mean(share_alimentacao       ),
              share_serv_banc            =mean(share_serv_banc         ),
              share_serv_pess            =mean(share_serv_pess         ),
              share_assist_saude         =mean(share_assist_saude      ),
              share_higiene              =mean(share_higiene           ),
              share_educacao             =mean(share_educacao          ),
              share_vestuario            =mean(share_vestuario         ),
              share_emprestimo           =mean(share_emprestimo        ),
              share_prev_priv            =mean(share_prev_priv         ),
              share_pensoes              =mean(share_pensoes           ))


demographic_variables <-  readRDS("Dados_20221226\\MORADOR.rds") %>% 
  mutate(idhh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>%
  mutate(ishead = ifelse(V0306 == 1,
                         yes = 1,
                         no  = 0)) %>%
  mutate(ismalehead = ifelse(ishead == 1 & V0404 == 1,
                             yes = 1,
                             no  = 0),
         isfemalehead = ifelse(ishead == 1 & V0404 == 2,
                               yes = 1,
                               no  = 0)) %>%
  group_by(idhh) %>%
  mutate(femalehead = max(isfemalehead),
         malehead   = max(ismalehead),
         idhh       = cur_group_id()) %>% 
  summarise(idhh                      = mean(idhh),
            urban                     = ifelse(any(TIPO_SITUACAO_REG == 1),
                                               yes = 1,
                                               no = 0),
            n_male_over_14            = sum(V0404 == 1 & V0403 >= 14),
            n_under_14                = sum(V0403 <= 14),
            n_between_15_29           = sum(V0403 %in% 15:29),
            n_between_30_44           = sum(V0403 %in% 30:44),
            n_between_45_59           = sum(V0403 %in% 45:59),
            n_over_60                 = sum(V0403 >= 60),
            n_employed                = sum(!is.na(V0407) & V0407 == 1),
            n_students                = sum(!is.na(V0419)),
            n_higher_education        = sum(INSTRUCAO >= 6),
            avg_years_educ            = mean(ANOS_ESTUDO, na.rm = T),
            malehead                  = mean(malehead),
            weights                   = sum(PESO_FINAL),
            region                    = (mean(as.numeric(substr(UF, 1,1)))),
            north                     = ifelse(region == 1,
                                               yes = 1,
                                               no = 0),
            northeast                 = ifelse(region == 2,
                                               yes = 1,
                                               no = 0),
            southeast                 = ifelse(region == 3,
                                               yes = 1,
                                               no = 0),
            south                     = ifelse(region == 4,
                                               yes = 1,
                                               no = 0))


final_covariates <- merge(demographic_variables,
                          pof_hh,
                          by.x = "idhh",
                          by.y = "idhh",
                          all.x = T)

final_base_pof <- merge(shares_idhh_filtered,
                        final_covariates,
                        by.x = "idhh",
                        by.y = "idhh",
                        all.y = T) %>% 
  na.omit()


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

dependent_vars <- demographic_variables %>% 
  select(-c("idhh", "weights", "region")) %>% 
  names()

#Loop to create and run models for each share variable

probit_models <- list()
ols_models <- list()

for(var in share_vars){
  model_number <- which(share_vars == var)
  
  model <- reformulate(termlabels =  dependent_vars,
                       response = var)
  
  probit <- glm(model, family = binomial(link = "probit"),
                data = final_base_pof)
  
  probit_models[[model_number]] <- probit
  
  ols <- lm(model, data = final_base_pof%>% 
              filter(get(var) > 0))
  
  ols_models[[model_number]] <- ols
}


#Prediciton for POF

covariates_pof <- demographic_variables %>%
  filter(idhh %in% shares_idhh_filtered$idhh) %>% #We're only going to use the "reasonable" households for matching
  select(-idhh, -weights, -region) %>% 
  na.omit()
  

pof_shares_hat <- demographic_variables %>%
  filter(idhh %in% shares_idhh_filtered$idhh) %>%
  na.omit() %>% 
  select(idhh)


for(var in share_vars){
  model_number <- which(share_vars == var)
  
  share_hat <- predict(ols_models[[model_number]],
                       newdata = covariates_pof)
  
  prob_hat <- predict(probit_models[[model_number]], 
                      newdata = covariates_pof, type="response")
  
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
    region                    = mean(drgn1),
    north                     = ifelse(region == 1,
                                       yes = 1,
                                       no = 0),
    northeast                 = ifelse(region == 2,
                                       yes = 1,
                                       no = 0),
    southeast                 = ifelse(region == 3,
                                       yes = 1,
                                       no = 0),
    south                     = ifelse(region == 4,
                                       yes = 1,
                                       no = 0)) %>% 
  select(-idhh, -weights, -region)

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
                    replace = TRUE) #there are more treated than control observations

matching_matrix <- as.data.frame(matching$match.matrix) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  mutate(idhh = row_number(),
         idhh_match = V1 - nrow(pnad_shares_hat)) %>% 
  select(idhh, idhh_match)

#Constructing PNAD expenditure data by getting expenditure at the lowest aggregation level
#of matched households

pnad_expenditures <- data.frame(matrix(ncol=3,nrow=0, 
                                       dimnames=list(NULL, c("idhh", "category", "value"))))



for(idhh_pnad in matching_matrix$idhh){
  idhh_pof <- matching_matrix$idhh_match[idhh_pnad]
  
  idhh_expenditures <- base_pof %>% 
    filter(idhh == idhh_pof)
  
  
  
  
}

pnad_expenditures <- merge(matching_matrix,
                           pnad_hh,
                           by.x = "idhh",
                           by.y = "idhh")

pof_expenditures <- shares_idhh %>% 
  group_by(idhh.x) %>% 
  summarise(idhh = mean(idhh.x),
            share_habitacao            =mean(share_habitacao         ),
            share_despesas_diversas    =mean(share_despesas_diversas ),
            share_prestacao            =mean(share_prestacao         ),
            share_impostos             =mean(share_impostos          ),
            share_imovel               =mean(share_imovel            ),
            share_outras               =mean(share_outras            ),
            share_investimentos        =mean(share_investimentos     ),
            share_cultura              =mean(share_cultura           ),
            share_contribuicoes_trab   =mean(share_contribuicoes_trab),
            share_fumo                 =mean(share_fumo              ),
            share_transporte           =mean(share_transporte        ),
            share_alimentacao          =mean(share_alimentacao       ),
            share_serv_banc            =mean(share_serv_banc         ),
            share_serv_pess            =mean(share_serv_pess         ),
            share_assist_saude         =mean(share_assist_saude      ),
            share_higiene              =mean(share_higiene           ),
            share_educacao             =mean(share_educacao          ),
            share_vestuario            =mean(share_vestuario         ),
            share_emprestimo           =mean(share_emprestimo        ),
            share_prev_priv            =mean(share_prev_priv         ),
            share_pensoes              =mean(share_pensoes           )) %>% 
  ungroup(idhh.x) 


  







  



  














