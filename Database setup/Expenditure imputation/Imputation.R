#PACKAGES USED
library(tidyverse)
library(readxl)
library(data.table)
library(MatchIt)
library(hutils)
library(StatMatch)

#SET WORKING DIRECTORY

#Set it to where the BRASMOD folder is

setwd("C:\\Users\\joao.perez\\Downloads\\brasmod\\brasmod")

#Set year for imputation

year <- 2018

#READING THE DATA

#PNAD

#Outputs from BRASMOD
pnad_individual <- fread(paste0("Database setup\\Expenditure imputation\\PNAD output data\\bra_", 
                                as.character(year), "_std.txt"), 
                         sep = "\t", 
                         header = T, dec = ",") %>% 
  mutate(across(everything(), as.numeric))

pnad_hh <- fread(paste0("Database setup\\Expenditure imputation\\PNAD output data\\bra_",
                        as.character(year), "_std_hh.txt"), sep = "\t", 
                 header = T, dec = ",") %>% 
  mutate(across(everything(), as.numeric))

#POF

#Outputs from BRASMOD
pof_individual <- fread(paste0("Database setup\\Expenditure imputation\\POF output data\\bra_", as.character(year), "_std.txt"), 
                        sep = "\t", 
                        header = T, dec = ",") %>% 
  mutate(across(everything(), as.numeric))

pof_hh <- fread(paste0("Database setup\\Expenditure imputation\\POF output data\\bra_", as.character(year), "_std_hh.txt"), 
                sep = "\t", 
                header = T, dec = ",") %>% 
  mutate(across(everything(), as.numeric))

#Correction for inflation for POF's 2018 monetary variables

cpi <- read_xls("Database setup\\Expenditure imputation\\CPI (IPCA).xls")
inflation_correction <- cpi$price_level[cpi$year == year]

#Expenditure data

DESPESA_COLETIVA <- readRDS("Database setup\\POF data\\DESPESA_COLETIVA.rds") %>%  
  mutate(V9001 = str_sub(V9001, 1, -3)) %>% 
  mutate(idorighh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>%
  mutate(V9011 = ifelse(is.na(V9011), 1, V9011)) %>% 
  filter(as.numeric(V9002) <= 6) %>% 
  mutate(V8000_DEFLA_new = (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)*inflation_correction/12) %>% 
  select(idorighh, V9001, V8000_DEFLA_new, UF)

DESPESA_INDIVIDUAL <- readRDS("Database setup\\POF data\\DESPESA_INDIVIDUAL.rds") %>% 
  mutate(V9001 = str_sub(V9001, 1, -3)) %>% 
  mutate(idorighh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>% 
  mutate(V9011 = ifelse(is.na(V9011), 1, V9011)) %>% 
  filter(as.numeric(V9002) <= 6) %>% 
  mutate(V8000_DEFLA_new = (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)*inflation_correction/12) %>% 
  select(idorighh, V9001, V8000_DEFLA_new, UF)

CADERNETA_COLETIVA <- readRDS("Database setup\\POF data\\CADERNETA_COLETIVA.rds") %>%
  mutate(V9001 = str_sub(V9001, 1, -3)) %>%
  mutate(idorighh = paste0(COD_UPA, NUM_DOM, NUM_UC)) %>%
  filter(as.numeric(V9002) <= 6) %>% 
  mutate(V8000_DEFLA_new = (V8000_DEFLA*FATOR_ANUALIZACAO)*inflation_correction/12) %>% 
  select(idorighh, V9001, V8000_DEFLA_new, UF)

tables_pof <- do.call(rbind,
                      list(DESPESA_COLETIVA, 
                           DESPESA_INDIVIDUAL,
                           CADERNETA_COLETIVA)) %>% 
  mutate(across(everything(), as.numeric))

morador_hh <- readRDS("Database setup\\POF data\\MORADOR.rds") %>%
  filter(as.numeric(V0306) < 17) %>% 
  mutate(idorighh = paste0(COD_UPA, NUM_DOM, NUM_UC),
         idorigperson = paste0(idorighh, COD_INFORMANTE)) %>% 
  group_by(idorighh) %>% 
  mutate(idhh = cur_group_id(), #the new household ID is the group's ID
         #the individual ID is just the new idhh + "0" + the row number within the household
         #so first individual in HH 1 is 101, second individual is 102, etc
         idperson = paste0(idhh, "0", row_number())) %>%
  summarise(idhh               = first(idhh),
            idorighh           = first(idorighh),
            RENDA_TOTAL        = mean(RENDA_TOTAL),
            RENDA_DISP         = sum(PC_RENDA_DISP),
            RENDA_MONET        = sum(PC_RENDA_MONET),
            DEDUCAO            = sum(PC_DEDUCAO),
            RENDA_DISP_MONET   = sum(PC_RENDA_MONET) - sum(PC_DEDUCAO),
            PESO               = first(PESO_FINAL),
            PC_RENDA_MONET     = mean(PC_RENDA_MONET),
            PC_RENDA_TOTAL     = mean(RENDA_TOTAL)/n())



#Codes for expenditure categories (already taken from POF's own categorization)

tradutor <- read_xls("Database setup\\Expenditure imputation\\general_expenditure_translator.xls", )

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

base_pof <- merge(morador_hh,
                  expenditures_pof,
                  by.x = "idorighh",
                  by.y = "idorighh",
                  all.x = TRUE) %>% 
  na.omit()

base_shares <- base_pof %>%
  filter(RENDA_DISP > 0) %>% 
  mutate(share_habitacao            = habitacao/RENDA_DISP,            
         share_despesas_diversas    = despesas_diversas/RENDA_DISP,    
         share_prestacao            = prestacao/RENDA_DISP,            
         share_impostos             = impostos/RENDA_DISP,             
         share_imovel               = imovel/RENDA_DISP,               
         share_outras               = outras/RENDA_DISP,               
         share_investimentos        = investimentos/RENDA_DISP,        
         share_cultura              = cultura/RENDA_DISP,              
         share_contribuicoes_trab   = contribuicoes_trab/RENDA_DISP,   
         share_fumo                 = fumo/RENDA_DISP,                 
         share_transporte           = transporte/RENDA_DISP,           
         share_alimentacao          = alimentacao/RENDA_DISP,          
         share_serv_banc            = serv_banc/RENDA_DISP,            
         share_serv_pess            = serv_pess/RENDA_DISP,            
         share_assist_saude         = assist_saude/RENDA_DISP,         
         share_higiene              = higiene/RENDA_DISP,              
         share_educacao             = educacao/RENDA_DISP,             
         share_vestuario            = vestuario/RENDA_DISP,            
         share_emprestimo           = emprestimo/RENDA_DISP,           
         share_prev_priv            = prev_priv/RENDA_DISP,            
         share_pensoes              = pensoes/RENDA_DISP) %>% 
  mutate(sum_shares = rowSums(across(starts_with("share"))))


idhh_shares_filtered <- base_shares %>% 
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
             share_pensoes             > 1 |
             sum_shares                > 2)) %>% 
  pull(idhh)



%>% 
  mutate(ntile = weighted_ntile(RENDA_DISP, n = 100, weights = PESO)) %>%
  #mutate(ntile = ntile(RENDA_DISP, 100)) %>%
  mutate(expenditure_sum = sum_shares*RENDA_MONET) %>% 
  group_by(ntile) %>% 
  summarise(sum_shares       = sum(sum_shares*PESO)/sum(PESO),
            sum_expenditures = sum(expenditure_sum*PESO)/sum(PESO),
            income       = sum(RENDA_DISP*PESO)/sum(PESO),
            sd               = sd(sum_shares)) %>% 
  mutate(quant_plus = sum_shares + sd,
         quant_min  = sum_shares - sd)

base_shares_monet <- base_pof %>%
  filter(RENDA_MONET > 0) %>% 
  mutate(share_habitacao            = habitacao/RENDA_MONET,            
         share_despesas_diversas    = despesas_diversas/RENDA_MONET,    
         share_prestacao            = prestacao/RENDA_MONET,            
         share_impostos             = impostos/RENDA_MONET,             
         share_imovel               = imovel/RENDA_MONET,               
         share_outras               = outras/RENDA_MONET,               
         share_investimentos        = investimentos/RENDA_MONET,        
         share_cultura              = cultura/RENDA_MONET,              
         share_contribuicoes_trab   = contribuicoes_trab/RENDA_MONET,   
         share_fumo                 = fumo/RENDA_MONET,                 
         share_transporte           = transporte/RENDA_MONET,           
         share_alimentacao          = alimentacao/RENDA_MONET,          
         share_serv_banc            = serv_banc/RENDA_MONET,            
         share_serv_pess            = serv_pess/RENDA_MONET,            
         share_assist_saude         = assist_saude/RENDA_MONET,         
         share_higiene              = higiene/RENDA_MONET,              
         share_educacao             = educacao/RENDA_MONET,             
         share_vestuario            = vestuario/RENDA_MONET,            
         share_emprestimo           = emprestimo/RENDA_MONET,           
         share_prev_priv            = prev_priv/RENDA_MONET,            
         share_pensoes              = pensoes/RENDA_MONET) %>% 
  mutate(sum_shares = rowSums(across(starts_with("share"))))


idhh_shares_filtered_monet <- base_shares_monet %>% 
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
             share_pensoes             > 1 |
             sum_shares                > 3)) %>% 
  mutate(ntile = weighted_ntile(PC_RENDA_MONET, n = 100, weights = PESO)) %>%
  #mutate(ntile = ntile(RENDA_MONET, 100)) %>% 
  mutate(expenditure_sum = sum_shares*RENDA_MONET) %>% 
  group_by(ntile) %>% 
  summarise(share_expenditure = sum(sum_shares*PESO)/sum(PESO),
            sum_expenditures  = sum(expenditure_sum*PESO)/sum(PESO),
            income            = sum(PC_RENDA_MONET*PESO)/sum(PESO),
            sd                = sd(sum_shares),
            n                 = n()) %>% 
  mutate(quant_plus = share_expenditure + 1.96*sd/sqrt(n),
         quant_min  = share_expenditure - 1.96*sd/sqrt(n))

base_shares_total <- base_pof %>%
  filter(RENDA_TOTAL > 0) %>% 
  mutate(share_habitacao            = habitacao/RENDA_TOTAL,            
         share_despesas_diversas    = despesas_diversas/RENDA_TOTAL,    
         share_prestacao            = prestacao/RENDA_TOTAL,            
         share_impostos             = impostos/RENDA_TOTAL,             
         share_imovel               = imovel/RENDA_TOTAL,               
         share_outras               = outras/RENDA_TOTAL,               
         share_investimentos        = investimentos/RENDA_TOTAL,        
         share_cultura              = cultura/RENDA_TOTAL,              
         share_contribuicoes_trab   = contribuicoes_trab/RENDA_TOTAL,   
         share_fumo                 = fumo/RENDA_TOTAL,                 
         share_transporte           = transporte/RENDA_TOTAL,           
         share_alimentacao          = alimentacao/RENDA_TOTAL,          
         share_serv_banc            = serv_banc/RENDA_TOTAL,            
         share_serv_pess            = serv_pess/RENDA_TOTAL,            
         share_assist_saude         = assist_saude/RENDA_TOTAL,         
         share_higiene              = higiene/RENDA_TOTAL,              
         share_educacao             = educacao/RENDA_TOTAL,             
         share_vestuario            = vestuario/RENDA_TOTAL,            
         share_emprestimo           = emprestimo/RENDA_TOTAL,           
         share_prev_priv            = prev_priv/RENDA_TOTAL,            
         share_pensoes              = pensoes/RENDA_TOTAL) %>% 
  mutate(sum_shares = rowSums(across(starts_with("share"))))


idhh_shares_filtered_total <- base_shares_total %>% 
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
             share_pensoes             > 1 |
             sum_shares                > 3)) %>% 
  mutate(ntile = weighted_ntile(PC_RENDA_TOTAL, n = 100, weights = PESO)) %>%
  #mutate(ntile = ntile(RENDA_TOTAL, 100)) %>% 
  mutate(expenditure_sum = sum_shares*RENDA_TOTAL) %>% 
  group_by(ntile) %>% 
  summarise(share_expenditure = sum(sum_shares*PESO)/sum(PESO),
            sum_expenditures  = sum(expenditure_sum*PESO)/sum(PESO),
            income            = sum(PC_RENDA_TOTAL*PESO)/sum(PESO),
            sd                = sd(sum_shares, na.rm = T),
            n                 = n()) %>% 
  mutate(quant_plus = share_expenditure + 1.96*sd/sqrt(n),
         quant_min  = share_expenditure - 1.96*sd/sqrt(n))



ggplot(data = idhh_shares_filtered_total) +
  geom_point(aes(x = ntile, y = share_expenditure), colour = "black") +
  geom_point(aes(x = ntile, y = quant_min), colour = "red") +
  geom_point(aes(x = ntile, y = quant_plus), colour = "red") +
  geom_point(aes(x = ntile, y = n), colour = "blue") +
  scale_y_continuous(limits = c(0.0, 1.3)) +
  theme_bw()


%>% #remove outliers
  pull(idhh) 

covariates_pof <- pof_individual %>% 
  filter(idhh %in% idhh_shares_filtered) %>% 
  group_by(idhh) %>% 
  summarise(idhh                       = mean(idhh),
            ils_dispy                  = sum(ils_dispy),           #Original income
            ils_benmt                  = sum(ils_benmt),           #Means-tested transfers
            #ils_pen                    = sum(ils_pen),             #Pensions
            n_kids                     = sum(dag <= 17),           #Number of kids
            n_adults                   = sum(dag %in% 18:59),      #Number of adults
            n_elders                   = sum(dag >= 60),           #Number of elders
            n_students                 = sum(les == 6),            #Number of students
            avg_dey                    = mean(dey),                #Average years of education
            urban                      = first(drgur),             #Urban household,
            state                      = as.factor(mean(drgn2)),   #State
            weights                    = sum(dwt),
            bin_benmt                  = as.factor(ifelse(ils_benmt > 0,
                                                          yes = 1,
                                                          no = 0)))

covariates_pnad <- pnad_individual %>%
  group_by(idhh) %>% 
  summarise(idhh                       = mean(idhh),
            ils_dispy                  = sum(ils_dispy),           #Original income
            ils_benmt                  = sum(ils_benmt),           #Means-tested transfers
            #ils_pen                    = sum(ils_pen),             #Pensions
            n_kids                     = sum(dag <= 17),           #Number of kids
            n_adults                   = sum(dag %in% 18:59),      #Number of adults
            n_elders                   = sum(dag >= 60),           #Number of elders
            n_students                 = sum(les == 6),            #Number of students
            avg_dey                    = mean(dey),                #Average years of education
            urban                      = first(drgur),             #Urban household,
            state                      = as.factor(mean(drgn2)),   #State
            weights                    = sum(dwt),
            bin_benmt                  = as.factor(ifelse(ils_benmt > 0,
                                                          yes = 1,
                                                          no = 0)))


#Create vector of independent variables

independent_vars <- covariates_pof %>% 
  select(-weights, -idhh, -state, -bin_benmt, -ils_benmt, -urban) %>% 
  names()

set.seed(1812)

matching_randhotdeck <- RANDwNND.hotdeck(data.rec    = as.data.frame(covariates_pnad),
                                         data.don    = as.data.frame(covariates_pof),
                                         match.vars  = independent_vars,
                                         dist.fun    = "Mahalanobis",
                                         weight.don  = "weights",
                                         don.class   = c("state", "bin_benmt"),
                                         cut.don     = "rot")

matching_matrix_rand <- as.data.frame(matching_randhotdeck$mtc.ids) %>% 
  mutate(idhh_pnad  = as.numeric(rec.id),
         idhh_match = covariates_pof$idhh[as.numeric(don.id)]) %>% 
  select(idhh_pnad, idhh_match)

summary_distances_rand <- as.data.frame(matching_randhotdeck$sum.dist)

matching_NNhotdeck <- NND.hotdeck(data.rec    = as.data.frame(covariates_pnad),
                                  data.don    = as.data.frame(covariates_pof),
                                  match.vars  = independent_vars,
                                  dist.fun    = "Mahalanobis",
                                  weight.don  = "weights",
                                  don.class   = c("state", "bin_benmt"),
                                  cut.don     = "rot")

matching_matrix_NN <- as.data.frame(matching_NNhotdeck$mtc.ids) %>% 
  mutate(idhh_pnad  = as.numeric(rec.id),
         idhh_match = covariates_pof$idhh[as.numeric(don.id)]) %>% 
  select(idhh_pnad, idhh_match)


matching_formula <- reformulate(termlabels =  independent_vars,
                                response = "pnad")

pnad_base <- covariates_pnad %>% 
  mutate(pnad = 1)

pof_base <- covariates_pof %>% 
  mutate(pnad = 0)

base_matching <- rbind(pnad_base, pof_base)

matching <- matchit(formula = matching_formula,
                    distance = "mahalanobis",
                    method = "nearest",
                    data = base_matching,
                    replace = TRUE)

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
                          morador_hh,
                          by = "idorighh",
                          all.x = TRUE) %>%
  filter(RENDA_DISP >0) %>% 
  mutate(value_share = V8000_DEFLA_new/RENDA_DISP)

tables_pnad_shares <- inner_join(matching_matrix %>% rename(idhh = idhh_match),
                                 tables_pof_share,
                                 by = "idhh") %>% 
  select(idhh_pnad, idhh, V9001, value_share) %>% 
  rename(product_code = V9001) %>% 
  arrange(idhh_pnad)

teste_tables_pnad_share <- tables_pnad_shares %>% 
  group_by(idhh_pnad) %>% 
  summarise(sum_shares = sum(value_share),
            idhh_pof   = first(idhh))


tables_pnad_totals <- merge(tables_pnad_shares,
                            pnad_hh %>% select(idhh, ils_dispy, ils_ben, ils_pen, dwt,
                                               ils_tax, ils_sicdy, ils_benmt, ils_bennt),
                            by.x = "idhh_pnad",
                            by.y = "idhh",
                            all.y = T) %>% 
  mutate(value = ifelse(ils_dispy > 0,
                        yes = value_share*ils_dispy,
                        no = 0)) %>% 
  group_by(idhh_pnad) %>% 
  summarise(idhh_pof  = first(idhh),
            dwt       = first(dwt),
            ils_dispy = mean(ils_dispy),
            ils_ben   = mean(ils_ben),
            total_income = ils_dispy + ils_ben,
            expenditure_sum = sum(value),
            expenditure_share = expenditure_sum/total_income) 


percentiles <- tables_pnad_totals %>% 
  mutate(ntile = weighted_ntile(total_income, dwt, n = 100)) %>% 
  group_by(ntile) %>% 
  summarise(expenditure_share = sum(expenditure_share*dwt)/sum(dwt))

ggplot(data = percentiles) +
  geom_point(aes(x = ntile, y = expenditure_share), colour = "black") +
  #geom_point(aes(x = ntile, y = quant_min), colour = "red") +
  #geom_point(aes(x = ntile, y = quant_plus), colour = "red") +
  #geom_point(aes(x = ntile, y = n), colour = "blue") +
  scale_y_continuous(limits = c(0.0, 1.3)) +
  theme_bw()


imputed_shares_pnad <- tables_pnad_shares %>% 
  group_by(idhh_pnad) %>% 
  summarise(
    habitacao            = sum(value_share[product_code %in% habitacao]),       
    despesas_diversas    = sum(value_share[product_code %in% despesas_diversas]),
    prestacao            = sum(value_share[product_code %in% prestacao]),        
    impostos             = sum(value_share[product_code %in% impostos]),          
    imovel               = sum(value_share[product_code %in% imovel]),           
    outras               = sum(value_share[product_code %in% outras]),
    investimentos        = sum(value_share[product_code %in% investimentos]),
    cultura              = sum(value_share[product_code %in% cultura]),           
    contribuicoes_trab   = sum(value_share[product_code %in% contribuicoes_trab]),
    fumo                 = sum(value_share[product_code %in% fumo]),
    transporte           = sum(value_share[product_code %in% transporte]),       
    alimentacao          = sum(value_share[product_code %in% alimentacao]),    
    serv_banc            = sum(value_share[product_code %in% serv_banc]),        
    serv_pess            = sum(value_share[product_code %in% serv_pess]),        
    assist_saude         = sum(value_share[product_code %in% assist_saude]),   
    higiene              = sum(value_share[product_code %in% higiene]),           
    educacao             = sum(value_share[product_code %in% educacao]),          
    vestuario            = sum(value_share[product_code %in% vestuario]),        
    emprestimo           = sum(value_share[product_code %in% emprestimo]),       
    prev_priv            = sum(value_share[product_code %in% prev_priv]),        
    pensoes              = sum(value_share[product_code %in% pensoes])) %>% 
  mutate(expenditure_share = rowSums(.))


#Impute values into input file

#Aggregate by expenditure category

#First we get expenditure categories included in personal income tax deductions
#If not doing it from the beginning, remember to get POF category product codes from translator above!

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

#Now we'll get expenditure categories in the National Accounts categorization
#We'll use those to compute indirect taxes using calculated effective tax rates by Silveira at al. (2022)

#Get codes for National Accounts categories

crosswalk_pof_nat_acc <- read_xlsx("Database setup\\Expenditure imputation\\Crosswalk POF - National Accounts.xlsx")

codes_nat_acc <- unique(crosswalk_pof_nat_acc$code_nat_acc)

#Create a dataframe that will have a column for idhh,
#and then one for every expenditure category in the National Accounts
#If not doing it from the beginning, remember to read the PNAD output to get pnad_hh!
expenditures_nat_acc <- pnad_hh %>% 
  select(idhh)


#This loops goes through the National Account categories
#and aggregates total household expenditure in each category.
#We then add the result as a column in the dataframe above
for(code in codes_nat_acc){
  codes_pof_list <- unique(crosswalk_pof_nat_acc %>%   #Get POF codes from the crosswalk
                             filter(code_nat_acc == code) %>% 
                             pull(code_pof)) 
  
  expenditure_values <- tables_pnad_totals %>%       #Aggregate expenditures by household
    filter(product_code %in% codes_pof_list) %>% 
    group_by(idhh_pnad) %>% 
    summarise(x = sum(value))
  
  
  expenditures_nat_acc <- merge(expenditures_nat_acc, #Join aggregated expenditure values into dataframe created above
                                expenditure_values,
                                by.x = "idhh",
                                by.y = "idhh_pnad",
                                all.x = T)
  
  colnames(expenditures_nat_acc)[ncol(expenditures_nat_acc)] <- paste0("x", as.character(code)) #Rename column
}

expenditures_nat_acc <- expenditures_nat_acc %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(x00 = rowSums(across(starts_with("x"))))



#Get PNAD input database for BRASMOD
pnad_input <- fread(paste0("Input\\BR_", as.character(year), "_a1.txt"))

base_xhl<- merge(pnad_input,
                 pnad_xhl,
                 by.x = "idhh",
                 by.y = "idhh_pnad",
                 all.x = T) %>% 
  group_by(idhh) %>% 
  mutate(xhl = ifelse(row_number(),
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

base_nat_acc <- merge(base_xmp,
                      expenditures_nat_acc,
                      by.x = "idhh",
                      by.y = "idhh",
                      all.x = T)

base_final_pnad_expenditures <- base_nat_acc %>% 
  mutate_all(~replace(., is.na(.), 0))

base_total_expenditures <- base_final_pnad_expenditures %>% 
  select(-c("xed", "xhl", "xpp", "xmp")) %>% 
  mutate(x00 = rowSums(across(starts_with("x"))),
         disp_income = yem + yse + yiy + bun + poa + bdioa + yprrt) %>% 
  select(idhh, idperson, yem, yse, yiy, yhh, bun, poa, bdioa, yprrt, disp_income, x00)


total_expenditures_ntiles <- base_total_expenditures %>% 
  group_by(idhh) %>% 
  summarise(total_income = sum(disp_income),
            total_expenditures = mean(expenditures_sum)) %>% 
  mutate(ntile = ntile(total_income, 100)) %>% 
  group_by(ntile) %>% 
  summarise(avg_expenditures = mean(total_expenditures))

ggplot(data = total_expenditures_ntiles) +
  geom_point(aes(x = ntile, y = avg_expenditures)) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  theme_bw()


#Save new input database as a tab separated .txt 
write.table(base_x, file=paste0("Input\\BR_", as.character(year), "_a2.txt"),
            quote=FALSE, sep='\t', row.names=FALSE)



base_teste <- fread(paste0("Input\\BR_", as.character(year), "_a2.txt"),
                    sep = "\t", dec = ".")

base_x <- base_teste %>%
  group_by(idhh) %>% 
  mutate(first = (row_number() == first(row_number()))) %>% 
  mutate_at(vars(starts_with("x")), ~.*first) %>% 
  select(-first)



