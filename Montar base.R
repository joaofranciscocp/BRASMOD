# PARTE 1: LEITURA, ORGANIZAÇÃO E ARMAZENAMENTO DOS DADOS DA POF 2017-2018 
#EM ARQUIVOS .RDS


#Definir diretório: "....." indica a pasta/diretório de trabalho no HD local 
# separados por "/"  onde se encontram os arquivos .txt descompactados do 
# arquivo Dados_aaaammdd.zip
# Exemplo: setwd("c:/POF2018/Dados_aaaammdd/")

setwd("C:\\Users\\joao.perez\\Desktop\\Projetos\\POF\\Dados_20221226")

# REGISTRO - MORADOR

MORADOR <- 
  read.fwf("MORADOR.txt" 
           , widths = c(2,4,1,9,2,1,2,2,1,2,2,4,3,1,1,
                        1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,2,1,1,2,1,1,2,1,1,1,
                        2,1,2,14,14,10,1,1,20,20,20,20)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", 
                           "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE",
                           "V0306", "V0401", "V04021", "V04022", "V04023",
                           "V0403", "V0404", "V0405", "V0406", "V0407",
                           "V0408", "V0409", "V0410", "V0411", "V0412",
                           "V0413", "V0414", "V0415", "V0416", 
                           "V041711", "V041712", "V041721", "V041722",
                           "V041731", "V041732", "V041741", "V041742",
                           "V0418", "V0419", "V0420", "V0421", "V0422",
                           "V0423", "V0424", "V0425", "V0426", "V0427",
                           "V0428", "V0429", "V0430", "ANOS_ESTUDO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL",
                           "INSTRUCAO", "COMPOSICAO", "PC_RENDA_DISP",
                           "PC_RENDA_MONET", "PC_RENDA_NAO_MONET",
                           "PC_DEDUCAO")
           , dec="."
  )

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(MORADOR,"MORADOR.rds")

MORADOR <- readRDS("MORADOR.rds")

# REGISTRO - RENDIMENTO DO TRABALHO

RENDIMENTO_TRABALHO <- 
  read.fwf("RENDIMENTO_TRABALHO.txt" 
           , widths = c(2,4,1,9,2,1,2,2,1,1,7,1,1,1,1,1,1,7,7,
                        7,7,2,2,3,1,12,10,10,10,10,1,1,14,14,
                        10,4,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SUB_QUADRO",
                           "SEQ", "V9001", "V5302", "V53021", "V5303",
                           "V5304", "V5305", "V5307", "V8500", "V531112",
                           "V531122", "V531132", "V9010", "V9011",
                           "V5314", "V5315", "DEFLATOR", "V8500_DEFLA",
                           "V531112_DEFLA", "V531122_DEFLA",
                           "V531132_DEFLA", "COD_IMPUT_VALOR",
                           "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL","V53011","V53061")
           , dec="."
  )

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(RENDIMENTO_TRABALHO,"RENDIMENTO_TRABALHO.rds")

RENDIMENTO_TRABALHO <- readRDS("RENDIMENTO_TRABALHO.rds")


# REGISTRO - OUTROS RENDIMENTOS

OUTROS_RENDIMENTOS <- 
  read.fwf("OUTROS_RENDIMENTOS.txt" 
           , widths = c(2,4,1,9,2,1,2,2,2,7,10,10,2
                        ,2,12,10,10,1,1,14,14,10
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
                           "V8500", "V8501", "V9010", "V9011",
                           "DEFLATOR", "V8500_DEFLA", "V8501_DEFLA",
                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL")
           , dec="."
  )   

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(OUTROS_RENDIMENTOS,"OUTROS_RENDIMENTOS.rds")

OUTROS_RENDIMENTOS <- readRDS("OUTROS_RENDIMENTOS.rds")

# REGISTRO - DESPESA INDIVIDUAL

DESPESA_INDIVIDUAL <- 
  read.fwf("DESPESA_INDIVIDUAL.txt" 
           , widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2
                        ,2,1,1,1,12,10,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
                           "V9002", "V8000", "V9010", "V9011", "V9012",
                           "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004")
           , dec="."
  )

# Armazena no HD local arquivo serializado para leituras futuras
saveRDS(DESPESA_INDIVIDUAL,"DESPESA_INDIVIDUAL.rds")

DESPESA_INDIVIDUAL <- readRDS("DESPESA_INDIVIDUAL.rds")

#PARTE 2: CRIAR O ARQUIVO .TXT COM AS VARIÁVEIS NECESSÁRIAS LISTADAS
#NO DRD (DATA REQUIREMENT DOCUMENT) DO EUROMOD

#Pacotes que vão ser utilizados
library(tidyverse)
library(readxl)

#Definir diretório: "....." indica a pasta/diretório de trabalho no HD local 
# separados por "/"  onde se encontram os arquivos .xls descompactados do 
# arquivo Tradutores_aaaammdd.zip
# Exemplo: setwd("c:/POF2018/Tradutores_aaaammdd/")

setwd("C:\\Users\\joao.perez\\Desktop\\Projetos\\POF\\Tradutores_20210304")

tradutor_renda <- readxl::read_xls('Tradutor_Rendimento.xls')
tradutor_renda <- tradutor_renda[-nrow(tradutor_renda),] #Remover última linha

#SALÁRIO 

#Códigos dos tipos de rendimentos do trabalho:
TRABALHO <- tradutor_renda$Codigo[tradutor_renda$Descricao_3 == 'Empregado']

#Pega só os 5 primeiros digitos dos códigos
RENDIMENTO_TRABALHO$V9001 <- substr(RENDIMENTO_TRABALHO$V9001, 1, 5)

#Substitui NAs na variável "nº de meses em que rendimento foi recebido" por -1 
RENDIMENTO_TRABALHO$V9011[is.na(RENDIMENTO_TRABALHO$V9011)] <- -1


#Pega o rendimento, multiplica pela quantidade de meses em que ele é recebido,
#anualiza e divide por 12
RENDIMENTO_TRABALHO[, c('V8500','V8500_DEFLA')] <- apply(
  RENDIMENTO_TRABALHO[, c('V8500','V8500_DEFLA')],2,
  function(vetor) (vetor * RENDIMENTO_TRABALHO$V9011 * RENDIMENTO_TRABALHO$FATOR_ANUALIZACAO)/12)


#Pega todos os rendimentos do trabalho e agrega (soma) por indivíduo e domicílio
trabalho_agregado <- aggregate(
  RENDIMENTO_TRABALHO$V8500_DEFLA[RENDIMENTO_TRABALHO$V9001 %in% TRABALHO],
  by = list(COD_UPA = RENDIMENTO_TRABALHO$COD_UPA[RENDIMENTO_TRABALHO$V9001 %in% TRABALHO],
            NUM_DOM = RENDIMENTO_TRABALHO$NUM_DOM[RENDIMENTO_TRABALHO$V9001 %in% TRABALHO],
            NUM_UC = RENDIMENTO_TRABALHO$NUM_UC[RENDIMENTO_TRABALHO$V9001 %in% TRABALHO],
            COD_INFORMANTE = RENDIMENTO_TRABALHO$COD_INFORMANTE[RENDIMENTO_TRABALHO$V9001 %in% TRABALHO]),
  FUN = sum, 
  na.rm = T)

#Pega a tabela MORADOR e trabalho_agregado e junta as duas, identificando cada
#linha pelo código da UPA (unidade primária de amostragem),
#o número da UC (unidade de consumo), e o código do informante

base_yem <- merge(MORADOR,
              trabalho_agregado,
              by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              all.x = T)

#Renomeia última coluna (soma dos rendimentos do trabalho calculada acima) 
#para "yem", de "income (y)" e "employment (em)"

names(base_yem)[length(base_yem)] <- "yem"


#RENDA TRABALHO AUTÔNOMO

#Pega os códigos para renda por conta própria
AUTONOMO <- tradutor_renda$Codigo[tradutor_renda$Descricao_3 == 'Conta propria']

#Pega todos os rendimentos autônomos e agrega (soma) por indivíduo e domicílio

autonomo_agregado <- aggregate(
  RENDIMENTO_TRABALHO$V8500_DEFLA[RENDIMENTO_TRABALHO$V9001 %in% AUTONOMO],
  by = list(COD_UPA = RENDIMENTO_TRABALHO$COD_UPA[RENDIMENTO_TRABALHO$V9001 %in% AUTONOMO],
            NUM_DOM = RENDIMENTO_TRABALHO$NUM_DOM[RENDIMENTO_TRABALHO$V9001 %in% AUTONOMO],
            NUM_UC = RENDIMENTO_TRABALHO$NUM_UC[RENDIMENTO_TRABALHO$V9001 %in% AUTONOMO],
            COD_INFORMANTE = RENDIMENTO_TRABALHO$COD_INFORMANTE[RENDIMENTO_TRABALHO$V9001 %in% AUTONOMO]),
  FUN = sum, 
  na.rm = T)

#Pega a tabela base_yem e autonomo_agregado e junta as duas, identificando cada
#linha pelo código da UPA (unidade primária de amostragem),
#o número da UC (unidade de consumo), e o código do informante

base_yse <- merge(base_yem,
              autonomo_agregado,
              by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              all.x = T)

#Renomeia última coluna (soma dos rendimentos autônomos calculada acima) 
#para "yse", de "income (y)" e "self-employment (se)"

names(base_yse)[length(base_yse)] <- "yse"

#NÚMERO DE HORAS TRABALHADAS

#códigos para rendimentos de trabalho empregado e autônomo
TRAB_E_AUTONOMO <- c(TRABALHO, AUTONOMO)

#Pega todas as horas trabalhadas (empregado e autônomo) e agrega (soma) por indivíduo e domicílio
trabalho_e_autonomo_agregado <- aggregate(
  RENDIMENTO_TRABALHO$V5314[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO],
  by = list(COD_UPA = RENDIMENTO_TRABALHO$COD_UPA[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO],
            NUM_DOM = RENDIMENTO_TRABALHO$NUM_DOM[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO],
            NUM_UC = RENDIMENTO_TRABALHO$NUM_UC[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO],
            COD_INFORMANTE = RENDIMENTO_TRABALHO$COD_INFORMANTE[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO]),
  FUN = sum, 
  na.rm = T)

#Pega a tabela base_yse e trabalho_e_autonomo_agregado e junta as duas, 
#identificando cada linha pelo código da UPA (unidade primária de amostragem),
#o número da UC (unidade de consumo), e o código do informante

base_lhw <- merge(base_yse,
                  trabalho_e_autonomo_agregado,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T)

#Renomeia última coluna (soma das horas trabalhadas calculada acima) 
#para "lhw", de "labour (l) hours (h) weekly (w)"
names(base_lhw)[length(base_lhw)] <- "lhw"

#OCUPAÇÃO

#Filtra as observações que não têm NA no código da ocupação (V53011),
#e pega só o primeiro número do código
RENDIMENTO_TRABALHO$V53011[!is.na(RENDIMENTO_TRABALHO$V53011)] <- substr(RENDIMENTO_TRABALHO$V53011[!is.na(RENDIMENTO_TRABALHO$V53011)], 1, 1)


#Pega todas as ocupações (empregado e autônomo) e agrega por indivíduo e domicílio
ocupacao_agreggado <- aggregate(
  RENDIMENTO_TRABALHO$V53011[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO],
  by = list(COD_UPA = RENDIMENTO_TRABALHO$COD_UPA[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO],
            NUM_DOM = RENDIMENTO_TRABALHO$NUM_DOM[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO],
            NUM_UC = RENDIMENTO_TRABALHO$NUM_UC[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO],
            COD_INFORMANTE = RENDIMENTO_TRABALHO$COD_INFORMANTE[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO]),
  FUN = function(x) paste(x[!is.na(x)], collapse = ""))

#Pega a tabela base_lhw e a ocupacao_agregado e junta as duas,
#identificando cada linha pelo código da UPA (unidade primária de amostragem),
#o número da UC (unidade de consumo), e o código do informante
base_loc <- merge(base_lhw,
              ocupacao_agreggado,
              by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              all.x = T)

#Renomeia a última coluna para "loc", de "labour (l) ocupation (oc)"
names(base_loc)[length(base_loc)] <- "loc"

#Substitui tudo que não for um número de 0 a 9 por "" (string vazia)
base_loc$loc <- gsub('[^0-9]', '', base_loc$loc)
#Substitui as strings vazias por "0"
base_loc$loc[base_loc$loc == ''] <- '0'


#STATUS ECONÔMICO

#Pega todos os status econômicos e agrega por indivíduo e domicílio
status_agregado <- aggregate(
  RENDIMENTO_TRABALHO$V5302[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO],
  by = list(COD_UPA = RENDIMENTO_TRABALHO$COD_UPA[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO],
            NUM_DOM = RENDIMENTO_TRABALHO$NUM_DOM[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO],
            NUM_UC = RENDIMENTO_TRABALHO$NUM_UC[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO],
            COD_INFORMANTE = RENDIMENTO_TRABALHO$COD_INFORMANTE[RENDIMENTO_TRABALHO$V9001 %in% TRAB_E_AUTONOMO]),
  FUN = function(x) paste(x[!is.na(x)], collapse = ""))


#Pega a tabela base_loc e a status_agregado e junta as duas,
#identificando cada linha pelo código da UPA (unidade primária de amostragem),
#o número da UC (unidade de consumo), e o código do informante
base_les <- merge(base_loc,
              status_agregado,
              by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              all.x = T)

#Renomeia a última coluna para "les", de "labour (l) economic status (es)"
names(base_les)[length(base_les)] <- "les"

#Substitui tudo que não for um número de 0 a 9 por "" (string vazia)
base_les$les <- gsub('[^0-9]', '', base_les$les)
#Substitui as strings vazias por "0"
base_les$les[base_les$les == ''] <- '0'


#INVESTIMENTOS

#Códigos para rendimentos de investimentos
INVESTIMENTOS <- c(5401401, 5501201, 5501301, 5501601, 5501602, 5594401,
                   5504801, 5506001, 5506101, 5600101, 5600201, 5600301,
                   5600401, 5700101, 5700201, 5700301, 5700401)

#Pega os rendimentos, multiplica pela quantidade de meses em que ele é recebido,
#e divide por 12
OUTROS_RENDIMENTOS[, c('V8500','V8500_DEFLA')] <- apply(
  OUTROS_RENDIMENTOS[, c('V8500','V8500_DEFLA')],2,
  function(vetor) (vetor * OUTROS_RENDIMENTOS$V9011 * OUTROS_RENDIMENTOS$FATOR_ANUALIZACAO)/12)


#Pega todos os rendimentos de investimento e agrega (soma) por indivíduo e domicílio
investimento_agregado <- aggregate(
  OUTROS_RENDIMENTOS$V8500_DEFLA[OUTROS_RENDIMENTOS$V9001 %in% INVESTIMENTOS],
  by = list(COD_UPA = OUTROS_RENDIMENTOS$COD_UPA[OUTROS_RENDIMENTOS$V9001 %in% INVESTIMENTOS],
            NUM_DOM = OUTROS_RENDIMENTOS$NUM_DOM[OUTROS_RENDIMENTOS$V9001 %in% INVESTIMENTOS],
            NUM_UC = OUTROS_RENDIMENTOS$NUM_UC[OUTROS_RENDIMENTOS$V9001 %in% INVESTIMENTOS],
            COD_INFORMANTE = OUTROS_RENDIMENTOS$COD_INFORMANTE[OUTROS_RENDIMENTOS$V9001 %in% INVESTIMENTOS]),
  FUN = sum, 
  na.rm = T)

#Pega a tabela base_les e a investimento_agregado e junta as duas,
#identificando cada linha pelo código da UPA (unidade primária de amostragem),
#o número da UC (unidade de consumo), e o código do informante
base_yiy <- merge(base_les,
              investimento_agregado,
              by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              all.x = T)

#Renomeia a última coluna para "yiy", de "income (y)", "investiment (i)" e "yearly (y)"
names(base_yiy)[length(base_yiy)] <- "yiy"


#APOSENTADORIAS

#Códigos para aposentadorias
APOSENTADORIAS <- c(5400201, 5400401, 5400501, 5400601, 5400701, 5400801,
                    5400901, 5403101, 5503301, 5504601, 5505001, 5506201,
                    5506401, 5500301, 5500401, 5500501, 5500601, 5500701)

### CORRIGIR O FATO DE QUE, NO QUADRO 55, OS ITENS SÃO REGISTRADOS COM OS SEUS VALORES ANUAIS, ENQUANTO O DRD EXIGE VALORES MENSAIS

#Pega todas as aposentadorias e agrega (soma) por indivíduo e domicílio
aposentadorias_agregado <- aggregate(
  OUTROS_RENDIMENTOS$V8500_DEFLA[OUTROS_RENDIMENTOS$V9001 %in% APOSENTADORIAS],
  by = list(COD_UPA = OUTROS_RENDIMENTOS$COD_UPA[OUTROS_RENDIMENTOS$V9001 %in% APOSENTADORIAS],
            NUM_DOM = OUTROS_RENDIMENTOS$NUM_DOM[OUTROS_RENDIMENTOS$V9001 %in% APOSENTADORIAS],
            NUM_UC = OUTROS_RENDIMENTOS$NUM_UC[OUTROS_RENDIMENTOS$V9001 %in% APOSENTADORIAS],
            COD_INFORMANTE = OUTROS_RENDIMENTOS$COD_INFORMANTE[OUTROS_RENDIMENTOS$V9001 %in% APOSENTADORIAS]),
  FUN = sum, 
  na.rm = T)

#Pega a tabela base_yiy e a aposentadorias_agregado e junta as duas,
#identificando cada linha pelo código da UPA (unidade primária de amostragem),
#o número da UC (unidade de consumo), e o código do informante
base_poa <- merge(base_yiy,
              aposentadorias_agregado,
              by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              all.x = T)

#Renomeia a última coluna de "base_poa" para "poa",
#de "pensions (p)" e "old age (oa)"
names(base_poa)[length(base_poa)] <- "poa"


#IDs

#Pega na tabela MORADOR as variáveis de interesse para os próximos passos:
#V0306 indica a condição (parentsco ou natureza de subordinação) em referência
#ao chefe do domicílio, e V0404 indica o sexo do indivíduo (1 = homem, 2 = mulher)
morador_IDs <- MORADOR %>% 
  select(c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE", "V0306", "V0404")) %>% 
  rename(condicao = V0306, sexo = V0404)

#Pega a tabela base_poa e a morador_IDs e junta as duas,
#identificando cada linha pelo código da UPA (unidade primária de amostragem),
#o número da UC (unidade de consumo), e o código do informante
base_IDs <- merge(base_poa,
                  morador_IDs,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T)

#Cria um ID para o domicílio (household) concatenando
#o código da UPA (unidade primária de amostragem), o número do domicílio e
#o número da UC (unidade de consumo)
base_IDs$idhh <- paste(base_IDs$COD_UPA, base_IDs$NUM_DOM, base_IDs$NUM_UC, sep='')

#Cria um ID para o indivíduo, só adicionando o código do informante ao ID acima
base_IDs$idperson <- paste(base_IDs$COD_UPA, base_IDs$NUM_DOM, base_IDs$NUM_UC, base_IDs$COD_INFORMANTE, sep='')

#Transforma o idhh em número e ordena por ele
base_IDs$idhh <- as.double(base_IDs$idhh)
base_IDs <- base_IDs[order(base_IDs$idhh),]

y <- base_IDs
base_IDs <- y

#Abaixo tem dois for-loops pra poder organizar melhor os IDs individuais e do domicílio.

#Primeiro, criamos uma variável auxiliar x que começa em 1.
#O primeiro loop passa então por cada observação: dá o valor de x ao idhh; e se a observação
#seguinte estiver em outro domicílio, aumenta x em 1. Assim, cada domicílio tem um
#idhh que é diferente, começando por 1.

x <- 1

base_IDs$idhh_sorted <- base_IDs$idhh

for(n in 1:(length(base_IDs$idhh_sorted))) {
  tryCatch({
    if(base_IDs$idhh[[n]] < base_IDs$idhh[[n+1]]) {
      base_IDs$idhh_sorted[[n]] <- x
      x <- x + 1
      
    } 
    else {
      base_IDs$idhh_sorted[[n]] <- x
    }
  }, error = function(e) {
    #o assignment precisa ser global (<<-) pra sair da função também
    base_IDs$idhh_sorted[[n]] <<- x 
  })
}

#Igualamos o idhh ao idhh_sorted criado, e removemos este. 
#Depois, trazemos de volta pra string
base_IDs$idhh <- base_IDs$idhh_sorted
base_IDs <- base_IDs %>% select(-c(idhh_sorted))
base_IDs$idhh <- as.character(base_IDs$idhh)


#Agora, para os indivíduos, começamos criando uma nova variável igual ao idhh,
#mas com um "0" a mais (vai servir pra separar a parte do idperson que é idhh)
base_IDs$idperson_sorted <- paste0(as.character(base_IDs$idhh), "0")

#Novamente, variável auxiliar x começando em 1. Esse segundo for-loop passa por cada
#observação: primeiro, adiciona x ao novo idperson (idhh + 0); se a próxima observação
#for do mesmo domicílio, aumenta x em 1; se for de outro domicílio, volta x para 1.

x <- 1

for (n in 1:length(base_IDs$idperson_sorted)) {
  tryCatch({
    if(base_IDs$idperson_sorted[[n]] < base_IDs$idperson_sorted[[n+1]]) {
      base_IDs$idperson_sorted[[n]] <- paste0(base_IDs$idperson_sorted[[n]], as.character(x))
      x <- 1
      
    } 
    else {
      base_IDs$idperson_sorted[[n]] <- paste0(base_IDs$idperson_sorted[[n]], as.character(x))
      x <- x+ 1
    }
  }, error = function(e) {
    #o assignment precisa ser global (<<-) pra sair da função também
    base_IDs$idperson_sorted[[n]] <<- paste0(base_IDs$idperson_sorted[[n]], as.character(x)) 
  })
}

base_IDs$idperson <- base_IDs$idperson_sorted  
base_IDs <- base_IDs %>% select(-c(idperson_sorted))

#Cria a variável idhead, sendo igual a idperson se o indivíduo for chefe do domicílio,
#e 0 se não; depois agrupa por domicílio e dá o idperson do head como idhead para todos
base_IDs <- base_IDs %>% 
  mutate(idhead = ifelse(base_IDs$condicao == 1,
                          yes = as.numeric(base_IDs$idperson),
                          no = 0)) %>% 
  group_by(idhh) %>% 
  mutate(idhead = as.character(max(idhead)))

#Cria duas variáveis binárias pra identificar heads homens e heads mulheres,
#depois agrega por domicílio e identifica se o domicílio tem head homem ou mulher
base_IDs <- base_IDs %>% 
  mutate(malehead = ifelse(sexo == 1 & condicao == 1, #identifica heads homens
                           yes = 1,
                           no = 0),
         femalehead = ifelse(sexo == 2 & condicao == 1, #identifica heads mulheres
                             yes = 1,
                             no = 0)) %>% 
  group_by(idhh) %>% 
  mutate(malehead = max(malehead), femalehead = max(femalehead))

#Pra checar se essa última parte deu certo, o resultado abaixo tem que dar 0:
#print(as.vector(base_IDs$malehead) %*% as.vector(base_IDs$femalehead))

#Identifica os indivíduos do domicílio com referência ao head ("pessoa de referência da UC"):
base_IDs <- base_IDs %>% 
  mutate(idpartner = ifelse(condicao == 2 | condicao == 3, #se cônjuge do head
                            yes = idhead,
                            no = "0"),
         idfather = ifelse(
           malehead == 1 & 
             (condicao == 4 | condicao == 5 | condicao == 6), #se filho do head e head for homem
                           yes = idhead,
                           no = "0"),
         idmother = ifelse(
           femalehead == 1 & 
             (condicao == 4 | condicao == 5 | condicao == 6), #se filho do head e head for mulher
                           yes = idhead,
                           no = "0"))

#Faz com que o idpartner do head do domicílio seja o id do indivíduo que é
#partner do head (deve ter um jeito mais fácil e bonito de fazer isso, mas esse funciona)
base_IDs <- base_IDs %>% 
  mutate(idpartnerofhead = ifelse(idpartner == idhead, #se a pessoa for partner do head
                                  yes = idperson, #idpartner é o id dela
                                  no = "0")) %>% 
  group_by(idhh) %>% #agrupa por domicílio
  mutate(idpartnerofhead = as.character(max(as.numeric(idpartnerofhead)))) %>% #agrega o idpartnerofhead
  ungroup(idhh) %>% #desagrupa
  mutate(idpartner = ifelse(condicao == 1, #se a pessoa for head
                            yes = idpartnerofhead, #o idpartner dela é o idpartnerofhead
                            no = idpartner)) #se não, mantém

#AJUSTES FINAIS

base <- base_IDs

#Cria a variável dct (código do país baseado no ISO 3166-1), Brasil é 076
base$dct <- "076"

#Renomeia variáveis demográficas (d) pro padrão do DRD
#dwt = peso amostral (weight total)
#dag = idade (age)
#dec = nível educacional (education)
#dgn = gênero (gender)

base <- base %>% 
  rename(dwt = PESO_FINAL,
         dag = V0403,
         dec = V0419,
         dgn = V0404)

#A variável dms identifica se o indivíduo é casado (1) ou não (0)
base$dms <- ifelse(base$idpartner != 0,
                   yes = 1,
                   no = 0)

#A variável ddi identifica se o indivíduo tem alguma deficiência,
#e a POF não dá essa informação; as convenções do Euromod sugerem usar -1
base$ddi <- -1

#Salva tudo como string
base <- base %>% 
  mutate(across(everything(), as.character))

#Seleciona as variáveis obrigatórias pro Euromod
base_final <- base %>% 
  select(idhh, idperson, idfather, idmother, idpartner, dct, dwt, dag,
         dec, les, yem, dgn, lhw, dms, loc, yse, yiy, ddi, poa)

#Substitui os NAs por -1
base_final[is.na(base_final)] <- "-1"

#Definir diretório: "....." indica a pasta/diretório de trabalho no HD local 
#separados por "/"  onde se deseja salvar o arquivo txt para usar como input no Euromod
setwd("C:\\Users\\joao.perez\\Desktop\\Projetos\\POF")

#Salvar arquivos em .txt separados por tab
write.table(base_final, file='pof euromod.txt', quote=FALSE, sep='\t', row.names=FALSE)

#Salva uma versão com todas as variáveis, pra adicionar mais no futuro
write.table(base, file='pof euromod raw.txt', quote=FALSE, sep='\t', row.names=FALSE)



#PARTE 3: ADICIONAR MAIS VARIÁVEIS, NÃO-OBRIGATÓRIAS, NA BASE

setwd("C:\\Users\\joao.perez\\Desktop\\Projetos\\POF")
base <- read.csv(file = 'pof euromod raw.txt', sep = '\t', header = TRUE)


#GASTOS COM SAÚDE

#Pega códigos das variáveis relacionadas a saúde
SAUDE <- c(4200101,4200102,4200201,4200202,4200301,4200302,4200303,4200401,
                   4200402,4200403,4200501,4200502,4200601,4200602,4200701,4200801,
                   4200802,4200803,4200804,4200901,4201001,4201002,4201101,4201201,
                   4201202,4201203,4201204,4201205,4201206,4201207,4201208,4201209,
                   4201301,4201302,4201303,4201401,4201402,4201403,4201601,4201602,
                   4201603,4201701,4201702,4201703,4201704,4201705,4201706,4201707,
                   4201801,4201802,4201901,4202001,4202002,4202101,4202102,4202103,
                   4202201,4202301,4202401,4202402,4202403,4202404,4202405,4202501,
                   4202502,4202601,4202602,4202603,4202604,4202605,4202606,4202607,
                   4202608,4202701,4202702,4202703,4202704,4203101,4203102,4203103,
                   4203104,4203201,4203202,4203203,4203204,4203205,4203206,4203207,
                   4203208,4203209,4203210,4203301,4203302,4203303,4203304,4203401,
                   4203501,4203601,4203701,4203801,4203901,4204001,4204101,4204201,
                   4204301,4204302,4204303,4204401,4204501,4204601,4204701,4204702,
                   4204801,4204802,4204901,4205001,4205101,4205201,4205301,4205302,
                   4205401,4205402,4205501,4205601,4205701,4205801,4205901,4205902,
                   4206001,4206101,4206201,4206301,4206401,4206501,4206502,4206601,
                   4206701,4206801,4206901,4207001,4207101,4207102,4207201,4207301,
                   4207401,4207402,4207403,4207404,4207501,4207502,4207601,4207602,
                   4207701,4207801,4207901,4208001,4208101,4208201,4208301,4208401,
                   4208501,4208601,4208701,4208702,4208801,4208901,4209001,4209101,
                   4209201,4209202,4209301,4209401,4209402,4209501,4209601,4209701,
                   4209801,4209901,4210001,4210101,4210201,4210301,4210302,4210401)

#Agrega (soma) despesas com saúde por indivíduo
saude_agregado <- aggregate(DESPESA_INDIVIDUAL$V8000_DEFLA[DESPESA_INDIVIDUAL$V9001 %in% SAUDE],
          by = list(COD_UPA = DESPESA_INDIVIDUAL$COD_UPA[DESPESA_INDIVIDUAL$V9001 %in% SAUDE],
                    NUM_DOM = DESPESA_INDIVIDUAL$NUM_DOM[DESPESA_INDIVIDUAL$V9001 %in% SAUDE],
                    NUM_UC = DESPESA_INDIVIDUAL$NUM_UC[DESPESA_INDIVIDUAL$V9001 %in% SAUDE],
                    COD_INFORMANTE = DESPESA_INDIVIDUAL$COD_INFORMANTE[DESPESA_INDIVIDUAL$V9001 %in% SAUDE]),
          FUN = sum, 
          na.rm = T)


#Pega a tabela base e junta com a saude_agregado,
#mantendo o código da UPA, o número do domicílio, o número da unidade de consumo,
#e o código do informante constantes
base_xhl <- merge(base,
              saude_agregado,
              by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              all.x = T)

#Renomeia a última variável (soma de despesas com saúde calculada acima) 
#como xhl, de "expenditure (x)" e "health (hl)"
names(base_xhl)[length(base_xhl)] <- "xhl"


#GASTOS COM EDUCAÇÃO

#Códigos para variáveis de gastos com educação
EDUCACAO <- c(4900101,4900201,4900301,4900302,4900303,4900401,
                      4900402,4900403,4900501,4900502,4900503,4900601,
                      4900602,4900603,4900801,4900901,4900902,4901001,
                      4901002,4901003,4901101,4901102,4901103,4901201,
                      4901202,4901203,4901901,4902001,4902101,4903501,
                      4904001,4904201,4904401,4904701,4904801,4904901,
                      4905001,4905101,4905601,4905701,4905801,4905901,
                      4906101,4906301,4906401,4907801,4907802,4907901,
                      4907902,4908001,4908002,4908101,4908102,4909001,
                      4909601,4910401,4910601,4910801,4911301,4911401,
                      4911501,4911601,4911701,4911801,4912001,4912101,
                      4912601,4912701,4912801,4912901,4913001,4913101)


#Agrega (soma) todos os gastos com educação por indivíduo
educacao_agregado <- aggregate(
  DESPESA_INDIVIDUAL$V8000_DEFLA[DESPESA_INDIVIDUAL$V9001 %in% EDUCACAO],
  by = list(COD_UPA = DESPESA_INDIVIDUAL$COD_UPA[DESPESA_INDIVIDUAL$V9001 %in% EDUCACAO],
            NUM_DOM = DESPESA_INDIVIDUAL$NUM_DOM[DESPESA_INDIVIDUAL$V9001 %in% EDUCACAO],
            NUM_UC = DESPESA_INDIVIDUAL$NUM_UC[DESPESA_INDIVIDUAL$V9001 %in% EDUCACAO],
            COD_INFORMANTE = DESPESA_INDIVIDUAL$COD_INFORMANTE[DESPESA_INDIVIDUAL$V9001 %in% EDUCACAO]),
  FUN = sum, 
  na.rm = T)


base_xed <- merge(base_xhl,
              educacao_agregado,
              by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              all.x = T)

#Renomeia a última coluna (gastos agregados com educação calculados acima) para xed,
#de "expenditure (x)" e "education (ed)"
names(base_xed)[length(base_xed)] <- "xed"

#GASTOS COM PENSÃO ALIMENTÍCIA

#Agregada (soma) todos os gastos com pensão alimentícia dos indivíduos
pensao_agregado <- aggregate(
  DESPESA_INDIVIDUAL$V8000_DEFLA[DESPESA_INDIVIDUAL$V9001 == 4800901],
  by = list(COD_UPA = DESPESA_INDIVIDUAL$COD_UPA[DESPESA_INDIVIDUAL$V9001 == 4800901],
            NUM_DOM = DESPESA_INDIVIDUAL$NUM_DOM[DESPESA_INDIVIDUAL$V9001 == 4800901],
            NUM_UC = DESPESA_INDIVIDUAL$NUM_UC[DESPESA_INDIVIDUAL$V9001 == 4800901],
            COD_INFORMANTE = DESPESA_INDIVIDUAL$COD_INFORMANTE[DESPESA_INDIVIDUAL$V9001 == 4800901]),
  FUN = sum, 
  na.rm = T)

#Adiciona essa nova variável à base, mantendo código da UPA, número do domicílio,
#número da unidade de consumo e código do informante iguais
base_xmp <- merge(base_xed,
              pensao_agregado,
              by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              all.x = T)

#Renomeia a última coluna (da nova variável) para xmp, de "expenditure (x)",
#"alimony payments (mp)"
names(base_xmp)[length(base_xmp)] <- "xmp"


#GASTOS COM PREVIDÊNCIA PRIVADA

#Códigos para variáveis de gastos com previdência privada
PREVIDENCIA_PRIV <- c(4800601, 4800602, 4800603)

#Agrega (soma) todos os gastos com previdência privada por indivíduo
previdencia_priv_agregado <- aggregate(
  DESPESA_INDIVIDUAL$V8000_DEFLA[DESPESA_INDIVIDUAL$V9001 %in% PREVIDENCIA_PRIV],
  by = list(COD_UPA = DESPESA_INDIVIDUAL$COD_UPA[DESPESA_INDIVIDUAL$V9001 %in% PREVIDENCIA_PRIV],
            NUM_DOM = DESPESA_INDIVIDUAL$NUM_DOM[DESPESA_INDIVIDUAL$V9001 %in% PREVIDENCIA_PRIV],
            NUM_UC = DESPESA_INDIVIDUAL$NUM_UC[DESPESA_INDIVIDUAL$V9001 %in% PREVIDENCIA_PRIV],
            COD_INFORMANTE = DESPESA_INDIVIDUAL$COD_INFORMANTE[DESPESA_INDIVIDUAL$V9001 %in% PREVIDENCIA_PRIV]),
  FUN = sum, 
  na.rm = T)

#Adiciona essa nova variável à base, mantendo código da UPA, número do domicílio,
#número da unidade de consumo e código do informante iguais
base_xpp <- merge(base_xmp,
              previdencia_priv_agregado,
              by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              all.x = T)
#Renomeia a última coluna (da nova variável) para xpp, de "expenditure (x)",
#"private pensions (pp)"
names(base_xpp)[length(base_xpp)] <- "xpp"


base_xpp <- base_xpp[, !duplicated(colnames(base_xpp))]

#FREQUENTA ESCOLA 

base_xpp <- base_xpp %>% rename(dec01 = V0415) %>% 
  mutate(dec01 = ifelse(dec01 == 1,
                        yes = 1,
                        no = 2))

#SUB-CHEFE

#O sub-chefe de cada domicílio será identificado como o indivíduo que tiver a maior
#renda e não for chefe; ou, quando todas as rendas forem N/A, o que tiver maior idperson


#Cria a variável binária subhead
base_subhead <- base_xpp %>% 
  filter(idperson != idhead) %>% 
  group_by(idhh) %>% 
  mutate(subhead = ifelse(
    yem == max(yem) | idperson == max(idperson),
    yes = 1,
    no = 0
  )) %>% 
  ungroup(idhh)

#Junta com a base anterior
base_subhead <- left_join(base_xpp,
                          base_subhead)

#Todos que não tiveram nem 1 nem 0 (domicílio não tem subhead) recebem 0
base_subhead$subhead <- base_subhead$subhead %>% 
  replace_na(0)

#Cria a variável idsubhead, sendo para cada domicílio o idperson de seu subhead
base_subhead <- base_subhead %>% 
  group_by(idhh) %>% 
  mutate(idsubhead = ifelse(subhead == 1,
                            yes = idperson,
                            no = 0)) %>% 
  mutate(idsubhead = max(idsubhead))

#EMPREGO

#Pega um vetor e checa se 1 está nele
exists_one <- function(x) {
  return(1 %in% x)
}

#Variável de carteira assinada (1) ou não (2)
carteira <- aggregate(
  RENDIMENTO_TRABALHO$V5304,
                      by = list(COD_UPA = RENDIMENTO_TRABALHO$COD_UPA,
                                NUM_DOM = RENDIMENTO_TRABALHO$NUM_DOM,
                                NUM_UC = RENDIMENTO_TRABALHO$NUM_UC,
                                COD_INFORMANTE = RENDIMENTO_TRABALHO$COD_INFORMANTE),
                      FUN = exists_one) 


base_lem <- merge(base_subhead,
                  carteira,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T)

names(base_lem)[length(base_lem)] <- "lem"

base_lem <- base_lem %>% 
  mutate(lem = ifelse(lem == 1,
                      yes = 1,
                      no = 2))

#VARIÁVEIS PARA CALCULAR IRPF SOBRE INVESTIMENTOS

#Códigos para títulos do governo
APLICACAO_TITULOS <- c(5600201, 5600401)
RESGATE_TITULOS <- c(5700201, 5700401)

aplicacao_agregado <- aggregate(
  OUTROS_RENDIMENTOS$V8500_DEFLA[OUTROS_RENDIMENTOS$V9001 %in% APLICACAO_TITULOS],
  by = list(COD_UPA = OUTROS_RENDIMENTOS$COD_UPA[OUTROS_RENDIMENTOS$V9001 %in% APLICACAO_TITULOS],
            NUM_DOM = OUTROS_RENDIMENTOS$NUM_DOM[OUTROS_RENDIMENTOS$V9001 %in% APLICACAO_TITULOS],
            NUM_UC = OUTROS_RENDIMENTOS$NUM_UC[OUTROS_RENDIMENTOS$V9001 %in% APLICACAO_TITULOS],
            COD_INFORMANTE = OUTROS_RENDIMENTOS$COD_INFORMANTE[OUTROS_RENDIMENTOS$V9001 %in% APLICACAO_TITULOS]),
  FUN = sum, 
  na.rm = T)

base_apps <- merge(base_lem,
                   aplicacao_agregado,
                   by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                   by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                   all.x = T)

names(base_apps)[length(base_apps)] <- "apps_invest"

base_apps$apps_invest[is.na(base_apps$apps_invest)] <- 0

resgate_agregado <- aggregate(
  OUTROS_RENDIMENTOS$V8500_DEFLA[OUTROS_RENDIMENTOS$V9001 %in% RESGATE_TITULOS],
  by = list(COD_UPA = OUTROS_RENDIMENTOS$COD_UPA[OUTROS_RENDIMENTOS$V9001 %in% RESGATE_TITULOS],
            NUM_DOM = OUTROS_RENDIMENTOS$NUM_DOM[OUTROS_RENDIMENTOS$V9001 %in% RESGATE_TITULOS],
            NUM_UC = OUTROS_RENDIMENTOS$NUM_UC[OUTROS_RENDIMENTOS$V9001 %in% RESGATE_TITULOS],
            COD_INFORMANTE = OUTROS_RENDIMENTOS$COD_INFORMANTE[OUTROS_RENDIMENTOS$V9001 %in% RESGATE_TITULOS]),
  FUN = sum, 
  na.rm = T)

base_resg <- merge(base_apps,
                   resgate_agregado,
                   by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                   by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                   all.x = T)

names(base_resg)[length(base_resg)] <- "resg_invest"
base_resg$resg_invest[is.na(base_resg$resg_invest)] <- 0

#Cria variável "ybd" de renda (líquida) advinda dos títulos de governo,
#de "income (y)" e "bonds (bd)"
base_resg$ybd <- base_resg$resg_invest - base_resg$apps_invest

#Ações
#Código de venda = 5700301
#Código de compra = 5600301
acoes_venda <- OUTROS_RENDIMENTOS[OUTROS_RENDIMENTOS$V9001 == 5700301, 
                        names(OUTROS_RENDIMENTOS) %in% 
                          c('COD_UPA', 'NUM_DOM', 'NUM_UC', 'COD_INFORMANTE', 'V8500_DEFLA')]

base_venda <- merge(base_resg,
              acoes_venda,
              by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              all.x = TRUE)

#Renome última variável para "ash", "asset (a)" e "shares (sh)"
names(base_venda)[length(base_venda)] <- "venda"

base_venda$venda[is.na(base_venda$venda)] <- 0

acoes_compra <- OUTROS_RENDIMENTOS[OUTROS_RENDIMENTOS$V9001 == 5600301, 
                   names(OUTROS_RENDIMENTOS) %in% 
                     c('COD_UPA', 'NUM_DOM', 'NUM_UC', 'COD_INFORMANTE', 'V8500_DEFLA')]

base_compra <- merge(base_venda,
              acoes_compra,
              by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              all.x = TRUE)

names(base_compra)[length(base_compra)] <- "compra"
base_compra$compra[is.na(base_compra$compra)] <- 0

#Cria a variável "ysh" para renda advinda da venda (líquida) de ações, de
#"income (y)" e "shares (sh)"
base_compra$ysh <- base_compra$venda - base_compra$compra

# SEGURO E AUXÍLIO DESEMPREGO

SEGURO_AUXILIO <- c(5501701, 5501702)

seguro_auxilio_agregado <- aggregate(
  OUTROS_RENDIMENTOS$V8500_DEFLA[OUTROS_RENDIMENTOS$V9001 %in% SEGURO_AUXILIO],
  by = list(COD_UPA = OUTROS_RENDIMENTOS$COD_UPA[OUTROS_RENDIMENTOS$V9001 %in% SEGURO_AUXILIO],
            NUM_DOM = OUTROS_RENDIMENTOS$NUM_DOM[OUTROS_RENDIMENTOS$V9001 %in% SEGURO_AUXILIO],
            NUM_UC = OUTROS_RENDIMENTOS$NUM_UC[OUTROS_RENDIMENTOS$V9001 %in% SEGURO_AUXILIO],
            COD_INFORMANTE = OUTROS_RENDIMENTOS$COD_INFORMANTE[OUTROS_RENDIMENTOS$V9001 %in% SEGURO_AUXILIO]),
  FUN = sum, 
  na.rm = T)

base_bun <- merge(base_compra,
                  seguro_auxilio_agregado,
                  by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
                  all.x = T)

names(base_bun)[length(base_bun)] <- "bun"

base_bun <- base_bun %>% 
  group_by(idhh) %>% 
  mutate(yhh = sum(as.numeric(yse), 
                   as.numeric(yem), 
                   as.numeric(yiy)),
         yhh00 = mean(as.numeric(yse)) + 
           mean(as.numeric(yem)) + 
           mean(as.numeric(yiy)))

#BENEFÍCIO DE PRESTAÇÃO CONTINUADA

#Agrega todos os valores por indivíduo (código do BPC = 5400201)
bpc_agregado <- aggregate(
  OUTROS_RENDIMENTOS$V8500_DEFLA[OUTROS_RENDIMENTOS$V9001 == 5400201],
  by = list(COD_UPA = OUTROS_RENDIMENTOS$COD_UPA[OUTROS_RENDIMENTOS$V9001 == 5400201],
            NUM_DOM = OUTROS_RENDIMENTOS$NUM_DOM[OUTROS_RENDIMENTOS$V9001 == 5400201],
            NUM_UC = OUTROS_RENDIMENTOS$NUM_UC[OUTROS_RENDIMENTOS$V9001 == 5400201],
            COD_INFORMANTE = OUTROS_RENDIMENTOS$COD_INFORMANTE[OUTROS_RENDIMENTOS$V9001 == 5400201]),
  FUN = sum, 
  na.rm = T)

base_poa01 <- merge(base_bun,
              bpc_agregado,
              by.x = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              by.y = c("COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE"),
              all.x = T)

names(base_poa01)[length(base_poa01)] <- "poa01"

#AJUSTES FINAIS

#Pegar a última base montada
base <- base_poa01

#Salva tudo como string
base <- base %>% 
  mutate(across(everything(), as.character))

#Substitui NAs por -1, como sugerem as convenções do Euromod
base[is.na(base)] <- "-1"

base_final_nao_obrigatorias <- base %>% 
  select(idhh, idperson, idfather, idmother, idpartner, idhead, idsubhead, 
         dct, dwt, dag, dgn, dec, dms, les, yem, lem, lhw, loc, yse, yiy, ybd, ysh,
         ddi, poa, poa01, bun, xhl, xed, xmp, xpp, yhh, yhh00) %>% 
  arrange(as.numeric(idhh))

#Salvar arquivos em .txt separados por tab
write.table(base_final_nao_obrigatorias, file='pof euromod 2.txt', quote=FALSE, sep='\t', row.names=FALSE)

#Salva uma versão com todas as variáveis, pra adicionar mais no futuro
write.table(base, file='pof euromod raw 2.txt', quote=FALSE, sep='\t', row.names=FALSE)






