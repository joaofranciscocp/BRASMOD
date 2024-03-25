# Pacotes ----------------------------------------------------------------------

#Tidyverse and dplyr
install.packages("tidyverse")
library(tidyverse)

install.packages("haven")
library(haven)

install.packages("dplyr")
library(dplyr)

# Quantil ponderado por peso
install.packages("modi")
library(modi)

# Gini com pesos
install.packages("acid")
library(acid)

#Abrindo output data - Brasmod -------------------------------------------------

#Setting work directory
setwd("C:\\Users\\USER\\Documents\\GitHub\\BRASMOD\\Output")

#Checking whether file for individuals exists in work directory
file.exists("bra_2018_std.txt")

#Checking whether file for households exists in work directory
file.exists("bra_2018_std_hh.txt")

#Reading file for individuals
brasmod_ind_2018 <- read.delim("bra_2018_std.txt")

#Reading for households
brasmod_hh_2018 <- read.delim("bra_2018_std_hh.txt")

# Ordenar observacoes conforme renda--------------------------------------------
brasmod_ind_2018 <- brasmod_ind_2018[order(brasmod_ind_2018$ils_dispy), ]

brasmod_hh_2018 <- brasmod_hh_2018[order(brasmod_hh_2018$ils_dispy), ]

#Análise------------------------------------------------------------------------

# Gerar lista com minimo e maximo de cada decil + 1%----------------------------

class(brasmod_hh_2018$ils_dispy)

class(brasmod_hh_2018$dwt)

brasmod_hh_2018$ils_dispy <- gsub(",", ".", brasmod_hh_2018$ils_dispy)

brasmod_hh_2018$dwt <- gsub(",", ".", brasmod_hh_2018$dwt)

brasmod_hh_2018$dwt <- as.numeric(brasmod_hh_2018$dwt)
brasmod_hh_2018$ils_dispy <- as.numeric(brasmod_hh_2018$ils_dispy)

decis <- lapply(lapply(as.list(c(paste0(sapply(seq(from = 0, to = .9, by = .1), 
                                               function(quantil) weighted.quantile(brasmod_hh_2018$ils_dispy,
                                                                                   brasmod_hh_2018$dwt,
                                                                                   prob = quantil)),
                                        "-",
                                        c(sapply(seq(0.1,.9,.1), 
                                                 function(quantil) weighted.quantile(brasmod_hh_2018$ils_dispy,
                                                                                     brasmod_hh_2018$dwt,
                                                                                     prob = quantil)),
                                          max(brasmod_hh_2018$ils_dispy))))),
                       function(decil) strsplit(decil, "-")), 
                function(elemento) as.numeric(elemento[[1]]))


# Nomear os decis
names(decis) <- paste0(seq(0,.9,.1), "-", seq(.1,1,.1))


### Apropriacao da renda--------------------------------------------------------

apropriacao <- data.frame()

for(dec in decis) {
  
  apropriacao <- rbind(apropriacao,
                       data.frame(Decil = paste(dec, collapse = "0"),
                                  Apro = sum(brasmod_hh_2018$ils_dispy[brasmod_hh_2018$ils_dispy >= dec[1] &
                                                                   brasmod_hh_2018$ils_dispy <= dec[2]]*
                                               brasmod_hh_2018$dwt[brasmod_hh_2018$ils_dispy >= dec[1] &
                                                                     brasmod_hh_2018$ils_dispy <= dec[2]])/
                                    sum(brasmod_hh_2018$dwt*brasmod_hh_2018$ils_dispy)))
  
}

apropriacao$Decil <- names(decis)

#Gráfico
barplot(apropriacao$Apro, names.arg = apropriacao$Decil, xlab = "Decil", ylab = "Income Appropriation", col = "blue", main = "Income Appropriation by Deciles")


### Rendimento médio por decil--------------------------------------------------

rendimento.med <- data.frame()

for(dec in decis) {
  
  rendimento.med <- rbind(rendimento.med,
                          data.frame(Decil = paste(dec, collapse = "-"),
                                     RendMed = weighted.mean(x = brasmod_hh_2018$ils_dispy[brasmod_hh_2018$ils_dispy >= dec[1] &
                                                                                         brasmod_hh_2018$ils_dispy <= dec[2]],
                                                             w = brasmod_hh_2018$dwt[brasmod_hh_2018$ils_dispy >= dec[1] &
                                                                                     brasmod_hh_2018$ils_dispy <= dec[2]], na.rm = T)))
  
}

# Atributir o nome
rendimento.med$Decil <- names(decis)

#Gráfico
barplot(rendimento.med$RendMed, names.arg = rendimento.med$Decil, xlab = "Decil", ylab = "Average Income", col = "red", main = "Average Income by Deciles")


# Gerar lista com minimo e maximo dos quantis 90%, 99% e 99,9%------------------
decis.topo <- lapply(lapply(as.list(c(paste0(sapply(c(.9, .99, .999), 
                                                    function(quantil) weighted.quantile(x = brasmod_hh_2018$ils_dispy,
                                                                                        w = brasmod_hh_2018$dwt,
                                                                                        prob = quantil)),
                                             "-",
                                             rep(max(brasmod_hh_2018$ils_dispy),3)))),
                            function(decil) strsplit(decil, "-")), 
                     function(elemento) as.numeric(elemento[[1]]))

names(decis.topo) <- c("10%", "1%", "0,1%")
