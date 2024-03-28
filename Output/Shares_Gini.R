### Packages--------------------------------------------------------------------

## tidyverse--------------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)

## haven------------------------------------------------------------------------
install.packages("haven")
library(haven)

## data.table------------------------------------------------------------------- 
install.packages("data.table")
library(data.table)

## dplyr------------------------------------------------------------------------
install.packages("dplyr")
library(dplyr)

## modi (Quantil ponderado por peso)--------------------------------------------
install.packages("modi")
library(modi)

## acid (Gini com pesos)--------------------------------------------------------
install.packages("acid")
library(acid)

### Opening output data - Brasmod-----------------------------------------------

## Setting work directory-------------------------------------------------------
setwd("C:\\Users\\lauro\\Documents\\GitHub\\BRASMOD\\Output")

## Checking whether file for individuals exists in work directory---------------
file.exists("bra_2018_std.txt")

## Checking whether file for households exists in work directory----------------
file.exists("bra_2018_std_hh.txt")

## Reading file for individuals-------------------------------------------------
# bra_2018_ind <- read.delim("bra_2018_std.txt")
bra_2018_ind <- fread("bra_2018_std.txt", sep = "\t", dec = ",")

## Reading file for households--------------------------------------------------
# bra_2018_hh <- read.delim("bra_2018_std_hh.txt")
bra_2018_hh <- fread("bra_2018_std_hh.txt", sep = "\t", dec = ",")

### Changing variables of interest into numeric values--------------------------

## Checking class of variable of interest---------------------------------------
class(bra_2018_hh$ils_dispy)
class(bra_2018_hh$dwt)

## Converting comma to dot------------------------------------------------------
#bra_2018_hh$ils_dispy <- gsub(",", ".", bra_2018_hh$ils_dispy)
#bra_2018_hh$dwt <- gsub(",", ".", bra_2018_hh$dwt)

## Telling R data is numeric----------------------------------------------------
#bra_2018_hh$ils_dispy <- as.numeric(bra_2018_hh$ils_dispy)
#bra_2018_hh$dwt <- as.numeric(bra_2018_hh$dwt)

## Checking whether procedure was successful------------------------------------
#class(bra_2018_hh$ils_dispy)
#class(bra_2018_hh$dwt)

### Ordering observations for variable of interest (ils_dispy)------------------
bra_2018_ind <- bra_2018_ind[order(bra_2018_ind$ils_dispy), ]
bra_2018_hh <- bra_2018_hh[order(bra_2018_hh$ils_dispy), ]

### Analysis--------------------------------------------------------------------

## Gini-------------------------------------------------------------------------
weighted.gini(bra_2018_hh$ils_dispy, bra_2018_hh$dwt)
weighted.gini(bra_2018_ind$ils_dispy, bra_2018_ind$dwt)

## Gini index weighted by race--------------------------------------------------

# Filtering individual Brasmod data by race
race_1_Gini <- bra_2018_ind %>% 
  filter(dra==1) #dra == 1, white people

race_2_4_Gini <- bra_2018_ind %>% 
  filter(dra==2 | dra==4) #dra == 2, black people; dra == 4, people of colour.

#Calculating Gini in race group of interest
weighted.gini(race_1_Gini$ils_dispy, race_1_Gini$dwt)
weighted.gini(race_2_4_Gini$ils_dispy, race_2_4_Gini$dwt)

#Using household data
#base2 <- bra_2018_hh %>% 
  #mutate(ils_dispy_new = ifelse(ils_dispy < 0,
                                #0,
                                #ils_dispy))%>% 
  #group_by(idhh) %>% 
  #summarise(ils_dispy_new = sum(ils_dispy_new),
            #dwt = sum(dwt))  

#weighted.gini(base2$ils_dispy_new, base2$dwt)

## Gini index weighted by gender------------------------------------------------

gender_1_Gini <- bra_2018_ind %>% 
  filter(dgn==1) #dgn == 1, man

gender_2_Gini <- bra_2018_ind %>% 
  filter(dgn==2) #dgn == 2, woman

#Calculating Gini in gender group of interest
weighted.gini(gender_1_Gini$ils_dispy, gender_1_Gini$dwt)
weighted.gini(gender_2_Gini$ils_dispy, gender_2_Gini$dwt)

#Using household data
#base2 <- bra_2018_hh %>% 
#mutate(ils_dispy_new = ifelse(ils_dispy < 0,
#0,
#ils_dispy))%>% 
#group_by(idhh) %>% 
#summarise(ils_dispy_new = sum(ils_dispy_new),
#dwt = sum(dwt))  

#weighted.gini(base2$ils_dispy_new, base2$dwt)

## Gini index weighted by race and gender---------------------------------------

# Filtering individual Brasmod data by gender
gen_1_race_1_Gini <- bra_2018_ind %>% 
  filter(dgn==1 & dra==1) #dgn ==1, dra == 1, white men

gen_1_race_2_4_Gini <- bra_2018_ind %>% 
  filter(dgn==1 & dra==2 | dra==4) #dgn ==1, dra==2, dra==4 black men

gen_2_race_1_Gini <- bra_2018_ind %>% 
  filter(dgn==2 & dra==1) #dgn==2, dra==1, white women

gen_2_race_2_4_Gini <- bra_2018_ind %>% 
  filter(dgn==2 & dra==2 | dra==4) #dgn == 2, woman

#Calculating Gini in gender/race groups of interest
weighted.gini(gen_1_race_1_Gini$ils_dispy, gen_1_race_1_Gini$dwt) # white men
weighted.gini(gen_1_race_2_4_Gini$ils_dispy, gen_1_race_2_4_Gini$dwt) # black men
weighted.gini(gen_2_race_1_Gini$ils_dispy, gen_2_race_1_Gini$dwt) # white men
weighted.gini(gen_2_race_2_4_Gini$ils_dispy, gen_2_race_2_4_Gini$dwt)


## Income shares (general)------------------------------------------------------

# Generating list with min and max of each decile + 1%

decis <- lapply(lapply(as.list(c(paste0(sapply(seq(from = 0, to = .9, by = .1), 
                                               function(quantil) weighted.quantile(bra_2018_hh$ils_dispy,
                                                                                   bra_2018_hh$dwt,
                                                                                   prob = quantil)),
                                        "-",
                                        c(sapply(seq(0.1,.9,.1), 
                                                 function(quantil) weighted.quantile(bra_2018_hh$ils_dispy,
                                                                                     bra_2018_hh$dwt,
                                                                                     prob = quantil)),
                                          max(bra_2018_hh$ils_dispy))))),
                       function(decil) strsplit(decil, "-")), 
                function(elemento) as.numeric(elemento[[1]]))


# Naming deciles
names(decis) <- paste0(seq(0,.9,.1), "-", seq(.1,1,.1))


# Calculating income appropriation

apropriacao <- data.frame()

for(dec in decis) {
  
  apropriacao <- rbind(apropriacao,
                       data.frame(Decil = paste(dec, collapse = "0"),
                                  Apro = sum(bra_2018_hh$ils_dispy[bra_2018_hh$ils_dispy >= dec[1] &
                                                                   bra_2018_hh$ils_dispy <= dec[2]]*
                                               bra_2018_hh$dwt[bra_2018_hh$ils_dispy >= dec[1] &
                                                                     bra_2018_hh$ils_dispy <= dec[2]])/
                                    sum(bra_2018_hh$dwt*bra_2018_hh$ils_dispy)))
  
}

apropriacao$Decil <- names(decis)

# Plotting graph
barplot(apropriacao$Apro, names.arg = apropriacao$Decil, xlab = "Decil", ylab = "Income Appropriation", col = "blue", main = "Income Appropriation by Deciles")


## Average income per decile (general)------------------------------------------

rendimento.med <- data.frame()

for(dec in decis) {
  
  rendimento.med <- rbind(rendimento.med,
                          data.frame(Decil = paste(dec, collapse = "-"),
                                     RendMed = weighted.mean(x = bra_2018_hh$ils_dispy[bra_2018_hh$ils_dispy >= dec[1] &
                                                                                         bra_2018_hh$ils_dispy <= dec[2]],
                                                             w = bra_2018_hh$dwt[bra_2018_hh$ils_dispy >= dec[1] &
                                                                                     bra_2018_hh$ils_dispy <= dec[2]], na.rm = T)))
  
}

# Naming
rendimento.med$Decil <- names(decis)

# Plotting graph
barplot(rendimento.med$RendMed, names.arg = rendimento.med$Decil, xlab = "Decil", ylab = "Average Income", col = "red", main = "Average Income by Deciles")


# Generating list with min and max of quantiles 90%, 99% e 99,9%----------------
decis.topo <- lapply(lapply(as.list(c(paste0(sapply(c(.9, .99, .999), 
                                                    function(quantil) weighted.quantile(x = bra_2018_hh$ils_dispy,
                                                                                        w = bra_2018_hh$dwt,
                                                                                        prob = quantil)),
                                             "-",
                                             rep(max(bra_2018_hh$ils_dispy),3)))),
                            function(decil) strsplit(decil, "-")), 
                     function(elemento) as.numeric(elemento[[1]]))

# Naming quantiles
names(decis.topo) <- c("10%", "1%", "0,1%")



################################################################################
################################################################################
################################################################################
################################################################################
## Average income per decile (by groups of interest)----------------------------

# Vector to identify variable "dgn"
genero <- c("Homem" = 1, "Mulher" = 2)

# Vector to identify variable "dra"
raca <- list("B" = c(1), "N" = c(2,4))


apropriacao_gen_race <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis) {
      
      apropriacao_gen_race <- rbind(apropriacao_gen_race,
                           data.frame(Genero = gen,
                                      Raca = paste(rc, collapse = "-"),
                                      Decil = paste(dec, collapse = "-"),
                                      Apro = sum(bra_2018_ind$ils_dispy[bra_2018_ind$ils_dispy >= dec[1] &
                                                                          bra_2018_ind$ils_dispy <= dec[2] &
                                                                          bra_2018_ind$dgn == gen & 
                                                                         (bra_2018_ind$dra %in% rc)]*
                                                   bra_2018_ind$dwt[bra_2018_ind$ils_dispy >= dec[1] &
                                                                      bra_2018_ind$ils_dispy <= dec[2] &
                                                                      bra_2018_ind$dgn == gen & 
                                                                       (bra_2018_ind$dra %in% rc)])/
                                        sum(bra_2018_ind$dwt*bra_2018_ind$ils_dispy)))
      
    }
    
  }
  
}

# Naming columns
apropriacao_gen_race$Genero <- c(rep("H",nrow(apropriacao_gen_race)/2), rep("M",nrow(apropriacao_gen_race)/2))
apropriacao_gen_race$Raca <- c(rep(c(rep("B",(nrow(apropriacao_gen_race)/4)), rep("N",(nrow(apropriacao_gen_race)/4))),2))
apropriacao_gen_race$Decil <- names(decis)
apropriacao_gen_race$Grupo <- paste0(apropriacao_gen_race$Genero,"-", apropriacao_gen_race$Raca)


### Rendimento medio por decil por grupo ###

rendimento_gen_race <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis) {
      
      rendimento_gen_race <- rbind(rendimento_gen_race,
                          data.frame(Genero = gen,
                                     Raca = paste(rc, collapse = "-"),
                                     Decil = paste(dec, collapse = "-"),
                                     RendMed = weighted.mean(x = bra_2018_ind$ils_dispy[bra_2018_ind$ils_dispy >= dec[1] &
                                                                                         bra_2018_ind$ils_dispy <= dec[2] &
                                                                                          bra_2018_ind$dgn == gen & 
                                                                                         (bra_2018_ind$dra %in% rc)],
                                                             w = bra_2018_ind$dwt[bra_2018_ind$ils_dispy >= dec[1] &
                                                                                    bra_2018_ind$ils_dispy <= dec[2] &
                                                                                    bra_2018_ind$dgn == gen & 
                                                                                     (bra_2018_ind$dra %in% rc)], na.rm = T)))
      
      
    }
    
  }
  
}

# Atributir o nome
rendimento_gen_race$Genero <- c(rep("H",20), rep("M",20))
rendimento_gen_race$Raca <- c(rep(c(rep("B",10), rep("N",10)),2))
rendimento_gen_race$Decil <- names(decis)
rendimento_gen_race$Grupo <- paste0(rendimento_gen_race$Genero,"-", rendimento_gen_race$Raca)


### Participacao grupo por decil ###

participacao_gen_race <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis) {
      
      participacao_gen_race <- rbind(participacao_gen_race,
                            data.frame(Genero = gen,
                                       Raca = paste(rc, collapse = "-"),
                                       Decil = paste(dec, collapse = "-"),
                                       Part = sum(bra_2018_ind$dwt[bra_2018_ind$ils_dispy >= dec[1] &
                                                                     bra_2018_ind$ils_dispy <= dec[2] &
                                                                     bra_2018_ind$dgn == gen & 
                                                                      (bra_2018_ind$dra %in% rc)])/sum(bra_2018_ind$dwt[bra_2018_ind$ils_dispy >= dec[1] &
                                                                                                                          bra_2018_ind$ils_dispy <= dec[2]])))
      
    }
    
  }
  
}

# Atributir o nome
participacao_gen_race$Genero <- c(rep("H",20), rep("M",20))
participacao_gen_race$Raca <- c(rep(c(rep("B",10), rep("N",10)),2))
participacao_gen_race$Decil <- names(decis)
participacao_gen_race$Grupo <- paste0(participacao_gen_race$Genero,"-", participacao_gen_race$Raca)

# Salvar o grafico a ser feito a seguir
png("CompDemoDecil.png", width = 4800, height = 3200, res = 300)

# Ajuste das margens para caber a legenda
par(mar=c(5, 5, 5, 11), xpd=TRUE)
par(mfrow = c(1,1))

info.grafico <- matrix(participacao_gen_race$Part[c(1:10,21:30,11:20,31:40)], nrow = 4, ncol = 10, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       1:10)) * 100

info.grafico <- cbind("Pop." = participacao.med$Part[c(1,3,2,4)]*100, info.grafico)

barplot(info.grafico,
        col = c("#45ff66","#EB52FF","#3366FF","#FEFF41"),
        border = "white",
        ylim = c(0,100),
        space = 0.04,
        font.axis = 2, 
        cex.axis = 1.5, 
        cex.names = 1.5,
        cex.lab=1.5,
        xlab = "Decil e total população",
        ylab = "Composição demográfica dos decis de renda (em %)")

legend("right", 
       inset=c(-.09, 0), 
       cex = 1.5,
       legend = c("Mulheres negras", "Homens negros", "Mulheres brancas", "Homens brancos"),
       fill = rev(c("#45ff66","#EB52FF","#3366FF","#FEFF41")), 
       bty = "n")

dev.off()



# Composiçao demográfica média população

participacao.med <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    participacao.med <- rbind(participacao.med,
                              data.frame(Genero = gen,
                                         Raca = paste(rc, collapse = "-"),
                                         Part = sum(bra_2018_ind$dwt[bra_2018_ind$dgn == gen & 
                                                                        (bra_2018_ind$dra %in% rc)])/sum(bra_2018_ind$dwt)))
    
  }
  
}

### Composicao demografica do topo ###

### Participacao grupo por decil ###

participacao.topo_gen_race <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis.topo) {
      
      participacao.topo_gen_race <- rbind(participacao.topo_gen_race,
                                 data.frame(Genero = gen,
                                            Raca = paste(rc, collapse = "-"),
                                            Decil = paste(dec, collapse = "-"),
                                            Part = sum(bra_2018_ind$dwt[bra_2018_ind$ils_dispy >= dec[1] &
                                                                          bra_2018_ind$ils_dispy <= dec[2] &
                                                                          bra_2018_ind$dgn == gen & 
                                                                           (bra_2018_ind$dra %in% rc)])/
                                              sum(bra_2018_ind$dwt[bra_2018_ind$ils_dispy >= dec[1] &
                                                                     bra_2018_ind$ils_dispy <= dec[2]])))
      
      
    }
    
  }
  
}

# Atributir o nome
participacao.topo_gen_race$Genero <- c(rep("H",6), rep("M",6))
participacao.topo_gen_race$Raca <- c(rep(c(rep("B",3), rep("N",3)),2))
participacao.topo_gen_race$Decil <- names(decis.topo)
participacao.topo_gen_race$Grupo <- paste0(participacao.topo_gen_race$Genero,"-", participacao.topo_gen_race$Raca)

### Participacao de cada grupo dentro dos quantis do topo ###

png("CompDemoTopo.png", width = 4800, height = 3200, res = 300)

par(mar=c(5, 5, 5, 11), xpd=TRUE)
par(mfrow = c(1,1))

info.grafico <- matrix(participacao.topo_gen_race$Part[c(1:3,7:9,4:6,10:12)], nrow = 4, ncol = 3, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       c("10%", "1%", "0.1%"))) * 100

info.grafico <- cbind("Pop." = participacao.med$Part[c(1,3,2,4)]*100, info.grafico)

barplot(info.grafico,
        col = c("#45ff66","#EB52FF","#3366FF","#FEFF41"),
        border = "white",
        ylim = c(0,100),
        space = 0.1,
        font.axis = 2,
        cex.axis = 1.5, 
        cex.names = 1.5,
        cex.lab=1.5,
        xlab = "Quantis",
        ylab = "Composição demográfica dos quantis de renda (em %)")

legend("right", 
       inset=c(-0.8, 0),
       cex = 1.5,
       legend = c("Mulheres negras", "Homens negros", "Mulheres brancas", "Homens brancos"),
       fill = rev(c("#45ff66","#EB52FF","#3366FF","#FEFF41")), 
       bty = "n")

dev.off()













###########################################################################

# Generating list with min and max of quantiles 90%, 99% e 99,9%
decis.topo <- lapply(lapply(as.list(c(paste0(sapply(c(.9, .99, .999), 
                                                    function(quantil) weighted.quantile(x = bra_2018_hh$ils_dispy,
                                                                                        w = bra_2018_hh$dwt,
                                                                                        prob = quantil)),
                                             "-",
                                             rep(max(bra_2018_hh$ils_dispy),3)))),
                            function(decil) strsplit(decil, "-")), 
                     function(elemento) as.numeric(elemento[[1]]))

# Naming quantiles
names(decis.topo) <- c("10%", "1%", "0,1%")


### Participacao do 1% do topo no total da renda do grupo ###

# Gerar lista com minimo e maximo dos quantis 90%, 99% e 99,9% por grupo 
decis.topo.grupo <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in c(.9, .99, .999)) {
      
      decis.topo.grupo <- rbind(decis.topo.grupo,
                                data.frame(Genero = gen,
                                           Raca = paste(rc, collapse = "-"),
                                           Decil = dec,
                                           Renda = weighted.quantile(x = bra_2018_ind$ils_dispy[bra_2018_ind$dgn == gen & (bra_2018_ind$dra %in% rc)],
                                                                     w = bra_2018_ind$dwt[bra_2018_ind$dgn == gen & (bra_2018_ind$dra %in% rc)],
                                                                     prob = dec)))
      
    }
    
  }
  
}

# Atributir o nome
decis.topo.grupo$Genero <- c(rep("H",6), rep("M",6))
decis.topo.grupo$Raca <- c(rep(c(rep("B",3), rep("N",3)),2))
decis.topo.grupo$Decil <- names(decis.topo)
decis.topo.grupo$Grupo <- paste0(decis.topo.grupo$Genero,"-", decis.topo.grupo$Raca)

### Apropriacao da renda de cada grupo demográfico por decil ###

png("ApropGrupoDecil.png", width = 4800, height = 3200, res = 300)

par(mar=c(5, 5, 5, 5), xpd=TRUE)
par(mfrow = c(2,2))

info.grafico <- matrix(apropriacao.grupo$Apro[c(1:10,21:30,11:20,31:40)], nrow = 4, ncol = 10, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       1:10)) * 100

cores <- c("#45ff66","#EB52FF","#3366FF","#FEFF41")
legenda <- c("Homens brancos", "Mulheres brancas", "Homens negros", "Mulheres negras")

for(i in 1:4) {
  
  barplot(info.grafico[i,],
          col = cores[i],
          border = "white",
          ylim = c(0,65),
          beside = T,
          font.axis = 2,
          cex.axis = 1.5, 
          cex.names = 1.5,
          cex.lab=1.5,
          xlab = "Decis",
          ylab = "%")
  
  text(x = seq(.7, 11.5, length.out = 10),
       y = info.grafico[i,]+3,
       labels = round(info.grafico[i,], digits = 0), font = 2)
  
  text(x = 1.9,
       y = 40,
       labels = legenda[i], font = 2, cex = 1.5)
  
}

dev.off()

# Calcular a concentracao dentro do grupo

criterios <- list("HB" = list("Genero" = genero[1],
                              "Raca" = raca[[1]],
                              "Quantis" = decis.topo.grupo$Renda[1:3]),
                  "HN" = list("Genero" = genero[1],
                              "Raca" = raca[[2]],
                              "Quantis" = decis.topo.grupo$Renda[4:6]),
                  "MB" = list("Genero" = genero[2],
                              "Raca" = raca[[1]],
                              "Quantis" = decis.topo.grupo$Renda[7:9]),
                  "MN" = list("Genero" = genero[2],
                              "Raca" = raca[[2]],
                              "Quantis" = decis.topo.grupo$Renda[10:12]))

apropriacao.topo.grupo <- data.frame()

for (grupo in criterios) {
  
  for (decil in grupo$Quantis) {
    
    apropriacao.topo.grupo <- rbind(apropriacao.topo.grupo,
                                    data.frame(Genero = grupo$Genero,
                                               Raca = paste0(grupo$Raca, collapse = "-"),
                                               Quantil = decil,
                                               Part = sum(bra_2018_ind$ils_dispy[bra_2018_ind$ils_dispy >= decil &
                                                                                  bra_2018_ind$ils_dispy <= max(bra_2018_ind$ils_dispy) &
                                                                                  bra_2018_ind$dgn == grupo$Genero & 
                                                                                  (bra_2018_ind$dra %in% grupo$Raca)]*
                                                            bra_2018_ind$dwt[bra_2018_ind$ils_dispy >= decil &
                                                                                bra_2018_ind$ils_dispy <= max(bra_2018_ind$ils_dispy) &
                                                                                bra_2018_ind$dgn == grupo$Genero & 
                                                                                (bra_2018_ind$dra %in% grupo$Raca)])/
                                                 sum(bra_2018_ind$ils_dispy[bra_2018_ind$dgn == grupo$Genero & 
                                                                             (bra_2018_ind$dra %in% grupo$Raca)]*
                                                       bra_2018_ind$dwt[bra_2018_ind$dgn == grupo$Genero & 
                                                                           (bra_2018_ind$dra %in% grupo$Raca)])))
    
  }
  
}

# Atributir o nome
apropriacao.topo.grupo$Genero <- c(rep("H",6), rep("M",6))
apropriacao.topo.grupo$Raca <- c(rep(c(rep("B",3), rep("N",3)),2))
apropriacao.topo.grupo$Quantil <- names(decis.topo)
apropriacao.topo.grupo$Grupo <- paste0(apropriacao.topo.grupo$Genero,"-", apropriacao.topo.grupo$Raca)

### Apropriacao da renda pelo topo por grupo ###

apropriacao.topo <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis.topo) {
      
      apropriacao.topo <- rbind(apropriacao.topo,
                                data.frame(Genero = gen,
                                           Raca = paste(rc, collapse = "-"),
                                           Decil = paste(dec, collapse = "-"),
                                           Apro = sum(bra_2018_ind$ils_dispy[bra_2018_ind$ils_dispy >= dec[1] &
                                                                              bra_2018_ind$dgn == gen & 
                                                                              (bra_2018_ind$dra %in% rc)]*
                                                        bra_2018_ind$dwt[bra_2018_ind$ils_dispy >= dec[1] &
                                                                            bra_2018_ind$dgn == gen & 
                                                                            (bra_2018_ind$dra %in% rc)])/
                                             sum(bra_2018_ind$dwt*bra_2018_ind$ils_dispy)))
      
      
    }
    
  }
  
}

# Atributir o nome
apropriacao.topo$Genero <- c(rep("H",nrow(apropriacao.topo)/2), rep("M",nrow(apropriacao.topo)/2))
apropriacao.topo$Raca <- c(rep(c(rep("B",(nrow(apropriacao.topo)/4)), rep("N",(nrow(apropriacao.topo)/4))),2))
apropriacao.topo$Decil <- names(decis.topo)
apropriacao.topo$Grupo <- paste0(apropriacao.topo$Genero,"-", apropriacao.topo$Raca)

### Apropriacao da renda no topo de cada grupo ###

png("ApropGrupoTopo.png", width = 4800, height = 3200, res = 300)

par(mar=c(5, 5, 5, 5), xpd=TRUE)
par(mfrow = c(2,2))

info.grafico <- matrix(apropriacao.topo.grupo$Part[c(1:3,7:9,4:6,10:12)], nrow = 4, ncol = 3, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       c("10%", "1%", "0.1%"))) * 100

cores <- c("#45ff66","#EB52FF","#3366FF","#FEFF41")
legenda <- c("Homens brancos", "Mulheres brancas", "Homens negros", "Mulheres negras")

for(i in 1:4) {
  
  barplot(info.grafico[i,],
          col = cores[i],
          border = "white",
          ylim = c(0,70),
          beside = T,
          font.axis = 2,
          cex.axis = 1.5, 
          cex.names = 1.5,
          cex.lab=1.5,
          xlab = "Quantis do topo",
          ylab = "%")
  
  text(x = seq(.7, 3.1, length.out = 3),
       y = info.grafico[i,]+3,
       labels = round(info.grafico[i,], digits = 0), font = 2)
  
  text(x = 2.9,
       y = 60,
       labels = legenda[i], font = 2, cex = 1.5)
  
}

dev.off()

### Apropriacao da renda pelo topo ###

png("ApropTopo.png", width = 4800, height = 3200, res = 300)

par(mar=c(5, 5, 5, 5), xpd=TRUE)
par(mfrow = c(1,1))

info.grafico <- cbind("0 - 90%" = rowSums(info.grafico[,-ncol(info.grafico)]),
                      matrix(apropriacao.topo$Apro[c(1:3,7:9,4:6,10:12)], nrow = 4, ncol = 3, byrow = T,
                             dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                               "Homens negros", "Mulheres negras"),
                                             c("10%", "1%", "0.1%"))) * 100)


barplot(info.grafico,
        col = c("#45ff66","#EB52FF","#3366FF","#FEFF41"),
        border = "white",
        ylim = c(0,60),
        space = 0.04,
        font.axis = 2,
        cex.axis = 1.5, 
        cex.names = 1.5,
        cex.lab=1.5,
        xlab = "Quantil",
        ylab = "Apropriação da renda total pelo topo (em %)")

legend("topright", 
       inset=c(.1, 0),
       cex = 1.5,
       legend = c("Mulheres negras", "Homens negros", "Mulheres brancas", "Homens brancos"),
       fill = rev(c("#45ff66","#EB52FF","#3366FF","#FEFF41")), 
       bty = "n")

dev.off()



# Gerar lista com minimo e maximo dos decis por grupo
decis.grupo <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in seq(0, .9, .1)) {
      
      decis.grupo <- rbind(decis.grupo,
                           data.frame(Genero = gen,
                                      Raca = paste(rc, collapse = "-"),
                                      Decil = dec,
                                      Renda = weighted.quantile(x = bra_2018_ind$ils_dispy[bra_2018_ind$dgn == gen & (bra_2018_ind$dra %in% rc)],
                                                                w = bra_2018_ind$dwt[bra_2018_ind$dgn == gen & (bra_2018_ind$dra %in% rc)],
                                                                prob = dec)))
      
    }
    
    decis.grupo <- rbind(decis.grupo,
                         data.frame(Genero = gen,
                                    Raca = paste(rc, collapse = "-"),
                                    Decil = "Max",
                                    Renda = max(bra_2018_ind$ils_dispy[bra_2018_ind$dgn == gen & (bra_2018_ind$dra %in% rc)])))
    
  }
  
}

# Atributir o nome
decis.grupo$Genero <- c(rep("H",22), rep("M",22))
decis.grupo$Raca <- c(rep(c(rep("B",11), rep("N",11)),2))
decis.grupo$Decil <- c(paste(seq(0, .9, .1)), "Max")
decis.grupo$Grupo <- paste0(decis.grupo$Genero,"-", decis.grupo$Raca)

# Calcular a concentracao dentro do grupo

criterios <- list("HB" = list("Genero" = genero[1],
                              "Raca" = raca[[1]],
                              "Quantis" = decis.grupo$Renda[1:11]),
                  "HN" = list("Genero" = genero[1],
                              "Raca" = raca[[2]],
                              "Quantis" = decis.grupo$Renda[12:22]),
                  "MB" = list("Genero" = genero[2],
                              "Raca" = raca[[1]],
                              "Quantis" = decis.grupo$Renda[23:33]),
                  "MN" = list("Genero" = genero[2],
                              "Raca" = raca[[2]],
                              "Quantis" = decis.grupo$Renda[34:44]))

apropriacao.grupo <- data.frame()

for (grupo in criterios) {
  
  for (decil in seq_along(grupo$Quantis)[-11]) {
    
    apropriacao.grupo <- rbind(apropriacao.grupo,
                               data.frame(Genero = grupo$Genero,
                                          Raca = paste0(grupo$Raca, collapse = "-"),
                                          Quantil = decil,
                                          Apro = sum(bra_2018_ind$ils_dispy[bra_2018_ind$ils_dispy >= grupo$Quantis[decil] &
                                                                             bra_2018_ind$ils_dispy <= grupo$Quantis[decil+1] &
                                                                             bra_2018_ind$dgn == grupo$Genero & 
                                                                             (bra_2018_ind$dra %in% grupo$Raca)]*
                                                       bra_2018_ind$dwt[bra_2018_ind$ils_dispy >= grupo$Quantis[decil] &
                                                                           bra_2018_ind$ils_dispy <= grupo$Quantis[decil+1] &
                                                                           bra_2018_ind$dgn == grupo$Genero & 
                                                                           (bra_2018_ind$dra %in% grupo$Raca)])/
                                            sum(bra_2018_ind$ils_dispy[bra_2018_ind$dgn == grupo$Genero & 
                                                                        (bra_2018_ind$dra %in% grupo$Raca)]*
                                                  bra_2018_ind$dwt[bra_2018_ind$dgn == grupo$Genero & 
                                                                      (bra_2018_ind$dra %in% grupo$Raca)])))
    
  }
  
}

# Atributir o nome
apropriacao.grupo$Genero <- c(rep("H",20), rep("M",20))
apropriacao.grupo$Raca <- c(rep(c(rep("B",10), rep("N",10)),2))
apropriacao.grupo$Grupo <- paste0(apropriacao.grupo$Genero,"-", apropriacao.grupo$Raca)

### Apropriacao da renda ###

png("ApropDecil.png", width = 4800, height = 3200, res = 300)

# Ajuste das margens para caber a legenda
par(mar=c(5, 5, 5, 5), xpd=TRUE)
par(mfrow = c(1,1))

info.grafico <- matrix(apropriacao_gen_race$Apro[c(1:10,21:30,11:20,31:40)], nrow = 4, ncol = 10, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       1:10)) * 100

barplot(info.grafico,
        col = c("#45ff66","#EB52FF","#3366FF","#FEFF41"),
        border = "white",
        ylim = c(0,60),
        space = 0.04,
        font.axis = 2,
        cex.axis = 1.5, 
        cex.names = 1.5,
        cex.lab=1.5,
        xlab = "Decil",
        ylab = "Apropriação da renda total (em %)")

legend("topleft", 
       inset=c(.1, 0),
       cex = 1.5,
       legend = c("Mulheres negras", "Homens negros", "Mulheres brancas", "Homens brancos"),
       fill = rev(c("#45ff66","#EB52FF","#3366FF","#FEFF41")), 
       bty = "n")

dev.off()






## Concentration coefficient----------------------------------------------------
# Ordering data
bra_2018_hh <- bra_2018_hh[order(bra_2018_hh$ils_dispy),]
bra_2018_ind <- bra_2018_ind[order(bra_2018_ind$ils_dispy),]

# Creating relevant column
bra_2018_hh$ordem <- 1:nrow(bra_2018_hh)
bra_2018_ind$ordem <- 1:nrow(bra_2018_ind)

bra_2018_hh$prop_pop <- cumsum(bra_2018_hh$dwt/sum(bra_2018_hh$dwt))
bra_2018_ind$prop_pop <- cumsum(bra_2018_ind$dwt/sum(bra_2018_hh$dwt))

# Calculating concentration coeffiecient
bra_2018_hh$cc <- cumsum(bra_2018_hh$ils_dispy*bra_2018_hh$dwt/sum(bra_2018_hh$ils_dispy*bra_2018_hh$dwt))

# Plotting graph
# Salvar o grafico a ser feito a seguir
png("CompDemoDecil.png", width = 4800, height = 3200, res = 300)

# Ajuste das margens para caber a legenda
par(mar=c(5, 5, 5, 11), xpd=TRUE)
par(mfrow = c(1,1))

x <- bra_2018_hh$prop_pop
y <- bra_2018_hh$cc
plot(x, y, col = "red")






