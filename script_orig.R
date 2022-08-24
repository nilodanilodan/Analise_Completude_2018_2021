##PRODUTO 1##

install.packages("tidyr")

library(data.table)
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)


#CONSOLIDAÇÃO BASE DE DADOS#
setwd("C:\\Users\\danilo.imbimbo\\Downloads\\Dados")

#Importando os Dados no R#
#2018#
data_2018 <- read.csv("2018.csv", header = T, sep = ";")
subset_2018_empenhada <- data_2018[grep("Despesas_Empenhadas", data_2018$CONTA), ]
subset_2018_liquidada <- data_2018[grep("Despesas_Liquidadas", data_2018$CONTA), ]

rm(data_2018)
gc()

#2019#
data_2019 <- read.csv("2019.csv", header = T, sep = ";")
subset_2019_empenhada <- data_2019[grep("Despesas_Empenhadas", data_2019$CONTA), ]
subset_2019_liquidada <- data_2019[grep("Despesas_Liquidadas", data_2019$CONTA), ]

rm(data_2019)

gc()

#2020#
data_2020 <- read.csv("2020.csv", header = T, sep = ";")
subset_2020_empenhada <- data_2020[grep("Despesas_Empenhadas", data_2020$CONTA), ]
subset_2020_liquidada <- data_2020[grep("Despesas_Liquidadas", data_2020$CONTA), ]

rm(data_2020)

gc()

#2021#
data_2021 <- read.csv("2021.csv", header = T, sep = ";")
subset_2021_empenhada <- data_2021[grep("Despesas_Empenhadas", data_2021$CONTA), ]
subset_2021_liquidada <- data_2021[grep("Despesas_Liquidadas", data_2021$CONTA), ]

rm(data_2021)

gc()

#Organizando os Data_Frames#
#Criando coluna ano para todos os DFs
mget(ls(pattern = "^subset_\\d{4}_.*")) %>%
  imap(~ .x %>%   
         mutate(ANO = str_extract(.y, "\\d{4}"))) %>%
  list2env(.GlobalEnv)

subset_2018_empenhada$ANO <- 2018
subset_2018_liquidada$ANO <- 2018
subset_2019_empenhada$ANO <- 2019
subset_2019_liquidada$ANO <- 2019
subset_2020_empenhada$ANO <- 2020
subset_2020_liquidada$ANO <- 2020
subset_2021_empenhada$ANO <- 2021
subset_2021_liquidada$ANO <- 2021

#juntando os df em apenas 2
empenhada <- bind_rows(subset_2018_empenhada, subset_2019_empenhada, subset_2020_empenhada, subset_2021_empenhada)
liquidada <- bind_rows(subset_2018_liquidada, subset_2019_liquidada, subset_2020_liquidada, subset_2021_liquidada)

rm(list = ls()[grep("subset", ls())])

#alterando nomes
colnames(empenhada) <- c("cod_munic", "munic", "UF", "CONTA", "FONTE", "codigo", "valor", "ANO")
colnames(liquidada) <- c("cod_munic", "munic", "UF", "CONTA", "FONTE", "codigo", "valor", "ANO")
gc()

#tirando pontos repetidos no final
empenhada$codigo <- str_replace_all(empenhada$codigo, "[[:punct:]]", "") #tive que tirar pontos que se repetiram ao final
liquidada$codigo <- str_replace_all(liquidada$codigo, "[[:punct:]]", "") #tive que tirar pontos que se repetiram ao final

#manter últimos 10 dígitos dos códigos
empenhada$codigo <- substr(empenhada$codigo, 1, 10) #mantendo apenas os 10 digitos dos codigos
liquidada$codigo <- substr(liquidada$codigo, 1, 10) #mantendo apenas os 10 digitos dos codigos

#checando qualidade da informação#
#para empenhado
qualidade_empenhada <- empenhada %>% 
  select(ANO, cod_munic, munic, UF, CONTA, codigo, valor) %>% 
  filter(codigo == "ACDO000007") %>%
  mutate(valor = as.numeric(valor)) %>%
  mutate(valor = replace_na(valor, 0))  %>%
  filter(codigo == "ACDO000007" & valor == 0) %>%
  group_by(ANO) %>% 
  summarise(n = n()) # vários municípios com valores zerados dos gastos em ASPS

#para liquidado
qualidade_liquidado <- liquidada %>% 
  select(ANO, cod_munic, munic, UF, CONTA, codigo, valor) %>% 
  filter(codigo == "ACDO000007") %>%
  mutate(valor = as.numeric(valor)) %>%
  mutate(valor = replace_na(valor, 0))  %>%
  filter(codigo == "ACDO000007" & valor == 0) %>%
  group_by(ANO) %>% 
  summarise(n = n())

#Determinando Tabela Descritiva das Informações para Liquidado e Empenhado#
#tabela descritiva de qualidade
qualidade_empenhada$Transmitiram <- 5569 - qualidade_empenhada$n
colnames(qualidade_empenhada) <- c("Ano", "Não_Transmitiram", "Transmitiram")

qualidade_liquidado$Transmitiram <- 5569 - qualidade_liquidado$n
colnames(qualidade_liquidado) <- c("Ano", "Não_Transmitiram", "Transmitiram")

#agrupar por UF para verificar municípios que não preencheram adequadamente a informação em cada Unidade da Federação
qualidade_empenhada_UF <- empenhada %>% 
  select(ANO, cod_munic, munic, UF, CONTA, codigo, valor) %>% 
  filter(codigo == "ACDO000007") %>%
  mutate(valor = as.numeric(valor)) %>%
  mutate(valor = replace_na(valor, 0))  %>%
  filter(codigo == "ACDO000007" & valor == 0) %>%
  group_by(UF, ANO) %>% 
  summarise(n = n())

write.csv(qualidade_empenhada_UF, "uf.csv", row.names = F)

##PARA VERIFICAR DIFERENÇA ENTRE EMPENHADO E LIQUIDADO##
asps_empenhada <- empenhada[grep("ACDO000007", empenhada$codigo), ]
summarise(asps_empenhada)
asps_empenhada <- asps_empenhada %>%
  mutate(valor = as.numeric(valor))
totalasps_empenhada <- aggregate(valor ~ ANO, asps_empenhada, sum)

asps_liquidada <- liquidada[grep("ACDO000007", liquidada$codigo), ]
summarise(asps_liquidada)
asps_liquidada <- asps_liquidada %>%
  mutate(valor = as.numeric(valor))
totalasps_liquidada <- aggregate(valor ~ ANO, asps_liquidada, sum)

dif <- asps_empenhada$valor - asps_liquidada$valor
summary(dif)

#verificar número de municípios na base
n_distinct(empenhada$cod_munic)

#população e localidade
pop <- read.csv2(file = "pop1.csv", sep=";" , dec=",", stringsAsFactors = FALSE)

localidade <- read.csv(file = "RurUrb.csv", stringsAsFactors = FALSE)
localidade <- localidade[,c("BaseRurU_1", "BaseRurU_2","Tipologia_")]

names(localidade)[1]<-"cod_munic"
names(localidade)[2]<-"munic" 
names(localidade)[3]<-"Localidade" 
names(pop)[3] <- "ano"
names(pop)[4] <- "pop"

localidade$cod_munic <-  as.numeric(localidade$cod_munic)
pop$POPULACAO2 <-  as.numeric(pop$pop)
colSums(is.na(pop))

localidade$cod_munic <- substr(localidade$cod_munic, 1, 6)

indicadores <- merge(pop, localidade, by = c("cod_munic"))
names(indicadores)[3] <- "Ano"

indicadores <- indicadores %>%
  select(cod_munic, munic.x, pop, Localidade, Ano)

colnames(indicadores) <- c("cod_munic", "munic", "pop", "localidade", "ANO")
indicadores$ANO <- as.numeric(indicadores$ANO)

asps_empenhada <- empenhada[grep("ACDO000007", empenhada$codigo), ]

asps_empenhada <- asps_empenhada %>%
    mutate(valor = as.numeric(valor))
asps_empenhada <- asps_empenhada %>%
    select(cod_munic, UF, CONTA, FONTE, codigo, valor, ANO)

#adicionando infos na tabela original
asps_indicadores_empenhado <- merge(asps_empenhada, indicadores, by = c("ANO", "cod_munic"), all.x = T)

#urbano, urbano rural
asps_indicadores_empenhado_local <- asps_indicadores_empenhado %>%
    select(ANO, cod_munic, munic, codigo, valor, pop, localidade) %>%
    mutate(valor = replace_na(valor, 0)) %>%
    filter(valor == 0) %>%
    group_by(ANO, localidade) %>%
    summarise(n = n())

write.csv(asps_indicadores_empenhado_local, "local.csv", row.names = F)

#criando estratos pop ibge
asps_indicadores_empenhado <- asps_indicadores_empenhado %>%
    mutate(pop_ibge = case_when(pop <= 5000 ~ "até 5",
                                pop <= 10000 ~ "> 5 a 10",
                                pop <= 25000 ~ "> 10 a 25",
                                pop <= 50000 ~ "> 25 a 50",
                                pop <= 100000 ~ "> 50 a 100",
                                pop <= 500000 ~ "> 100 a 500",
                                pop > 500000 ~ "> 500"))

asps_indicadores_empenhado_porte <- asps_indicadores_empenhado %>%
    select(ANO, cod_munic, munic, codigo, valor, pop, pop_ibge) %>%
    mutate(valor = replace_na(valor, 0)) %>%
    filter(valor == 0) %>%
    group_by(ANO, pop_ibge) %>%
    summarise(n = n())

write.csv(asps_indicadores_empenhado_porte, "porte.csv", row.names = F)

#Descobrindo o número total de municípios por indicadores para construir as tabelas#
#Total de Municípios Por UF (geral)
asps_indicadores_empenhado_por_UF <- asps_indicadores_empenhado %>%
    group_by(ANO, UF) %>%
    summarise(n = n())
write.csv(asps_indicadores_empenhado_por_UF, "por_uf.csv", row.names = F)
#Total de Municípios por Localidade (geral)
asps_indicadores_empenhado_por_localidade <- asps_indicadores_empenhado %>%
    group_by(ANO, localidade) %>%
    summarise(n = n())
write.csv(asps_indicadores_empenhado_por_localidade, "por_localidade.csv", row.names = F)
#Total de Municípios por porte (geral)
asps_indicadores_empenhado_por_porte <- asps_indicadores_empenhado %>%
    group_by(ANO, pop_ibge) %>%
    summarise(n = n())

#Descobrindo os Municípios com repetição de erro#
asps_indicadores_empenhado_repetido <- asps_indicadores_empenhado %>%
    select(ANO, valor, munic) %>%
    mutate(valor = replace_na(valor, 0)) %>%
    filter(valor == 0) 
write.csv(asps_indicadores_empenhado_repetido, "repetido.csv", row.names = F)

repetido <- table(asps_indicadores_empenhado_repetido$munic)
write.csv(repetido, "repetido2.csv", row.names = F)
