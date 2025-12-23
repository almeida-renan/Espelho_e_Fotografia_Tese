
# Cruzamento Autodeclaração e Heteroclassificação

# Este script contém o passo a passo para a criação das tabelas 5.1, 5.2 e 5.3, que mostram o
# cruzamento entre autodeclaração e heteroclassificação, nas eleições de 2014, 2018 e 2022.

# Obs: os percentuais foram calculados no Excel a partir dos valores brutos presentes nas
# tabelas geradas neste script.

# Carregando os pacotes necessários
library(janitor)
library(tidyverse)
library(openxlsx)

ordem_racas <- c("BRANCA", "PARDA", "PRETA", "AMARELA", "INDÍGENA")

# 2014

HC.TSE2014$DS_COR_RACA <- factor(HC.TSE2014$DS_COR_RACA, levels = ordem_racas)
HC.TSE2014$Cor.final2 <- factor(HC.TSE2014$Cor.final2, levels = ordem_racas)

CrossAH.14 <- HC.TSE2014 %>%
  tabyl(DS_COR_RACA, Cor.final2) %>%
  adorn_totals(where = c("row", "col"))

#Salvando tabela de 2014
openxlsx::write.xlsx(CrossAH.14, "CrossAH14.xlsx")

# 2018

HC.TSE2018$DS_COR_RACA <- factor(HC.TSE2018$DS_COR_RACA, levels = ordem_racas)
HC.TSE2018$Cor.final2 <- factor(HC.TSE2018$Cor.final2, levels = ordem_racas)

CrossAH.18 <- HC.TSE2018 %>%
  tabyl(DS_COR_RACA, Cor.final2) %>%
  adorn_totals(where = c("row", "col"))

openxlsx::write.xlsx(CrossAH.18, "CrossAH18.xlsx")

# 2022

HC.TSE2022$DS_COR_RACA <- factor(HC.TSE2022$DS_COR_RACA, levels = ordem_racas)
HC.TSE2022$Cor.final2 <- factor(HC.TSE2022$Cor.final2, levels = ordem_racas)

CrossAH.22 <- HC.TSE2022 %>%
  tabyl(DS_COR_RACA, Cor.final2, show_na = F) %>%
  adorn_totals(where = c("row", "col"))

openxlsx::write.xlsx(CrossAH.22, "CrossAH22.xlsx")
