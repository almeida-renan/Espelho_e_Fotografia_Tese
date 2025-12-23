
# Mapa do percentual de candidatos brancos

# Este script contém o passo a passo para gerar a Figura 1, que mostra o percentual de
# candidatos brancos por UF do Brasil, segundo a autodeclaração e a heteroclassificação, 
# nas eleições de 2014, 2018 e 2022.

# Carregando pacotes necessários
library(tidyverse)
library(geobr)

#2014
auto2014 <- HC.TSE2014 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  group_by(SG_UF, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

hetero2014 <- HC.TSE2014 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  group_by(SG_UF, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

#2018

auto2018 <- HC.TSE2018 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  group_by(SG_UF, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

hetero2018 <- HC.TSE2018 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  group_by(SG_UF, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

#2022

auto2022 <- HC.TSE2022 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_COR_RACA != "NÃO DIVULGÁVEL") %>%
  filter(DS_COR_RACA != "NÃO INFORMADO") %>%
  group_by(SG_UF, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

hetero2022 <- HC.TSE2022 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(Cor.final2 != "NÃO DIVULGÁVEL") %>%
  filter(Cor.final2 != "NÃO INFORMADO") %>%
  group_by(SG_UF, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

# Juntando autodeclaração

auto2014 <- auto2014 %>%
  rename(Cor = DS_COR_RACA) %>%
  mutate(Ano = 2014)

auto2018 <- auto2018 %>%
  rename(Cor = DS_COR_RACA) %>%
  mutate(Ano = 2018)

auto2022 <- auto2022 %>%
  rename(Cor = DS_COR_RACA) %>%
  mutate(Ano = 2022)

autojunto <- rbind(auto2014, auto2018, auto2022)

# Juntando heteroclassificação

hetero2014 <- hetero2014 %>%
  rename(Cor = Cor.final2) %>%
  mutate(Ano = 2014)

hetero2018 <- hetero2018 %>%
  rename(Cor = Cor.final2) %>%
  mutate(Ano = 2018)

hetero2022 <- hetero2022 %>%
  rename(Cor = Cor.final2) %>%
  mutate(Ano = 2022)

heterojunto <- rbind(hetero2014, hetero2018, hetero2022)

# Juntando auto e hetero

autojunto <- autojunto %>%
  mutate(Classification = "Auto")

heterojunto <- heterojunto %>%
  mutate(Classification = "Hetero")

autohetero <- rbind(autojunto, heterojunto)

autohetero.sóbranco <- autohetero %>%
  filter(Cor == "BRANCA")

# Carregando o shapefile dos estados do Brasil
estados <- read_state(code_state = "all", year = 2020)

# Juntando os dados de auto e heteroclassificação racial com o mapa
mapa_dados <- estados %>%
  left_join(autohetero.sóbranco, by = c("abbrev_state" = "SG_UF"))

# Organizando percentuais

dados <- autohetero.sóbranco %>%
  mutate(nivel = case_when(
    perc > 20 & perc <= 40 ~ "20-40%",
    perc > 40 & perc <= 60 ~ "40-60%",
    perc > 60 & perc <= 80 ~ "60-80%",
    perc > 80 & perc <= 100 ~ "mais de 80%"
  ))

escala_cinza <- colorRampPalette(c("#333333", "#FFFFFF"))
escala_cinza(5)

mapa_dados <- estados %>%
  left_join(dados, by = c("abbrev_state" = "SG_UF"))

# Mapa

ggplot() +
  geom_sf(data = mapa_dados, aes(fill = nivel), color = "black", linewidth = 0.2) +
  scale_fill_manual(
    name = "Percentual",
    values = escala_cinza(4)
  ) +
  facet_grid(Classification ~ Ano) +
  theme_void() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 13))

# Salvando o mapa
ggsave("Mapa Candidatos.png",bg = "white", dpi = 300, width = 8, height = 6)
