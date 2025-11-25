
# Percentuais de raça/cor nas regiões (Eleitos)

# Este script contém o passo a passo para criar os gráficos 5.7 e 5.8, que mostram
# a distribuição percentual de eleitos por raça (auto e heteroclassificada) e região
# nas eleições de 2014, 2018 e 2022.


library(tidyverse)

# Criando variável de região

#2014
HC.TSE2014 <- HC.TSE2014 %>%
  mutate(Region = as.factor(case_when(
    SG_UF == "GO" | SG_UF == "MT" | SG_UF == "MS" | SG_UF == "DF" ~ "Centro-Oeste",
    SG_UF == "ES" | SG_UF == "MG" | SG_UF == "RJ" | SG_UF == "SP" ~ "Sudeste",
    SG_UF == "PR" | SG_UF == "SC" | SG_UF == "RS" ~ "Sul",
    SG_UF == "AC" | SG_UF == "AP" | SG_UF == "AM" | SG_UF == "PA" | SG_UF == "RO" | SG_UF == "RR" | SG_UF == "TO" ~ "Norte",
    SG_UF == "AL" | SG_UF == "BA" | SG_UF == "CE" | SG_UF == "MA" | SG_UF == "PB" | SG_UF == "PE" | SG_UF == "PI" | SG_UF == "RN" | SG_UF == "SE" ~ "Nordeste"
  )))

#2018
HC.TSE2018 <- HC.TSE2018 %>%
  mutate(Region = as.factor(case_when(
    SG_UF == "GO" | SG_UF == "MT" | SG_UF == "MS" | SG_UF == "DF" ~ "Centro-Oeste",
    SG_UF == "ES" | SG_UF == "MG" | SG_UF == "RJ" | SG_UF == "SP" ~ "Sudeste",
    SG_UF == "PR" | SG_UF == "SC" | SG_UF == "RS" ~ "Sul",
    SG_UF == "AC" | SG_UF == "AP" | SG_UF == "AM" | SG_UF == "PA" | SG_UF == "RO" | SG_UF == "RR" | SG_UF == "TO" ~ "Norte",
    SG_UF == "AL" | SG_UF == "BA" | SG_UF == "CE" | SG_UF == "MA" | SG_UF == "PB" | SG_UF == "PE" | SG_UF == "PI" | SG_UF == "RN" | SG_UF == "SE" ~ "Nordeste"
  )))

#2022
HC.TSE2022 <- HC.TSE2022 %>%
  mutate(Region = as.factor(case_when(
    SG_UF == "GO" | SG_UF == "MT" | SG_UF == "MS" | SG_UF == "DF" ~ "Centro-Oeste",
    SG_UF == "ES" | SG_UF == "MG" | SG_UF == "RJ" | SG_UF == "SP" ~ "Sudeste",
    SG_UF == "PR" | SG_UF == "SC" | SG_UF == "RS" ~ "Sul",
    SG_UF == "AC" | SG_UF == "AP" | SG_UF == "AM" | SG_UF == "PA" | SG_UF == "RO" | SG_UF == "RR" | SG_UF == "TO" ~ "Norte",
    SG_UF == "AL" | SG_UF == "BA" | SG_UF == "CE" | SG_UF == "MA" | SG_UF == "PB" | SG_UF == "PE" | SG_UF == "PI" | SG_UF == "RN" | SG_UF == "SE" ~ "Nordeste"
  )))


# Verificando percentuais por região

# Autodeclaração ----

#2014
autoREG <- HC.TSE2014 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(Region, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(prop = round(prop.table(n)*100, digits = 1))

#2018
autoREG2 <- HC.TSE2018 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(Region, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(prop = round(prop.table(n)*100, digits = 1))

# 2022
autoREG3 <- HC.TSE2022 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_COR_RACA != "NÃO DIVULGÁVEL") %>%
  filter(DS_COR_RACA != "NÃO INFORMADO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(Region, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(prop = round(prop.table(n)*100, digits = 1))

# Brasil 2014

Brasil2014 <- HC.TSE2014 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(prop = round(prop.table(n)*100, digits = 1))

# Brasil 2018

Brasil2018 <- HC.TSE2018 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(prop = round(prop.table(n)*100, digits = 1))

# Brasil 2022

Brasil2022 <- HC.TSE2022 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_COR_RACA != "NÃO DIVULGÁVEL") %>%
  filter(DS_COR_RACA != "NÃO INFORMADO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(prop = round(prop.table(n)*100, digits = 1))

# Juntanto tudo

Brasil2014 <- Brasil2014 %>%
  mutate(Ano = 2014,
         Region = "Brasil") %>%
  select(Region, DS_COR_RACA, n, prop, Ano)

Brasil2018 <- Brasil2018 %>%
  mutate(Ano = 2018,
         Region = "Brasil") %>%
  select(Region, DS_COR_RACA, n, prop, Ano)

Brasil2022 <- Brasil2022 %>%
  mutate(Ano = 2022,
         Region = "Brasil") %>%
  select(Region, DS_COR_RACA, n, prop, Ano)

autoREG <- autoREG %>%
  mutate(Ano = 2014)

autoREG2 <- autoREG2 %>%
  mutate(Ano = 2018)

autoREG3 <- autoREG3 %>%
  mutate(Ano = 2022)

tudojunto <- rbind(autoREG, autoREG2, autoREG3, Brasil2014, Brasil2018, Brasil2022)

library(ggrepel)

# Gráfico

tudojunto <- tudojunto %>%
  mutate(DS_COR_RACA = str_to_title(DS_COR_RACA))

tudojunto %>%
  ggplot(aes(x = factor(Ano), y = prop, group = DS_COR_RACA)) +
  geom_line(aes(color = DS_COR_RACA), size = 1.2, lineend = "round") +
  geom_point(aes(color = DS_COR_RACA), size = 2) +
  facet_wrap(vars(Region)) +
  scale_color_manual(
    values = c("#D9D9D9","#A67C52", "#4D4D4D")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 13),
    axis.text.x = element_text(size = 11),  # Tamanho do título do eixo x
    axis.text.y = element_text(size = 11)
  ) +
  labs(x = "", y = "", color = "") +
  geom_text_repel(aes(label = gsub("\\.", ",", as.character(prop))), size = 4)

# Para salvar, escolher o caminho usando o ggsave
ggsave("Percentuais Autodeclaração Regiões (Eleitos).png",bg = "white", dpi = 300, width = 8, height = 6)


# Heteroclassificação ----

#2014
heteroREG <- HC.TSE2014 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(Region, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(prop = round(prop.table(n)*100, digits = 1))

# 2018
heteroREG2 <- HC.TSE2018 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(Region, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(prop = round(prop.table(n)*100, digits = 1))


# 2022
heteroREG3 <- HC.TSE2022 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(Cor.final2 != "NÃO DIVULGÁVEL") %>%
  filter(Cor.final2 != "NÃO INFORMADO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(Region, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(prop = round(prop.table(n)*100, digits = 1))

# Juntando os anos

heteroREG <- heteroREG %>%
  mutate(Ano = 2014)

heteroREG2 <- heteroREG2 %>%
  mutate(Ano = 2018)

heteroREG3 <- heteroREG3 %>%
  mutate(Ano = 2022)

# Brasil.Hetero 2014

Brasil.Hetero2014 <- HC.TSE2014 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(prop = round(prop.table(n)*100, digits = 1))

# Brasil.Hetero 2018

Brasil.Hetero2018 <- HC.TSE2018 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(prop = round(prop.table(n)*100, digits = 1))

# Brasil.Hetero 2022

Brasil.Hetero2022 <- HC.TSE2022 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(Cor.final2 != "NÃO DIVULGÁVEL") %>%
  filter(Cor.final2 != "NÃO INFORMADO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(prop = round(prop.table(n)*100, digits = 1))

# Juntanto tudo

Brasil.Hetero2014 <- Brasil.Hetero2014 %>%
  mutate(Ano = 2014,
         Region = "Brasil") %>%
  select(Region, Cor.final2, n, prop, Ano)

Brasil.Hetero2018 <- Brasil.Hetero2018 %>%
  mutate(Ano = 2018,
         Region = "Brasil") %>%
  select(Region, Cor.final2, n, prop, Ano)

Brasil.Hetero2022 <- Brasil.Hetero2022 %>%
  mutate(Ano = 2022,
         Region = "Brasil") %>%
  select(Region, Cor.final2, n, prop, Ano)


tudojuntoHetero <- rbind(heteroREG, heteroREG2, heteroREG3, Brasil.Hetero2014, Brasil.Hetero2018, Brasil.Hetero2022)

tudojuntoHetero <- tudojuntoHetero %>% 
  mutate(Classificacao = "Heteroclassificação") %>%
  rename(Cor = Cor.final2)

# Igual ao gráfico de autodeclaração

tudojuntoHetero <- tudojuntoHetero %>%
  mutate(Cor = str_to_title(Cor))

tudojuntoHetero %>%
  ggplot(aes(x = factor(Ano), y = prop, group = Cor)) +
  geom_line(aes(color = Cor), size = 1.2, lineend = "round") +
  geom_point(aes(color = Cor), size = 2) +
  facet_wrap(vars(Region)) +
  scale_color_manual(
    values = c("#D9D9D9","#A67C52", "#4D4D4D")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 13),
    axis.text.x = element_text(size = 11),  # Tamanho do título do eixo x
    axis.text.y = element_text(size = 11)
  ) +
  labs(x = "", y = "", color = "") +
  geom_text_repel(aes(label = gsub("\\.", ",", as.character(prop))), size = 4)

# Salvando
ggsave("Percentuais Heteroclassificação Regiões (Eleitos).png",bg = "white", dpi = 300, width = 8, height = 6)



