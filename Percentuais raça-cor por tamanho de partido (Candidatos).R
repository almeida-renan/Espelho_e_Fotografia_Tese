
# Percentuais raça-cor por tamanho de partido (Candidatos)

# Este script contém o passo a passo para criar o gráfico 5.4, que mostra
# a distribuição percentual de candidatos por raça (auto e heteroclassificada) e tamanho
# do partido nas eleições de 2014, 2018 e 2022.

# Criando variável tamanho do partido

#2014

HC.TSE2014 <- HC.TSE2014 %>%
  mutate(tamanho.partido = case_when(
    SG_PARTIDO == "PTC" ~ "Pequeno",
    SG_PARTIDO == "DEM" ~ "Grande",
    SG_PARTIDO == "PT do B" ~ "Pequeno",
    SG_PARTIDO == "PPS" ~ "Médio",
    SG_PARTIDO == "PSDC" ~ "Pequeno",
    SG_PARTIDO == "PMDB" ~ "Grande",
    SG_PARTIDO == "NOVO" ~ "Pequeno",
    SG_PARTIDO == "PATRIOTA" ~ "Pequeno",
    SG_PARTIDO == "PC do B" ~ "Médio",
    SG_PARTIDO == "PCB" ~ "Pequeno",
    SG_PARTIDO == "PCO" ~ "Pequeno",
    SG_PARTIDO == "PDT" ~ "Grande",
    SG_PARTIDO == "PR" ~ "Médio",
    SG_PARTIDO == "PHS" ~ "Pequeno",
    SG_PARTIDO == "PMN" ~ "Pequeno",
    SG_PARTIDO == "PTN" ~ "Pequeno",
    SG_PARTIDO == "PP" ~ "Grande",
    SG_PARTIDO == "PPL" ~ "Pequeno",
    SG_PARTIDO == "PROS" ~ "Médio",
    SG_PARTIDO == "PRP" ~ "Pequeno",
    SG_PARTIDO == "PRTB" ~ "Pequeno",
    SG_PARTIDO == "PSB" ~ "Médio",
    SG_PARTIDO == "PSC" ~ "Médio",
    SG_PARTIDO == "PSD" ~ "Médio",
    SG_PARTIDO == "PSDB" ~ "Grande",
    SG_PARTIDO == "PSL" ~ "Pequeno",
    SG_PARTIDO == "PSOL" ~ "Pequeno",
    SG_PARTIDO == "PSTU" ~ "Pequeno",
    SG_PARTIDO == "PT" ~ "Grande",
    SG_PARTIDO == "PTB" ~ "Médio",
    SG_PARTIDO == "PV" ~ "Médio",
    SG_PARTIDO == "REDE" ~ "Pequeno",
    SG_PARTIDO == "PRB" ~ "Médio",
    SG_PARTIDO == "SD" ~ "Médio"
  ))

#2018

HC.TSE2018 <- HC.TSE2018 %>%
  mutate(tamanho.partido = case_when(
    SG_PARTIDO == "PTC" ~ "Pequeno",
    SG_PARTIDO == "DEM" ~ "Grande",
    SG_PARTIDO == "AVANTE" ~ "Pequeno",
    SG_PARTIDO == "PPS" ~ "Médio",
    SG_PARTIDO == "DC" ~ "Pequeno",
    SG_PARTIDO == "MDB" ~ "Grande",
    SG_PARTIDO == "NOVO" ~ "Pequeno",
    SG_PARTIDO == "PATRIOTA" ~ "Pequeno",
    SG_PARTIDO == "PC do B" ~ "Médio",
    SG_PARTIDO == "PCB" ~ "Pequeno",
    SG_PARTIDO == "PCO" ~ "Pequeno",
    SG_PARTIDO == "PDT" ~ "Grande",
    SG_PARTIDO == "PHS" ~ "Pequeno",
    SG_PARTIDO == "PR" ~ "Médio",
    SG_PARTIDO == "PMB" ~ "Pequeno",
    SG_PARTIDO == "PMN" ~ "Pequeno",
    SG_PARTIDO == "PODE" ~ "Pequeno",
    SG_PARTIDO == "PP" ~ "Grande",
    SG_PARTIDO == "PPL" ~ "Pequeno",
    SG_PARTIDO == "PROS" ~ "Médio",
    SG_PARTIDO == "PRP" ~ "Pequeno",
    SG_PARTIDO == "PRTB" ~ "Pequeno",
    SG_PARTIDO == "PSB" ~ "Médio",
    SG_PARTIDO == "PSC" ~ "Médio",
    SG_PARTIDO == "PSD" ~ "Médio",
    SG_PARTIDO == "PSDB" ~ "Grande",
    SG_PARTIDO == "PSL" ~ "Pequeno",
    SG_PARTIDO == "PSOL" ~ "Pequeno",
    SG_PARTIDO == "PSTU" ~ "Pequeno",
    SG_PARTIDO == "PT" ~ "Grande",
    SG_PARTIDO == "PTB" ~ "Médio",
    SG_PARTIDO == "PV" ~ "Médio",
    SG_PARTIDO == "REDE" ~ "Pequeno",
    SG_PARTIDO == "PRB" ~ "Médio",
    SG_PARTIDO == "SOLIDARIEDADE" ~ "Médio"
  ))

#2022

HC.TSE2022 <- HC.TSE2022 %>%
  mutate(tamanho.partido = case_when(
    SG_PARTIDO == "AGIR" ~ "Pequeno",
    SG_PARTIDO == "AVANTE" ~ "Pequeno",
    SG_PARTIDO == "CIDADANIA" ~ "Médio",
    SG_PARTIDO == "DC" ~ "Pequeno",
    SG_PARTIDO == "MDB" ~ "Grande",
    SG_PARTIDO == "NOVO" ~ "Pequeno",
    SG_PARTIDO == "PATRIOTA" ~ "Pequeno",
    SG_PARTIDO == "PC do B" ~ "Médio",
    SG_PARTIDO == "PCB" ~ "Pequeno",
    SG_PARTIDO == "PCO" ~ "Pequeno",
    SG_PARTIDO == "PDT" ~ "Grande",
    SG_PARTIDO == "PL" ~ "Médio",
    SG_PARTIDO == "PMB" ~ "Pequeno",
    SG_PARTIDO == "PMN" ~ "Pequeno",
    SG_PARTIDO == "PODE" ~ "Pequeno",
    SG_PARTIDO == "PP" ~ "Grande",
    SG_PARTIDO == "PROS" ~ "Médio",
    SG_PARTIDO == "PRTB" ~ "Pequeno",
    SG_PARTIDO == "PSB" ~ "Médio",
    SG_PARTIDO == "PSC" ~ "Médio",
    SG_PARTIDO == "PSD" ~ "Médio",
    SG_PARTIDO == "PSDB" ~ "Grande",
    SG_PARTIDO == "PSOL" ~ "Pequeno",
    SG_PARTIDO == "PSTU" ~ "Pequeno",
    SG_PARTIDO == "PT" ~ "Grande",
    SG_PARTIDO == "PTB" ~ "Médio",
    SG_PARTIDO == "PV" ~ "Médio",
    SG_PARTIDO == "REDE" ~ "Pequeno",
    SG_PARTIDO == "REPUBLICANOS" ~ "Médio",
    SG_PARTIDO == "SOLIDARIEDADE" ~ "Médio",
    SG_PARTIDO == "UNIÃO" ~ "Grande",
    SG_PARTIDO == "UP" ~ "Pequeno"
  ))

# Verificando percentuais

# Autodeclaração

# 2014

auto2014 <- HC.TSE2014 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  group_by(tamanho.partido, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

# 2018

auto2018 <- HC.TSE2014 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  group_by(tamanho.partido, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

# 2022

auto2022 <- HC.TSE2022 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_COR_RACA != "NÃO DIVULGÁVEL") %>%
  filter(DS_COR_RACA != "NÃO INFORMADO") %>%
  group_by(tamanho.partido, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

# Heteroclassificação

# 2014 

hetero2014 <- HC.TSE2014 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  group_by(tamanho.partido, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

# 2018

hetero2018 <- HC.TSE2018 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  group_by(tamanho.partido, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

# 2022

hetero2022 <- HC.TSE2022 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(Cor.final2 != "NÃO DIVULGÁVEL") %>%
  filter(Cor.final2 != "NÃO INFORMADO") %>%
  group_by(tamanho.partido, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

# Juntando auto

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

library(ggrepel)

# Juntando hetero

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
  mutate(Classification = "Autodeclaração")

heterojunto <- heterojunto %>%
  mutate(Classification = "Heteroclassificação")

autohetero <- rbind(autojunto, heterojunto)

# Tirando caixa alta

autohetero <- autohetero %>%
  mutate(Cor = str_to_title(Cor))

# Gráfico auto e hetero

autohetero %>%
  ggplot(aes(x = factor(Ano), y = perc, group = Cor)) +
  geom_line(aes(color = Cor), size = 1.2, lineend = "round") +
  geom_point(aes(color = Cor), size = 2) +
  facet_grid(Classification ~ tamanho.partido) +
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
  geom_text_repel(aes(label = gsub("\\.", ",", as.character(perc))), size = 4) +
  labs(x = "", y = "", color = "")

# Salvando

ggsave("Percentuais Auto e Hetero Tamanho Partido (Candidatos).png",bg = "white", dpi = 300, width = 8, height = 6)
