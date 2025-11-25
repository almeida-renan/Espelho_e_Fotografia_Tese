
# Percentuais raça-cor por tamanho de partido (Eleitos)

# Este script contém o passo a passo para criar o gráfico 5.11, que mostra
# a distribuição percentual de eleitos por raça (auto e heteroclassificada) e tamanho
# do partido nas eleições de 2014, 2018 e 2022.

# Obs: para criar a variável 'tamanho.partido', checar o script 
# "Percentuais raça-cor por tamanho de partido (Candidatos)"

# Verificando percentuais

# Autodeclaração

# 2014

auto2014 <- HC.TSE2014 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(tamanho.partido, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

# 2018

auto2018 <- HC.TSE2014 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(tamanho.partido, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

# 2022

auto2022 <- HC.TSE2022 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_COR_RACA != "NÃO DIVULGÁVEL") %>%
  filter(DS_COR_RACA != "NÃO INFORMADO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(tamanho.partido, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

# Heteroclassificação

# 2014 

hetero2014 <- HC.TSE2014 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(tamanho.partido, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

# 2018

hetero2018 <- HC.TSE2018 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  group_by(tamanho.partido, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

# 2022

hetero2022 <- HC.TSE2022 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(Cor.final2 != "NÃO DIVULGÁVEL") %>%
  filter(Cor.final2 != "NÃO INFORMADO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
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

ggsave("Percentuais Auto e Hetero Tamanho Partido (Eleitos).png",bg = "white", dpi = 300, width = 8, height = 6)
