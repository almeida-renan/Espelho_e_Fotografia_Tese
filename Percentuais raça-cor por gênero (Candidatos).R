
# Percentuais raça-cor por gênero (Candidatos)

# Este script contém o passo a passo para criar o gráfico 5.3, que mostra
# a distribuição percentual de candidatos por raça (auto e heteroclassificada) e gênero
# nas eleições de 2014, 2018 e 2022.

library(tidyverse)

# 2014 ----

auto2014 <- HC.TSE2014 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  group_by(DS_GENERO, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

hetero2014 <- HC.TSE2014 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  group_by(DS_GENERO, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

# 2018 ----

auto2018 <- HC.TSE2018 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  group_by(DS_GENERO, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

hetero2018 <- HC.TSE2018 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  group_by(DS_GENERO, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

# 2022 ----

auto2022 <- HC.TSE2022 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_COR_RACA != "NÃO DIVULGÁVEL") %>%
  filter(DS_COR_RACA != "NÃO INFORMADO") %>%
  filter(DS_GENERO != "NÃO DIVULGÁVEL") %>%
  group_by(DS_GENERO, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

hetero2022 <- HC.TSE2022 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(Cor.final2 != "NÃO INFORMADO") %>%
  filter(DS_GENERO != "NÃO DIVULGÁVEL") %>%
  group_by(DS_GENERO, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

#Juntando auto 
auto2014 <- auto2014 %>%
  mutate(Ano = 2014)

auto2018 <- auto2018 %>%
  mutate(Ano = 2018)

auto2022 <- auto2022 %>%
  mutate(Ano = 2022)

autojunto <- rbind(auto2014, auto2018, auto2022)

#Juntando hetero

hetero2014 <- hetero2014 %>%
  mutate(Ano = 2014)

hetero2018 <- hetero2018 %>%
  mutate(Ano = 2018)

hetero2022 <- hetero2022 %>%
  mutate(Ano = 2022)

heterojunto <- rbind(hetero2014, hetero2018, hetero2022)

#Juntando Auto e Hetero

autojunto <- autojunto %>%
  rename(Cor = DS_COR_RACA) %>%
  mutate(Classification = "Autodeclaração")

heterojunto <- heterojunto %>%
  rename(Cor = Cor.final2) %>%
  mutate(Classification = "Heteroclassificação")

autohetero <- rbind(autojunto, heterojunto)

# Gráfico
autohetero <- autohetero %>%
  mutate(DS_GENERO = str_to_title(DS_GENERO),
         Cor = str_to_title(Cor))

autohetero$DS_GENERO <- factor(autohetero$DS_GENERO, levels = c("Masculino", "Feminino"))

autohetero %>%
  ggplot(aes(x = factor(Ano), y = perc, group = Cor)) +
  geom_line(aes(color = Cor), size = 1.2, lineend = "round") +
  geom_point(aes(color = Cor), size = 2) +
  facet_grid(Classification ~ DS_GENERO) +
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
ggsave("Percentuais Auto e Hetero Gênero (Candidatos).png",bg = "white", dpi = 300, width = 8, height = 6)
