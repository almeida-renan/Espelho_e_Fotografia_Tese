
# Índice de Gini para análise das receitas de campanha

# Este script mostra o passo a passo para gerar os gráfico 7.7, 7.9 e 7.10, que mostra o coeficiente
# de gini das receitas de campanha por grupo racial, nas eleições de 2014, 2018 e 2022, por tamanho
# e ideologia de partido.

# O script também traz essa informação para as regiões do país, salvas em formato de tabela.

# Pacotes utilizados

library(tidyverse)
library(ineq)
library(openxlsx)

# Regiões ----

#Auto
gini_por_regiao_raca.auto14 <- HC.TSE2014 %>%
  filter(!DS_COR_RACA %in% c("AMARELA", "INDÍGENA")) %>%
  group_by(Region, DS_COR_RACA) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = DS_COR_RACA) %>%
  pivot_wider(names_from = Cor, values_from = gini)

gini_por_regiao_raca.auto18 <- HC.TSE2018 %>%
  filter(!DS_COR_RACA %in% c("AMARELA", "INDÍGENA")) %>%
  group_by(Region, DS_COR_RACA) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = DS_COR_RACA) %>%
  pivot_wider(names_from = Cor, values_from = gini)

gini_por_regiao_raca.auto22 <- HC.TSE2022 %>%
  filter(!DS_COR_RACA %in% c("AMARELA", "INDÍGENA", "NÃO INFORMADO")) %>%
  group_by(Region, DS_COR_RACA) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = DS_COR_RACA) %>%
  pivot_wider(names_from = Cor, values_from = gini)

#Hetero
gini_por_regiao_raca.hetero14 <- HC.TSE2014 %>%
  filter(!Cor.final2 %in% c("AMARELA", "INDÍGENA")) %>%
  group_by(Region, Cor.final2) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = Cor.final2) %>%
  pivot_wider(names_from = Cor, values_from = gini)

gini_por_regiao_raca.hetero18 <- HC.TSE2018 %>%
  filter(!Cor.final2 %in% c("AMARELA", "INDÍGENA")) %>%
  group_by(Region, Cor.final2) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = Cor.final2) %>%
  pivot_wider(names_from = Cor, values_from = gini)

gini_por_regiao_raca.hetero22 <- HC.TSE2022 %>%
  filter(!Cor.final2 %in% c("AMARELA", "INDÍGENA", "NÃO INFORMADO")) %>%
  group_by(Region, Cor.final2) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = Cor.final2) %>%
  pivot_wider(names_from = Cor, values_from = gini)

#Salvando autodeclaração
openxlsx::write.xlsx(gini_por_regiao_raca.auto14, "gini_região.auto14.xlsx")
openxlsx::write.xlsx(gini_por_regiao_raca.auto18, "gini_região.auto18.xlsx")
openxlsx::write.xlsx(gini_por_regiao_raca.auto22, "gini_região.auto22.xlsx")

#Salvando heteroclassificação
openxlsx::write.xlsx(gini_por_regiao_raca.hetero14, "gini_região.hetero14.xlsx")
openxlsx::write.xlsx(gini_por_regiao_raca.hetero18, "gini_região.hetero18.xlsx")
openxlsx::write.xlsx(gini_por_regiao_raca.hetero22, "gini_região.hetero22.xlsx")

# Brasil ----

#Auto

gini_por_raca.auto14 <- HC.TSE2014 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(!is.na(rec.total), rec.total > 0) %>%    # garante que os dados são válidos
  group_by(DS_COR_RACA) %>%
  summarise(gini = ineq::Gini(rec.total)) %>%
  mutate(Ano = 2014, Classification = "Autodeclaração") %>%
  rename(Cor = DS_COR_RACA)

gini_por_raca.auto18 <- HC.TSE2018 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(!is.na(rec.total), rec.total > 0) %>%    # garante que os dados são válidos
  group_by(DS_COR_RACA) %>%
  summarise(gini = ineq::Gini(rec.total)) %>%
  mutate(Ano = 2018, Classification = "Autodeclaração") %>%
  rename(Cor = DS_COR_RACA)

gini_por_raca.auto22 <- HC.TSE2022 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_COR_RACA != "NÃO INFORMADO") %>%
  filter(!is.na(rec.total), rec.total > 0) %>%    # garante que os dados são válidos
  group_by(DS_COR_RACA) %>%
  summarise(gini = ineq::Gini(rec.total)) %>%
  mutate(Ano = 2022, Classification = "Autodeclaração") %>%
  rename(Cor = DS_COR_RACA)

#Hetero

gini_por_raca.hetero14 <- HC.TSE2014 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(!is.na(rec.total), rec.total > 0) %>%    # garante que os dados são válidos
  group_by(Cor.final2) %>%
  summarise(gini = ineq::Gini(rec.total)) %>%
  mutate(Ano = 2014, Classification = "Heteroclassificação") %>%
  rename(Cor = Cor.final2)

gini_por_raca.hetero18 <- HC.TSE2018 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(!is.na(rec.total), rec.total > 0) %>%    # garante que os dados são válidos
  group_by(Cor.final2) %>%
  summarise(gini = ineq::Gini(rec.total)) %>%
  mutate(Ano = 2018, Classification = "Heteroclassificação") %>%
  rename(Cor = Cor.final2)

gini_por_raca.hetero22 <- HC.TSE2022 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(Cor.final2 != "NÃO INFORMADO") %>%
  filter(!is.na(rec.total), rec.total > 0) %>%    # garante que os dados são válidos
  group_by(Cor.final2) %>%
  summarise(gini = ineq::Gini(rec.total)) %>%
  mutate(Ano = 2022, Classification = "Heteroclassificação") %>%
  rename(Cor = Cor.final2)

# Juntando tudo

tudo <- rbind(gini_por_raca.auto14, gini_por_raca.auto18, gini_por_raca.auto22, gini_por_raca.hetero14,
              gini_por_raca.hetero18, gini_por_raca.hetero22)

# Gráfico de linha: Gini ao longo dos anos por grupo racial

tudo <- tudo %>%
  mutate(Cor = str_to_title(Cor))

tudo %>%
  ggplot(aes(x = factor(Ano), y = gini, group = Cor)) +
  geom_line(aes(color = Cor), size = 1.2, lineend = "round") +
  geom_point(aes(color = Cor), size = 2) +
  scale_color_manual(
    values = c("#D9D9D9","#A67C52", "#4D4D4D")
  ) +
  facet_wrap(~ Classification) +
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
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01, decimal.mark = ",")) #+
  #expand_limits(y = 0)

# Salvando o gráfico
ggsave("Gini_receitas.png",bg = "white", dpi = 300, width = 8, height = 4)

# Por tamanho de partido ----

#Auto
gini_por_tamanho_raca.auto14 <- HC.TSE2014 %>%
  filter(!DS_COR_RACA %in% c("AMARELA", "INDÍGENA")) %>%
  group_by(tamanho.partido, DS_COR_RACA) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = DS_COR_RACA) %>%
  mutate(Ano = 2014, Classification = "Autodeclaração")

gini_por_tamanho_raca.auto18 <- HC.TSE2018 %>%
  filter(!DS_COR_RACA %in% c("AMARELA", "INDÍGENA")) %>%
  group_by(tamanho.partido, DS_COR_RACA) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = DS_COR_RACA) %>%
  mutate(Ano = 2018, Classification = "Autodeclaração")

gini_por_tamanho_raca.auto22 <- HC.TSE2022 %>%
  filter(!DS_COR_RACA %in% c("AMARELA", "INDÍGENA", "NÃO INFORMADO")) %>%
  group_by(tamanho.partido, DS_COR_RACA) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = DS_COR_RACA) %>%
  mutate(Ano = 2022, Classification = "Autodeclaração")

#Hetero
gini_por_tamanho_raca.hetero14 <- HC.TSE2014 %>%
  filter(!Cor.final2 %in% c("AMARELA", "INDÍGENA")) %>%
  group_by(tamanho.partido, Cor.final2) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = Cor.final2) %>%
  mutate(Ano = 2014, Classification = "Heteroclassificação")

gini_por_tamanho_raca.hetero18 <- HC.TSE2018 %>%
  filter(!Cor.final2 %in% c("AMARELA", "INDÍGENA")) %>%
  group_by(tamanho.partido, Cor.final2) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = Cor.final2) %>%
  mutate(Ano = 2018, Classification = "Heteroclassificação")

gini_por_tamanho_raca.hetero22 <- HC.TSE2022 %>%
  filter(!Cor.final2 %in% c("AMARELA", "INDÍGENA", "NÃO INFORMADO")) %>%
  group_by(tamanho.partido, Cor.final2) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = Cor.final2) %>%
  mutate(Ano = 2022, Classification = "Heteroclassificação")


# Juntando tudo

tudo <- rbind(gini_por_tamanho_raca.auto14, gini_por_tamanho_raca.auto18, gini_por_tamanho_raca.auto22, gini_por_tamanho_raca.hetero14,
              gini_por_tamanho_raca.hetero18, gini_por_tamanho_raca.hetero22)

# Gráfico de linha: Gini ao longo dos anos por grupo racial

tudo <- tudo %>%
  mutate(Cor = str_to_title(Cor))

tudo$tamanho.partido <- factor(tudo$tamanho.partido,
                               levels = c("Grande", "Médio", "Pequeno"))

tudo %>%
  ggplot(aes(x = factor(Ano), y = gini, group = Cor)) +
  geom_line(aes(color = Cor), size = 1.2, lineend = "round") +
  geom_point(aes(color = Cor), size = 2) +
  scale_color_manual(
    values = c("#D9D9D9","#A67C52", "#4D4D4D")
  ) +
  facet_grid(Classification~tamanho.partido) +
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
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01, decimal.mark = ",")) #+
  #expand_limits(y = 0)

# Salvando o gráfico
ggsave("Gini_receitas_tamanho.png",bg = "white", dpi = 300, width = 8, height = 5)

# Por ideologia de partido

#Auto
gini_por_ideologia_raca.auto14 <- HC.TSE2014 %>%
  filter(!DS_COR_RACA %in% c("AMARELA", "INDÍGENA")) %>%
  group_by(Ideologia.partido, DS_COR_RACA) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = DS_COR_RACA) %>%
  mutate(Ano = 2014, Classification = "Autodeclaração")

gini_por_ideologia_raca.auto18 <- HC.TSE2018 %>%
  filter(!DS_COR_RACA %in% c("AMARELA", "INDÍGENA")) %>%
  group_by(Ideologia.partido, DS_COR_RACA) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = DS_COR_RACA) %>%
  mutate(Ano = 2018, Classification = "Autodeclaração")

gini_por_ideologia_raca.auto22 <- HC.TSE2022 %>%
  filter(!DS_COR_RACA %in% c("AMARELA", "INDÍGENA", "NÃO INFORMADO")) %>%
  group_by(Ideologia.partido, DS_COR_RACA) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = DS_COR_RACA) %>%
  mutate(Ano = 2022, Classification = "Autodeclaração")

#Hetero
gini_por_ideologia_raca.hetero14 <- HC.TSE2014 %>%
  filter(!Cor.final2 %in% c("AMARELA", "INDÍGENA")) %>%
  group_by(Ideologia.partido, Cor.final2) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = Cor.final2) %>%
  mutate(Ano = 2014, Classification = "Heteroclassificação")

gini_por_ideologia_raca.hetero18 <- HC.TSE2018 %>%
  filter(!Cor.final2 %in% c("AMARELA", "INDÍGENA")) %>%
  group_by(Ideologia.partido, Cor.final2) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = Cor.final2) %>%
  mutate(Ano = 2018, Classification = "Heteroclassificação")

gini_por_ideologia_raca.hetero22 <- HC.TSE2022 %>%
  filter(!Cor.final2 %in% c("AMARELA", "INDÍGENA", "NÃO INFORMADO")) %>%
  group_by(Ideologia.partido, Cor.final2) %>%
  filter(n() >= 10) %>%  
  summarise(gini = ineq::Gini(rec.total), .groups = "drop") %>%
  rename(Cor = Cor.final2) %>%
  mutate(Ano = 2022, Classification = "Heteroclassificação")


# Juntando tudo

tudo <- rbind(gini_por_ideologia_raca.auto14, gini_por_ideologia_raca.auto18, gini_por_ideologia_raca.auto22, gini_por_ideologia_raca.hetero14,
              gini_por_ideologia_raca.hetero18, gini_por_ideologia_raca.hetero22)

# Gráfico de linha: Gini ao longo dos anos por grupo racial

tudo <- tudo %>%
  mutate(Cor = str_to_title(Cor))

tudo$Ideologia.partido <- factor(tudo$Ideologia.partido,
                                 levels = c("Esquerda", "Centro", "Direita"))

tudo %>%
  ggplot(aes(x = factor(Ano), y = gini, group = Cor)) +
  geom_line(aes(color = Cor), size = 1.2, lineend = "round") +
  geom_point(aes(color = Cor), size = 2) +
  scale_color_manual(
    values = c("#D9D9D9","#A67C52", "#4D4D4D")
  ) +
  facet_grid(Classification~Ideologia.partido) +
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
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01, decimal.mark = ",")) #+
  #expand_limits(y = 0)

#Salvando o gráfico
ggsave("Gini_receitas_ideologia.png",bg = "white", dpi = 300, width = 8, height = 5)

