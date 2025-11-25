
# Gráfico diferença H - A por região

# Este script contém o passo a passo para criar o gráfico 6.1, que mostra
# a diferença em pontos percentuais entre a heteroclassificação e a autodeclaração
# racial de candidatos a deputado federal, por região, nas eleições de 2014, 2018 e 2022.

# Obs: a variável 'Region' é criada no script intitulado "Percentuais raça-cor por região"

#2014
auto2014 <- HC.TSE2014 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  group_by(Region, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

auto2014 <- auto2014 %>%
  select(-n)

auto2014 <- auto2014 %>%
  pivot_wider(names_from = DS_COR_RACA, values_from = perc)

hetero2014 <- HC.TSE2014 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  group_by(Region, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

hetero2014 <- hetero2014 %>%
  select(-n)

hetero2014 <- hetero2014 %>%
  pivot_wider(names_from = Cor.final2, values_from = perc)

junto2014 <- cbind(hetero2014, auto2014)

junto2014 <- junto2014 %>%
  mutate("Brancos(as)" = BRANCA...2 - BRANCA...6,
         "Pretos(as)" = PRETA...4 - PRETA...8,
         "Pardos(as)" = PARDA...3 - PARDA...7)

junto2014 <- junto2014 %>%
  select(Region...1, "Brancos(as)","Pardos(as)", "Pretos(as)")

#Brasil 2014

Brasil.auto14 <- HC.TSE2014 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  group_by(DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

Brasil.auto14 <- Brasil.auto14 %>%
  select(-n)

Brasil.auto14 <- Brasil.auto14 %>%
  pivot_wider(names_from = DS_COR_RACA, values_from = perc)

Brasil.hetero14 <- HC.TSE2014 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  group_by(Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

Brasil.hetero14 <- Brasil.hetero14 %>%
  select(-n)

Brasil.hetero14 <- Brasil.hetero14 %>%
  pivot_wider(names_from = Cor.final2, values_from = perc) %>%
  rename(Branca.h = BRANCA, Preta.h = PRETA, Parda.h = PARDA)

Brasiljunto2014 <- cbind(Brasil.hetero14, Brasil.auto14)

Brasiljunto2014 <- Brasiljunto2014 %>%
  mutate("Brancos(as)" = Branca.h - BRANCA,
         "Pretos(as)" = Preta.h - PRETA,
         "Pardos(as)" = Parda.h - PARDA) %>%
  mutate(Region...1 = "Brasil")


Brasiljunto2014 <- Brasiljunto2014 %>%
  select(Region...1, "Brancos(as)","Pardos(as)", "Pretos(as)")

Brasiljunto2014 <- Brasiljunto2014 %>% pivot_longer(2:4, names_to = "Cor")

junto2014 <- junto2014 %>%
  pivot_longer(2:4, names_to = "Cor")

junto2014 <- rbind(Brasiljunto2014, junto2014)


#2018
auto2018 <- HC.TSE2018 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  group_by(Region, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

auto2018 <- auto2018 %>%
  select(-n)

auto2018 <- auto2018 %>%
  pivot_wider(names_from = DS_COR_RACA, values_from = perc)

hetero2018 <- HC.TSE2018 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  group_by(Region, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

hetero2018 <- hetero2018 %>%
  select(-n)

hetero2018 <- hetero2018 %>%
  pivot_wider(names_from = Cor.final2, values_from = perc)

junto2018 <- cbind(hetero2018, auto2018)

junto2018 <- junto2018 %>%
  mutate("Brancos(as)" = BRANCA...2 - BRANCA...6,
         "Pretos(as)" = PRETA...4 - PRETA...8,
         "Pardos(as)" = PARDA...3 - PARDA...7)

junto2018 <- junto2018 %>%
  select(Region...1, "Brancos(as)","Pardos(as)", "Pretos(as)")

#Brasil 2018

Brasil.auto18 <- HC.TSE2018 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  group_by(DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

Brasil.auto18 <- Brasil.auto18 %>%
  select(-n)

Brasil.auto18 <- Brasil.auto18 %>%
  pivot_wider(names_from = DS_COR_RACA, values_from = perc)

Brasil.hetero18 <- HC.TSE2018 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  group_by(Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

Brasil.hetero18 <- Brasil.hetero18 %>%
  select(-n)

Brasil.hetero18 <- Brasil.hetero18 %>%
  pivot_wider(names_from = Cor.final2, values_from = perc) %>%
  rename(Branca.h = BRANCA, Preta.h = PRETA, Parda.h = PARDA)

Brasiljunto2018 <- cbind(Brasil.hetero18, Brasil.auto18)

Brasiljunto2018 <- Brasiljunto2018 %>%
  mutate("Brancos(as)" = Branca.h - BRANCA,
         "Pretos(as)" = Preta.h - PRETA,
         "Pardos(as)" = Parda.h - PARDA) %>%
  mutate(Region...1 = "Brasil")


Brasiljunto2018 <- Brasiljunto2018 %>%
  select(Region...1, "Brancos(as)","Pardos(as)", "Pretos(as)")

Brasiljunto2018 <- Brasiljunto2018 %>% pivot_longer(2:4, names_to = "Cor")

junto2018 <- junto2018 %>%
  pivot_longer(2:4, names_to = "Cor")

junto2018 <- rbind(Brasiljunto2018, junto2018)

#2022

auto2022 <- HC.TSE2022 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_COR_RACA != "NÃO DIVULGÁVEL") %>%
  filter(DS_COR_RACA != "NÃO INFORMADO") %>%
  group_by(Region, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

auto2022 <- auto2022 %>%
  select(-n)

auto2022 <- auto2022 %>%
  pivot_wider(names_from = DS_COR_RACA, values_from = perc)

hetero2022 <- HC.TSE2022 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(Cor.final2 != "NÃO DIVULGÁVEL") %>%
  filter(Cor.final2 != "NÃO INFORMADO") %>%
  group_by(Region, Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

hetero2022 <- hetero2022 %>%
  select(-n)

hetero2022 <- hetero2022 %>%
  pivot_wider(names_from = Cor.final2, values_from = perc)

junto2022 <- cbind(hetero2022, auto2022)

junto2022 <- junto2022 %>%
  mutate("Brancos(as)" = BRANCA...2 - BRANCA...6,
         "Pretos(as)" = PRETA...4 - PRETA...8,
         "Pardos(as)" = PARDA...3 - PARDA...7)

junto2022 <- junto2022 %>%
  select(Region...1, "Brancos(as)","Pardos(as)", "Pretos(as)")

#Brasil 2022

Brasil.auto22 <- HC.TSE2022 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_COR_RACA != "NÃO DIVULGÁVEL") %>%
  filter(DS_COR_RACA != "NÃO INFORMADO") %>%
  group_by(DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

Brasil.auto22 <- Brasil.auto22 %>%
  select(-n)

Brasil.auto22 <- Brasil.auto22 %>%
  pivot_wider(names_from = DS_COR_RACA, values_from = perc)

Brasil.hetero22 <- HC.TSE2022 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(Cor.final2 != "NÃO DIVULGÁVEL") %>%
  filter(Cor.final2 != "NÃO INFORMADO") %>%
  group_by(Cor.final2) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

Brasil.hetero22 <- Brasil.hetero22 %>%
  select(-n)

Brasil.hetero22 <- Brasil.hetero22 %>%
  pivot_wider(names_from = Cor.final2, values_from = perc) %>%
  rename(Branca.h = BRANCA, Preta.h = PRETA, Parda.h = PARDA)

Brasiljunto2022 <- cbind(Brasil.hetero22, Brasil.auto22)

Brasiljunto2022 <- Brasiljunto2022 %>%
  mutate("Brancos(as)" = Branca.h - BRANCA,
         "Pretos(as)" = Preta.h - PRETA,
         "Pardos(as)" = Parda.h - PARDA) %>%
  mutate(Region...1 = "Brasil")


Brasiljunto2022 <- Brasiljunto2022 %>%
  select(Region...1, "Brancos(as)","Pardos(as)", "Pretos(as)")

Brasiljunto2022 <- Brasiljunto2022 %>% pivot_longer(2:4, names_to = "Cor")

junto2022 <- junto2022 %>%
  pivot_longer(2:4, names_to = "Cor")

junto2022 <- rbind(Brasiljunto2022, junto2022)

# Juntando tudo

junto2014 <- junto2014 %>%
  mutate(Ano = 2014)

junto2018 <- junto2018 %>%
  mutate(Ano = 2018)

junto2022 <- junto2022 %>%
  mutate(Ano = 2022)

tudojunto <- rbind(junto2014, junto2018, junto2022)


# Gráfico
tudojunto <- tudojunto %>%
  rename(Region = Region...1)

tudojunto %>%
  mutate(Cor2 = case_when(
    Cor == "Brancos(as)" ~ "Branca",
    Cor == "Pardos(as)" ~ "Parda",
    Cor == "Pretos(as)" ~ "Preta"
  )) %>%
  ggplot(aes(x = Cor2, y = value, fill = factor(Ano))) +
  geom_bar(stat = "identity", position = "dodge2") +
  facet_wrap(vars(Region...1)) +
  theme_minimal() +
  scale_fill_manual(
    values = c("#fc8d59","#1f78b4", "#b2df8a")
  ) +
  geom_text(aes(label = gsub("\\.", ",", as.character(round(value, 0))), vjust = ifelse(value >= 0, -0.5, 1.5)), size = 3.5, position = position_dodge(0.9)) +
  labs(x = "", y = "", fill = "") +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(size = 11),  # Tamanho do título do eixo x
    axis.text.y = element_text(size = 11)) +
  expand_limits(y = max(tudojunto$value) * 1.1) +
  expand_limits(y = max(tudojunto$value) * -1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")

ggsave("Diferença Hetero Auto Regiões.png",bg = "white", dpi = 300, width = 8, height = 6)


