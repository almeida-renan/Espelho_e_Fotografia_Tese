# Gráfico diferença H - A por genero

# Este script contém o passo a passo para criar o gráfico 6.2, que mostra
# a diferença em pontos percentuais entre a heteroclassificação e a autodeclaração
# racial de candidatos a deputado federal, por gênero, nas eleições de 2014, 2018 e 2022.

#2014
auto2014 <- HC.TSE2014 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  group_by(DS_GENERO, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

auto2014 <- auto2014 %>%
  select(-n)

auto2014 <- auto2014 %>%
  pivot_wider(names_from = DS_COR_RACA, values_from = perc)

hetero2014 <- HC.TSE2014 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  group_by(DS_GENERO, Cor.final2) %>%
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
  select(DS_GENERO...1, "Brancos(as)","Pardos(as)", "Pretos(as)")




#2018
auto2018 <- HC.TSE2018 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  group_by(DS_GENERO, DS_COR_RACA) %>%
  summarise(n=n()) %>%
  mutate(perc = round(prop.table(n)*100, digits = 1))

auto2018 <- auto2018 %>%
  select(-n)

auto2018 <- auto2018 %>%
  pivot_wider(names_from = DS_COR_RACA, values_from = perc)

hetero2018 <- HC.TSE2018 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  group_by(DS_GENERO, Cor.final2) %>%
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
  select(DS_GENERO...1, "Brancos(as)","Pardos(as)", "Pretos(as)")



#2022

auto2022 <- HC.TSE2022 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_COR_RACA != "NÃO DIVULGÁVEL") %>%
  filter(DS_COR_RACA != "NÃO INFORMADO") %>%
  group_by(DS_GENERO, DS_COR_RACA) %>%
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
  filter(DS_GENERO != "NÃO DIVULGÁVEL") %>%
  group_by(DS_GENERO, Cor.final2) %>%
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
  select(DS_GENERO...1, "Brancos(as)","Pardos(as)", "Pretos(as)")



# Juntando tudo

junto2014 <- junto2014 %>%
  mutate(Ano = 2014)

junto2018 <- junto2018 %>%
  mutate(Ano = 2018)

junto2022 <- junto2022 %>%
  mutate(Ano = 2022)

tudojunto <- rbind(junto2014, junto2018, junto2022)

tudojunto <- tudojunto %>% pivot_longer(2:4, names_to = "Cor")

# Gráfico
tudojunto <- tudojunto %>%
  mutate(DS_GENERO...1 = str_to_title(DS_GENERO...1))

tudojunto$DS_GENERO...1 <- factor(tudojunto$DS_GENERO...1,
                                  levels = c("Masculino", "Feminino"))

tudojunto <- tudojunto %>%
  rename(DS_GENERO = DS_GENERO...1)

tudojunto %>%
  mutate(Cor2 = case_when(
    Cor == "Brancos(as)" ~ "Branca",
    Cor == "Pardos(as)" ~ "Parda",
    Cor == "Pretos(as)" ~ "Preta"
  )) %>%
  ggplot(aes(x = Cor2, y = value, fill = factor(Ano))) +
  geom_bar(stat = "identity", position = "dodge2") +
  facet_wrap(vars(DS_GENERO)) +
  theme_minimal() +
  scale_fill_manual(
    values = c("#fc8d59","#1f78b4", "#b2df8a")
  ) +
  geom_text(aes(label = gsub("\\.", ",", as.character(round(value, 0))), vjust = ifelse(value >= 0, -0.5, 1.5)), size = 4, position = position_dodge(0.9)) +
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


ggsave("Diferença Hetero-Auto Gênero.png",bg = "white", dpi = 300, width = 8, height = 6)
