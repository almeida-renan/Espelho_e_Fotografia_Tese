
# Correlação Candidatos e Eleitos por UF

# Este script contém o passo a passo para gerar o gráfico 5.6, que mostra
# a relação entre o percentual de candidatos e eleitos não-brancos por UF
# para os dois métodos de classificação, nas eleições de 2014, 2018 e 2022.

# O script também calcula coeficientes de correlação (Pearson, Spearman e Kendall)
# entre candidatos e eleitos não-brancos segundo os dois métodos de classificação,
# nas três eleições mencionadas. 


# 2014 ----

# Autodeclaração

#Cand.
auto14 <- HC.TSE2014 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  janitor::tabyl(SG_UF, DS_COR_RACA) %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F)

#Eleitos
auto14.eleitos <- HC.TSE2014 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  janitor::tabyl(SG_UF, DS_COR_RACA) %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F)

# Heteroclassificação

#Cand.
hetero14 <- HC.TSE2014 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  janitor::tabyl(SG_UF, Cor.final2) %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F) %>%
  select(-AMARELA, -INDÍGENA)

#Eleitos
hetero14.eleitos <- HC.TSE2014 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  janitor::tabyl(SG_UF, Cor.final2) %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F) %>%
  select(-AMARELA, -INDÍGENA)

# Convertendo tudo para número

auto14$BRANCA <- as.double(auto14$BRANCA)
auto14$PARDA <- as.double(auto14$PARDA)
auto14$PRETA <- as.double(auto14$PRETA)

hetero14$BRANCA <- as.double(hetero14$BRANCA)
hetero14$PARDA <- as.double(hetero14$PARDA)
hetero14$PRETA <- as.double(hetero14$PRETA)

auto14.eleitos$BRANCA <- as.double(auto14.eleitos$BRANCA)
auto14.eleitos$PARDA <- as.double(auto14.eleitos$PARDA)
auto14.eleitos$PRETA <- as.double(auto14.eleitos$PRETA)

hetero14.eleitos$BRANCA <- as.double(hetero14.eleitos$BRANCA)
hetero14.eleitos$PARDA <- as.double(hetero14.eleitos$PARDA)
hetero14.eleitos$PRETA <- as.double(hetero14.eleitos$PRETA)

#Renomeando colunas dos eleitos

auto14.eleitos <- auto14.eleitos %>%
  rename(Bra.Eleitos = BRANCA,
         Par.Eleitos = PARDA,
         Pre.Eleitos = PRETA)

hetero14.eleitos <- hetero14.eleitos %>%
  rename(Bra.Eleitos = BRANCA,
         Par.Eleitos = PARDA,
         Pre.Eleitos = PRETA)

# Juntando candidatos e eleitos

#Auto
cand.eleitos.auto14 <- cbind(auto14, auto14.eleitos)

cand.eleitos.auto14 <- cand.eleitos.auto14[c(1,2,3,4,6,7,8)]

cand.eleitos.auto14 <- cand.eleitos.auto14 %>%
  mutate(Nao.Brancos = PARDA + PRETA,
         Não.Brancos.Eleitos = Par.Eleitos + Pre.Eleitos)

#Hetero
cand.eleitos.hetero14 <- cbind(hetero14, hetero14.eleitos)

cand.eleitos.hetero14 <- cand.eleitos.hetero14[c(1,2,3,4,6,7,8)]

cand.eleitos.hetero14 <- cand.eleitos.hetero14 %>%
  mutate(Nao.Brancos = PARDA + PRETA,
         Não.Brancos.Eleitos = Par.Eleitos + Pre.Eleitos)

# Juntando auto e hetero


cand.eleitos.auto14 <- cand.eleitos.auto14 %>%
  mutate(classification = "Autodeclaração",
         Ano = 2014)

cand.eleitos.hetero14 <- cand.eleitos.hetero14 %>%
  mutate(classification = "Heteroclassificação",
         Ano = 2014)

CE.autohetero14 <- rbind(cand.eleitos.auto14, cand.eleitos.hetero14)

# 2018 ----

# Autodeclaração

#Cand.
auto18 <- HC.TSE2018 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  janitor::tabyl(SG_UF, DS_COR_RACA) %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F)

#Eleitos
auto18.eleitos <- HC.TSE2018 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  janitor::tabyl(SG_UF, DS_COR_RACA) %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F)

# Heteroclassificação

#Cand.
hetero18 <- HC.TSE2018 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  janitor::tabyl(SG_UF, Cor.final2) %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F) %>%
  select(-AMARELA, -INDÍGENA)

#Eleitos
hetero18.eleitos <- HC.TSE2018 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  janitor::tabyl(SG_UF, Cor.final2) %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F) %>%
  select(-AMARELA, -INDÍGENA)

# Convertendo tudo para número

auto18$BRANCA <- as.double(auto18$BRANCA)
auto18$PARDA <- as.double(auto18$PARDA)
auto18$PRETA <- as.double(auto18$PRETA)

hetero18$BRANCA <- as.double(hetero18$BRANCA)
hetero18$PARDA <- as.double(hetero18$PARDA)
hetero18$PRETA <- as.double(hetero18$PRETA)

auto18.eleitos$BRANCA <- as.double(auto18.eleitos$BRANCA)
auto18.eleitos$PARDA <- as.double(auto18.eleitos$PARDA)
auto18.eleitos$PRETA <- as.double(auto18.eleitos$PRETA)

hetero18.eleitos$BRANCA <- as.double(hetero18.eleitos$BRANCA)
hetero18.eleitos$PARDA <- as.double(hetero18.eleitos$PARDA)
hetero18.eleitos$PRETA <- as.double(hetero18.eleitos$PRETA)

#Renomeando colunas dos eleitos

auto18.eleitos <- auto18.eleitos %>%
  rename(Bra.Eleitos = BRANCA,
         Par.Eleitos = PARDA,
         Pre.Eleitos = PRETA)

hetero18.eleitos <- hetero18.eleitos %>%
  rename(Bra.Eleitos = BRANCA,
         Par.Eleitos = PARDA,
         Pre.Eleitos = PRETA)

# Juntando candidatos e eleitos

#Auto
cand.eleitos.auto18 <- cbind(auto18, auto18.eleitos)

cand.eleitos.auto18 <- cand.eleitos.auto18[c(1,2,3,4,6,7,8)]

cand.eleitos.auto18 <- cand.eleitos.auto18 %>%
  mutate(Nao.Brancos = PARDA + PRETA,
         Não.Brancos.Eleitos = Par.Eleitos + Pre.Eleitos)

#Hetero
cand.eleitos.hetero18 <- cbind(hetero18, hetero18.eleitos)

cand.eleitos.hetero18 <- cand.eleitos.hetero18[c(1,2,3,4,6,7,8)]

cand.eleitos.hetero18 <- cand.eleitos.hetero18 %>%
  mutate(Nao.Brancos = PARDA + PRETA,
         Não.Brancos.Eleitos = Par.Eleitos + Pre.Eleitos)

# Juntando auto e hetero


cand.eleitos.auto18 <- cand.eleitos.auto18 %>%
  mutate(classification = "Autodeclaração",
         Ano = 2018)

cand.eleitos.hetero18 <- cand.eleitos.hetero18 %>%
  mutate(classification = "Heteroclassificação",
         Ano = 2018)

CE.autohetero18 <- rbind(cand.eleitos.auto18, cand.eleitos.hetero18)

# 2022 ----

# Autodeclaração

#Cand.
auto22 <- HC.TSE2022 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_COR_RACA != "NÃO DIVULGÁVEL") %>%
  filter(DS_COR_RACA != "NÃO INFORMADO") %>%
  janitor::tabyl(SG_UF, DS_COR_RACA) %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F)

#Eleitos
auto22.eleitos <- HC.TSE2022 %>%
  filter(DS_COR_RACA != "AMARELA") %>%
  filter(DS_COR_RACA != "INDÍGENA") %>%
  filter(DS_COR_RACA != "NÃO DIVULGÁVEL") %>%
  filter(DS_COR_RACA != "NÃO INFORMADO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  janitor::tabyl(SG_UF, DS_COR_RACA) %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F)

# Heteroclassificação

#Cand.
hetero22 <- HC.TSE2022 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(DS_COR_RACA != "NÃO DIVULGÁVEL") %>%
  filter(DS_COR_RACA != "NÃO INFORMADO") %>%
  janitor::tabyl(SG_UF, Cor.final2) %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F) %>%
  select(-AMARELA, -INDÍGENA, -`NÃO INFORMADO`)

#Eleitos
hetero22.eleitos <- HC.TSE2022 %>%
  filter(Cor.final2 != "AMARELA") %>%
  filter(Cor.final2 != "INDÍGENA") %>%
  filter(DS_COR_RACA != "NÃO DIVULGÁVEL") %>%
  filter(DS_COR_RACA != "NÃO INFORMADO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA"|
           DS_SIT_TOT_TURNO == "ELEITO POR QP")  %>%
  janitor::tabyl(SG_UF, Cor.final2) %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting(digits = 1, affix_sign = F) %>%
  select(-AMARELA, -INDÍGENA, -`NÃO INFORMADO`)

# Convertendo tudo para número

auto22$BRANCA <- as.double(auto22$BRANCA)
auto22$PARDA <- as.double(auto22$PARDA)
auto22$PRETA <- as.double(auto22$PRETA)

hetero22$BRANCA <- as.double(hetero22$BRANCA)
hetero22$PARDA <- as.double(hetero22$PARDA)
hetero22$PRETA <- as.double(hetero22$PRETA)

auto22.eleitos$BRANCA <- as.double(auto22.eleitos$BRANCA)
auto22.eleitos$PARDA <- as.double(auto22.eleitos$PARDA)
auto22.eleitos$PRETA <- as.double(auto22.eleitos$PRETA)

hetero22.eleitos$BRANCA <- as.double(hetero22.eleitos$BRANCA)
hetero22.eleitos$PARDA <- as.double(hetero22.eleitos$PARDA)
hetero22.eleitos$PRETA <- as.double(hetero22.eleitos$PRETA)

#Renomeando colunas dos eleitos

auto22.eleitos <- auto22.eleitos %>%
  rename(Bra.Eleitos = BRANCA,
         Par.Eleitos = PARDA,
         Pre.Eleitos = PRETA)

hetero22.eleitos <- hetero22.eleitos %>%
  rename(Bra.Eleitos = BRANCA,
         Par.Eleitos = PARDA,
         Pre.Eleitos = PRETA)

# Juntando candidatos e eleitos

#Auto
cand.eleitos.auto22 <- cbind(auto22, auto22.eleitos)

cand.eleitos.auto22 <- cand.eleitos.auto22[c(1,2,3,4,6,7,8)]

cand.eleitos.auto22 <- cand.eleitos.auto22 %>%
  mutate(Nao.Brancos = PARDA + PRETA,
         Não.Brancos.Eleitos = Par.Eleitos + Pre.Eleitos)

#Hetero
cand.eleitos.hetero22 <- cbind(hetero22, hetero22.eleitos)

cand.eleitos.hetero22 <- cand.eleitos.hetero22[c(1,2,3,4,6,7,8)]

cand.eleitos.hetero22 <- cand.eleitos.hetero22 %>%
  mutate(Nao.Brancos = PARDA + PRETA,
         Não.Brancos.Eleitos = Par.Eleitos + Pre.Eleitos)

# Juntando auto e hetero


cand.eleitos.auto22 <- cand.eleitos.auto22 %>%
  mutate(classification = "Autodeclaração",
         Ano = 2022)

cand.eleitos.hetero22 <- cand.eleitos.hetero22 %>%
  mutate(classification = "Heteroclassificação",
         Ano = 2022)

CE.autohetero22 <- rbind(cand.eleitos.auto22, cand.eleitos.hetero22)

# Juntando todos os anos ----

CE.tudo <- rbind(CE.autohetero14, CE.autohetero18, CE.autohetero22)

# Gráfico

# Repelindo rótulos sem ggrepel (usar esse)
CE.tudo %>%
  ggplot(aes(x = Nao.Brancos, y = Não.Brancos.Eleitos, label = SG_UF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_text(vjust = -0.5, size = 4, check_overlap = T) +
  facet_grid(classification ~ Ano) +
  theme_minimal() +
  expand_limits(y = max(CE.tudo$Não.Brancos.Eleitos) * 1.1) +
  labs(x = "Percentual de Candidatos Não-brancos", y = "Percentual de Eleitos Não-Brancos") +
  theme(strip.text = element_text(size = 13),
    legend.text = element_text(size = 13),
    axis.text.x = element_text(size = 11),  # Tamanho do título do eixo x
    axis.text.y = element_text(size = 11))


#Repelindo rótulos com ggrepel (não usar esse)
CE.tudo %>%
  ggplot(aes(x = Nao.Brancos, y = Não.Brancos.Eleitos, label = SG_UF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  facet_grid(classification ~ Ano) +
  theme_minimal() +
  ggrepel::geom_text_repel(vjust = -0.5, size = 4)



ggsave("Correlação Cand. e Eleitos UF.png",bg = "white", dpi = 300, width = 8, height = 6)


# Testes de correlação

#Auto
cor.test(cand.eleitos.auto14$Nao.Brancos, cand.eleitos.auto14$Não.Brancos.Eleitos, method = "pearson")
cor.test(cand.eleitos.auto14$Nao.Brancos, cand.eleitos.auto14$Não.Brancos.Eleitos, method = "spearman")
cor.test(cand.eleitos.auto14$Nao.Brancos, cand.eleitos.auto14$Não.Brancos.Eleitos, method = "kendall")

cor.test(cand.eleitos.auto18$Nao.Brancos, cand.eleitos.auto18$Não.Brancos.Eleitos, method = "pearson")
cor.test(cand.eleitos.auto18$Nao.Brancos, cand.eleitos.auto18$Não.Brancos.Eleitos, method = "spearman")
cor.test(cand.eleitos.auto18$Nao.Brancos, cand.eleitos.auto18$Não.Brancos.Eleitos, method = "kendall")

cor.test(cand.eleitos.auto22$Nao.Brancos, cand.eleitos.auto22$Não.Brancos.Eleitos, method = "pearson")
cor.test(cand.eleitos.auto22$Nao.Brancos, cand.eleitos.auto22$Não.Brancos.Eleitos, method = "spearman")
cor.test(cand.eleitos.auto22$Nao.Brancos, cand.eleitos.auto22$Não.Brancos.Eleitos, method = "kendall")

#Hetero
cor.test(cand.eleitos.hetero14$Nao.Brancos, cand.eleitos.hetero14$Não.Brancos.Eleitos, method = "pearson")
cor.test(cand.eleitos.hetero14$Nao.Brancos, cand.eleitos.hetero14$Não.Brancos.Eleitos, method = "spearman")
cor.test(cand.eleitos.hetero14$Nao.Brancos, cand.eleitos.hetero14$Não.Brancos.Eleitos, method = "kendall")

cor.test(cand.eleitos.hetero18$Nao.Brancos, cand.eleitos.hetero18$Não.Brancos.Eleitos, method = "pearson")
cor.test(cand.eleitos.hetero18$Nao.Brancos, cand.eleitos.hetero18$Não.Brancos.Eleitos, method = "spearman")
cor.test(cand.eleitos.hetero18$Nao.Brancos, cand.eleitos.hetero18$Não.Brancos.Eleitos, method = "kendall")

cor.test(cand.eleitos.hetero22$Nao.Brancos, cand.eleitos.hetero22$Não.Brancos.Eleitos, method = "pearson")
cor.test(cand.eleitos.hetero22$Nao.Brancos, cand.eleitos.hetero22$Não.Brancos.Eleitos, method = "spearman")
cor.test(cand.eleitos.hetero22$Nao.Brancos, cand.eleitos.hetero22$Não.Brancos.Eleitos, method = "kendall")

# Shapiro Wilk normalidade dos resíduos 
# Mudar ano e método de classificação

modelo <- lm(Não.Brancos.Eleitos ~ Nao.Brancos, data = cand.eleitos.hetero22)
shapiro.test(resid(modelo))


# Testando normalidade das variáveis

shapiro.test(cand.eleitos.auto14$Nao.Brancos)
shapiro.test(cand.eleitos.auto14$Não.Brancos.Eleitos)

shapiro.test(cand.eleitos.auto18$Nao.Brancos)
shapiro.test(cand.eleitos.auto18$Não.Brancos.Eleitos)

shapiro.test(cand.eleitos.auto22$Nao.Brancos)
shapiro.test(cand.eleitos.auto22$Não.Brancos.Eleitos)

shapiro.test(cand.eleitos.hetero14$Nao.Brancos)
shapiro.test(cand.eleitos.hetero14$Não.Brancos.Eleitos)

shapiro.test(cand.eleitos.hetero18$Nao.Brancos)
shapiro.test(cand.eleitos.hetero18$Não.Brancos.Eleitos)

shapiro.test(cand.eleitos.hetero22$Nao.Brancos)
shapiro.test(cand.eleitos.hetero22$Não.Brancos.Eleitos)