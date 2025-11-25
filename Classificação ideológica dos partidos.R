
# Classificação ideológica partidos

# 2014

HC.TSE2014 <- HC.TSE2014 %>%
  mutate(Ideologia.partido = case_when(
    SG_PARTIDO == "PTC" ~ "Direita",
    SG_PARTIDO == "DEM" ~ "Direita",
    SG_PARTIDO == "PT do B" ~ "Centro",
    SG_PARTIDO == "PPS" ~ "Centro",
    SG_PARTIDO == "PSDC" ~ "Direita",
    SG_PARTIDO == "PMDB" ~ "Centro",
    SG_PARTIDO == "NOVO" ~ "Direita",
    SG_PARTIDO == "PATRIOTA" ~ "Direita",
    SG_PARTIDO == "PC do B" ~ "Esquerda",
    SG_PARTIDO == "PCB" ~ "Esquerda",
    SG_PARTIDO == "PCO" ~ "Esquerda",
    SG_PARTIDO == "PDT" ~ "Esquerda",
    SG_PARTIDO == "PR" ~ "Direita",
    SG_PARTIDO == "PHS" ~ "Centro",
    SG_PARTIDO == "PMN" ~ "Centro",
    SG_PARTIDO == "PTN" ~ "Direita",
    SG_PARTIDO == "PP" ~ "Direita",
    SG_PARTIDO == "PPL" ~ "Direita",
    SG_PARTIDO == "PROS" ~ "Direita",
    SG_PARTIDO == "PRP" ~ "Direita",
    SG_PARTIDO == "PRTB" ~ "Direita",
    SG_PARTIDO == "PSB" ~ "Esquerda",
    SG_PARTIDO == "PSC" ~ "Direita",
    SG_PARTIDO == "PSD" ~ "Direita",
    SG_PARTIDO == "PSDB" ~ "Centro",
    SG_PARTIDO == "PSL" ~ "Direita",
    SG_PARTIDO == "PSOL" ~ "Esquerda",
    SG_PARTIDO == "PSTU" ~ "Esquerda",
    SG_PARTIDO == "PT" ~ "Esquerda",
    SG_PARTIDO == "PTB" ~ "Direita",
    SG_PARTIDO == "PV" ~ "Centro",
    SG_PARTIDO == "REDE" ~ "Centro",
    SG_PARTIDO == "PRB" ~ "Direita",
    SG_PARTIDO == "SD" ~ "Centro"
  ))

# 2018

HC.TSE2018 <- HC.TSE2018 %>%
  mutate(Ideologia.partido = case_when(
    SG_PARTIDO == "PTC" ~ "Direita",
    SG_PARTIDO == "DEM" ~ "Direita",
    SG_PARTIDO == "AVANTE" ~ "Centro",
    SG_PARTIDO == "PPS" ~ "Centro",
    SG_PARTIDO == "DC" ~ "Direita",
    SG_PARTIDO == "MDB" ~ "Centro",
    SG_PARTIDO == "NOVO" ~ "Direita",
    SG_PARTIDO == "PATRIOTA" ~ "Direita",
    SG_PARTIDO == "PC do B" ~ "Esquerda",
    SG_PARTIDO == "PCB" ~ "Esquerda",
    SG_PARTIDO == "PCO" ~ "Esquerda",
    SG_PARTIDO == "PDT" ~ "Esquerda",
    SG_PARTIDO == "PHS" ~ "Centro",
    SG_PARTIDO == "PR" ~ "Direita",
    SG_PARTIDO == "PMB" ~ "Centro",
    SG_PARTIDO == "PMN" ~ "Centro",
    SG_PARTIDO == "PODE" ~ "Direita",
    SG_PARTIDO == "PP" ~ "Direita",
    SG_PARTIDO == "PPL" ~ "Direita",
    SG_PARTIDO == "PROS" ~ "Direita",
    SG_PARTIDO == "PRP" ~ "Direita",
    SG_PARTIDO == "PRTB" ~ "Direita",
    SG_PARTIDO == "PSB" ~ "Esquerda",
    SG_PARTIDO == "PSC" ~ "Direita",
    SG_PARTIDO == "PSD" ~ "Direita",
    SG_PARTIDO == "PSDB" ~ "Direita",
    SG_PARTIDO == "PSL" ~ "Direita",
    SG_PARTIDO == "PSOL" ~ "Esquerda",
    SG_PARTIDO == "PSTU" ~ "Esquerda",
    SG_PARTIDO == "PT" ~ "Esquerda",
    SG_PARTIDO == "PTB" ~ "Direita",
    SG_PARTIDO == "PV" ~ "Centro",
    SG_PARTIDO == "REDE" ~ "Centro",
    SG_PARTIDO == "PRB" ~ "Direita",
    SG_PARTIDO == "SOLIDARIEDADE" ~ "Centro"
  ))

# 2022

HC.TSE2022 <- HC.TSE2022 %>%
  mutate(Ideologia.partido = case_when(
    SG_PARTIDO == "AGIR" ~ "Direita",
    SG_PARTIDO == "AVANTE" ~ "Centro",
    SG_PARTIDO == "CIDADANIA" ~ "Centro",
    SG_PARTIDO == "DC" ~ "Direita",
    SG_PARTIDO == "MDB" ~ "Centro",
    SG_PARTIDO == "NOVO" ~ "Direita",
    SG_PARTIDO == "PATRIOTA" ~ "Direita",
    SG_PARTIDO == "PC do B" ~ "Esquerda",
    SG_PARTIDO == "PCB" ~ "Esquerda",
    SG_PARTIDO == "PCO" ~ "Esquerda",
    SG_PARTIDO == "PDT" ~ "Esquerda",
    SG_PARTIDO == "PL" ~ "Direita",
    SG_PARTIDO == "PMB" ~ "Centro",
    SG_PARTIDO == "PMN" ~ "Centro",
    SG_PARTIDO == "PODE" ~ "Direita",
    SG_PARTIDO == "PP" ~ "Direita",
    SG_PARTIDO == "PROS" ~ "Direita",
    SG_PARTIDO == "PRTB" ~ "Direita",
    SG_PARTIDO == "PSB" ~ "Esquerda",
    SG_PARTIDO == "PSC" ~ "Direita",
    SG_PARTIDO == "PSD" ~ "Direita",
    SG_PARTIDO == "PSDB" ~ "Centro",
    SG_PARTIDO == "PSOL" ~ "Esquerda",
    SG_PARTIDO == "PSTU" ~ "Esquerda",
    SG_PARTIDO == "PT" ~ "Esquerda",
    SG_PARTIDO == "PTB" ~ "Direita",
    SG_PARTIDO == "PV" ~ "Centro",
    SG_PARTIDO == "REDE" ~ "Centro",
    SG_PARTIDO == "REPUBLICANOS" ~ "Direita",
    SG_PARTIDO == "SOLIDARIEDADE" ~ "Centro",
    SG_PARTIDO == "UNIÃO" ~ "Direita",
    SG_PARTIDO == "UP" ~ "Esquerda"
  ))


