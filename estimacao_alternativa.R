########### Estimações alternativas de Event Study para os grupos de tratamento e controle criados ######

rm(list = ls())

##### Abrindo bibliotecas #####

library(tidyverse)
library(rio)
library(fixest)
library(broom)
library(geobr)
library(sf)
library(MatchIt)

##### Abrindo bases de dados #####

base_matching <- import('base_pareamento.dbf')
base_linhas_futuras <- import('base_linhas_futuras.dbf')
base_cptm <- import('base_cptm.dbf')

##### Fazendo novos Event Study para as dummies de modal: metro, carro e ônibus #####

#### Grupo 1: Linhas Futuras ####

base_reg_linhas <- base_linhas_futuras |>
  mutate(
    post = if_else(ano >= 2018, 1, 0)
  )

event_study_linhas_metro <- feols(
  metro ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

event_study_linhas_carro <- feols(
  carro ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

event_study_linhas_onibus <- feols(
  onibus ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

### Organizando os resultados ###

lista_event_study_linhas <- list(
  'Metrô' = event_study_linhas_metro,
  'Carro' = event_study_linhas_carro,
  'Ônibus' = event_study_linhas_onibus
)

tabela_result_es_linhas_modal <- etable(
  lista_event_study_linhas,
  tex = FALSE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Event Study - Grupo de Controle Linhas Futuras'
)

#### Grupo 2: CPTM ####

base_reg_cptm <- base_cptm |>
  mutate(
    post = if_else(ano >= 2018, 1, 0)
  )

event_study_cptm_metro <- feols(
  metro ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

event_study_cptm_carro <- feols(
  carro ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

event_study_cptm_onibus <- feols(
  onibus ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

### Organizando os resultados ###

lista_event_study_cptm <- list(
  'Metrô' = event_study_cptm_metro,
  'Carro' = event_study_cptm_carro,
  'Ônibus' = event_study_cptm_onibus
)

tabela_result_es_cptm <- etable(
  lista_event_study_cptm,
  tex = FALSE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Event Study - Grupo de Controle CPTM'
)

#### Grupo 3: Pareado ####

base_reg_matching <- base_matching |>
  mutate(
    post = if_else(ano >= 2018, 1, 0)
  )

event_study_matching_metro <- feols(
  metro ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,

  cluster = ~ZMC
)

event_study_matching_carro <- feols(
  carro ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,

  cluster = ~ZMC
)

event_study_matching_onibus <- feols(
  onibus ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,

  cluster = ~ZMC
)

### Obtendo os resultados pré-tratamento para os controles ###

y_medio_pre_match_metro <- base_reg_matching |>
  filter(tratamento == 1 & ano == 2017) |>
  summarise(media_pre = round(mean(metro, na.rm = TRUE), 4))

y_medio_pre_match_onibus <- base_reg_matching |>
  filter(tratamento == 1 & ano == 2017) |>
  summarise(media_pre = round(mean(onibus, na.rm = TRUE), 4))

y_medio_pre_match_carro <- base_reg_matching |>
  filter(tratamento == 1 & ano == 2017) |>
  summarise(media_pre = round(mean(carro, na.rm = TRUE), 4))

y_medio_pre_cptm_metro <- base_reg_cptm |>
  filter(tratamento == 1 & ano == 2017) |>
  summarise(media_pre = round(mean(metro, na.rm = TRUE), 4))

y_medio_pre_cptm_onibus <- base_reg_cptm |>
  filter(tratamento == 1 & ano == 2017) |>
  summarise(media_pre = round(mean(onibus, na.rm = TRUE), 4))

y_medio_pre_cptm_carro <- base_reg_cptm |>
  filter(tratamento == 1 & ano == 2017) |>
  summarise(media_pre = round(mean(carro, na.rm = TRUE), 4))

y_medio_pre_linhas_metro <- base_reg_linhas |>
  filter(tratamento == 1 & ano == 2017) |>
  summarise(media_pre = round(mean(metro, na.rm = TRUE), 4))

y_medio_pre_linhas_onibus <- base_reg_linhas |>
  filter(tratamento == 1 & ano == 2017) |>
  summarise(media_pre = round(mean(onibus, na.rm = TRUE), 4))

y_medio_pre_linhas_carro <- base_reg_linhas |>
  filter(tratamento == 1 & ano == 2017) |>
  summarise(media_pre = round(mean(carro, na.rm = TRUE), 4))

### Obtendo os efeitos relativos das estimações ###

## Grupo Pareado ##

y_pre_match_metro <- y_medio_pre_match_metro |>
  pull(media_pre)

coef_match_metro <- round(
  coef(event_study_matching_metro)['ano::2023:tratamento'],
  4
)

efeito_relativo_match_metro <- round(
  (coef_match_metro / y_pre_match_metro) * 100,
  2
)

y_pre_match_onibus <- y_medio_pre_match_onibus |>
  pull(media_pre)

coef_match_onibus <- round(
  coef(event_study_matching_onibus)['ano::2023:tratamento'],
  4
)

efeito_relativo_match_onibus <- round(
  (coef_match_onibus / y_pre_match_onibus) * 100,
  2
)

y_pre_match_carro <- y_medio_pre_match_carro |>
  pull(media_pre)

coef_match_carro <- round(
  coef(event_study_matching_carro)['ano::2023:tratamento'],
  4
)

efeito_relativo_match_carro <- round(
  (coef_match_carro / y_pre_match_carro) * 100,
  2
)

## Grupo CPTM ##

y_pre_cptm_metro <- y_medio_pre_cptm_metro |>
  pull(media_pre)

coef_cptm_metro <- round(
  coef(event_study_cptm_metro)['ano::2023:tratamento'],
  4
)

efeito_relativo_cptm_metro <- round(
  (coef_cptm_metro / y_pre_cptm_metro) * 100,
  2
)

y_pre_cptm_onibus <- y_medio_pre_cptm_onibus |>
  pull(media_pre)

coef_cptm_onibus <- round(
  coef(event_study_cptm_onibus)['ano::2023:tratamento'],
  4
)

efeito_relativo_cptm_onibus <- round(
  (coef_cptm_onibus / y_pre_cptm_onibus) * 100,
  2
)

y_pre_cptm_carro <- y_medio_pre_cptm_carro |>
  pull(media_pre)

coef_cptm_carro <- round(
  coef(event_study_cptm_carro)['ano::2023:tratamento'],
  4
)

efeito_relativo_cptm_carro <- round(
  (coef_cptm_carro / y_pre_cptm_carro) * 100,
  2
)

## Grupo Linhas Futuras ##

y_pre_linhas_metro <- y_medio_pre_linhas_metro |>
  pull(media_pre)

coef_linhas <- round(coef(event_study_linhas_metro)['ano::2023:tratamento'], 4)

efeito_relativo_linhas_metro <- round(
  (coef_linhas / y_pre_linhas_metro) * 100,
  2
)

y_pre_linhas_onibus <- y_medio_pre_linhas_onibus |>
  pull(media_pre)

coef_linhas_onibus <- round(
  coef(event_study_linhas_onibus)['ano::2023:tratamento'],
  4
)

efeito_relativo_linhas_onibus <- round(
  (coef_linhas_onibus / y_pre_linhas_onibus) * 100,
  2
)

y_pre_linhas_carro <- y_medio_pre_linhas_carro |>
  pull(media_pre)

coef_linhas_carro <- round(
  coef(event_study_linhas_carro)['ano::2023:tratamento'],
  4
)

efeito_relativo_linhas_carro <- round(
  (coef_linhas_carro / y_pre_linhas_carro) * 100,
  2
)

### Organizando os resultados ###

lista_tabela_inicial <- list(
  'Grupo de controle pareado' = event_study_matching_metro,
  'Grupo de controle CPTM' = event_study_cptm_metro,
  'Grupo de controle linhas futuras' = event_study_linhas_metro
)

linhas_extras <- list(
  "Média Pré-Tratamento sobre Tratados" = c(
    y_medio_pre_match_metro$media_pre,
    y_medio_pre_cptm_metro$media_pre,
    y_medio_pre_linhas_metro$media_pre
  ),
  "Efeito relativo (%)" = c(
    efeito_relativo_match_metro,
    efeito_relativo_cptm_metro,
    efeito_relativo_linhas_metro
  )
)

tabela_result_es_inicial <- etable(
  lista_tabela_inicial,
  tex = FALSE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados para a utilização do Metrô - Event Study',
  extralines = linhas_extras
)

tabela_result_es_inicial_latex <- etable(
  lista_tabela_inicial,
  tex = TRUE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados para a utilização do Metrô - Event Study',
  extralines = linhas_extras
)

print(tabela_result_es_inicial_latex)

##### Criando tabela completa dos resultados para o grupo de controle Linhas Futuras #####

lista_original <- readRDS("lista_modelos_originais.rds")

lista_completa_linhas <- list(
  'Indicador Destino' = lista_original$`Destino Centro Expandido`,
  'Viagem Geral' = lista_original$`Viagem Geral`,
  'Trabalho' = lista_original$`Trabalho Centro Expandido`,
  'Educação' = lista_original$`Educação Centro Expandido`,
  'Buscar Emprego' = lista_original$`Buscar Emprego Centro Expandido`,
  'Trab/Educ/Emp' = lista_original$`Trabalho ou Educação ou Buscar Emprego Centro Expandido`,
  'Laz/Comp/Saúde' = lista_original$`Lazer ou Compras ou Saúde Centro Expandido`,

  'Metrô' = event_study_linhas_metro,
  'Ônibus' = event_study_linhas_onibus,
  'Carro' = event_study_linhas_carro
)


lista_extra_completa <- list(
  "Média Pré-Tratamento sobre Tratados" = c(
    lista_original$`Média Pré-Tratamento`[1],
    lista_original$`Média Pré-Tratamento`[2],
    lista_original$`Média Pré-Tratamento`[3],
    lista_original$`Média Pré-Tratamento`[4],
    lista_original$`Média Pré-Tratamento`[8],
    lista_original$`Média Pré-Tratamento`[9],
    lista_original$`Média Pré-Tratamento`[12],
    y_medio_pre_linhas_metro$media_pre,
    y_medio_pre_linhas_onibus$media_pre,
    y_medio_pre_linhas_carro$media_pre
  ),
  "Efeito relativo (%)" = c(
    lista_original[[9]][c(1, 2, 3, 4, 8, 9, 12)],
    efeito_relativo_linhas_metro,
    efeito_relativo_linhas_onibus,
    efeito_relativo_linhas_carro
  )
)

tabela_result_es <- etable(
  lista_completa_linhas,
  tex = FALSE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados para a utilização do Metrô - Event Study - Grupo de Controle Linhas Futuras',
  extralines = lista_extra_completa
)

tabela_result_es_latex <- etable(
  lista_completa_linhas,
  tex = TRUE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados para a utilização do Metrô - Event Study - Grupo de Controle Linhas Futuras',
  extralines = lista_extra_completa
)

print(tabela_result_es_latex)

###### ANÁLISES DE ROBUSTEZ ######

##### Criando grupo de tratamento Spillover para o grupo de linhas futuras #####

zmc_polygons <- st_read("zmc_polygons.gpkg") |>
  st_make_valid()

#### Pegando somente as ZMC's tratadas e seus polígonos ####

zmcs_tratadas <- base_reg_linhas |>
  filter(tratamento == 1) |>
  pull(ZMC) |>
  unique()

poligonos_tratados <- zmc_polygons |>
  filter(ZMC %in% zmcs_tratadas)

#### Encontrando e mantendo somente os vizinhos das ZMC's tratadas ####

vizinhos_zmc <- st_is_within_distance(
  poligonos_tratados,
  zmc_polygons,
  dist = 50
)

indices_unicos <- unique(unlist(vizinhos_zmc))

zmcs_vizinhas <- zmc_polygons |>
  slice(indices_unicos) |>
  pull(ZMC) |>
  setdiff(zmcs_tratadas)

#### Atualizando a base antiga - filtragem do grupo precisa ser feita de novo ####

od_completa <- import('od_base_completa.dbf') |>
  rename(
    trab_cen = trab_centr,
    educ_cen = educ_centr,
    saude_cen = saude_cent,
    lazer_cen = lazer_cent,
    comp_cen = compras_ce,
    emp_cen = emp_centro,
    trabeducemp = trab_educ_,
    trabemp = trab_emp_c,
    lazcompsa = lazer_comp,
    lazcomp = lazer_comp.1,
    trabeduc = trab_educ_.1
  ) |>
  mutate(
    tipo_grupo_exp = case_when(
      tipo_grupo == 'Tratamento' ~ 'Tratamento_Nucleo',
      ZMC %in% zmcs_vizinhas ~ 'Tratamento_Vizinho',
      TRUE ~ tipo_grupo
    )
  )

base_robustez_linhas <- od_completa |>
  filter(tipo_grupo_exp != 'Morador_Centro_Exp') |>
  filter(tipo_grupo_exp != 'Candidatos_Controle_MatchIt') |>
  filter(tipo_grupo_exp != 'Controle_CPTM') |>
  mutate(
    tratamento = ifelse(tipo_grupo_exp == 'Tratamento_Nucleo', 1, 0),
    tratamento2 = ifelse(tipo_grupo_exp == 'Tratamento_Vizinho', 1, 0)
  ) |>
  select(
    ZMC,
    CO_DOM_X_S,
    CO_DOM_Y_S,
    ZONA,
    tipo_grupo_exp,
    tratamento,
    tratamento2,
    IDADE,
    SEXO,
    GRAU_INS,
    CD_ATIVI,
    ln_renda_f,
    declarou_r,
    FE_PESS,
    viaja,
    TIPVG,
    ano,
    indic_dest,
    trab_cen,
    educ_cen,
    saude_cen,
    lazer_cen,
    comp_cen,
    emp_cen,
    trabeducemp,
    trabemp,
    lazcompsa,
    lazcomp,
    trabeduc,
    metro,
    carro,
    onibus
  ) |>
  mutate(
    SEXO = case_when(
      SEXO == 1 ~ 'Masculino',
      SEXO == 2 ~ 'Feminino',
      TRUE ~ NA_character_
    ),
    SEXO = as.factor(SEXO),
    GRAU_INS = case_when(
      GRAU_INS == 1 ~ 'Não alfabetizado/ Fund 1 incompleto',
      GRAU_INS == 2 ~ 'Fund 1 completo/ Fund 2 incompleto',
      GRAU_INS == 3 ~ 'Fund 2 completo/ Médio incompleto',
      GRAU_INS == 4 ~ 'Médio completo/ Superior incompleto',
      GRAU_INS == 5 ~ 'Superior completo',
      TRUE ~ NA_character_
    ),
    GRAU_INS = as.factor(GRAU_INS),
    CD_ATIVI = case_when(
      CD_ATIVI == 1 ~ 'Trabalho Regular',
      CD_ATIVI == 2 ~ 'Bico',
      CD_ATIVI == 3 ~ 'Licença Médica',
      CD_ATIVI == 4 ~ 'Aposentado/Pensionista',
      CD_ATIVI == 5 ~ 'Desempregado',
      CD_ATIVI == 6 ~ 'Nunca Trabalhou',
      CD_ATIVI == 7 ~ 'Dona de Casa',
      CD_ATIVI == 8 ~ 'Estudante',
      TRUE ~ NA_character_
    ),
    CD_ATIVI = as.factor(CD_ATIVI)
  ) |>
  mutate(
    TIPVG = ifelse(is.na(TIPVG), 0, TIPVG),
    VIAGEM_TRANS_PUBL = ifelse(TIPVG == 1, 1, 0)
  ) |>
  drop_na()


#### Fazendo mapa para validar as ZMC's vizinhas ####

xlim <- c(300000, 385000)
ylim <- c(7365000, 7420000)

### Abrindo os shapefiles da RMSP, da mahca urbana e das linhas de metrô atuais e futuras ###

sp_urb <- read_urban_area(year = 2015, code_state = "SP")
mancha_urbana_sp <- sp_urb[sp_urb$name_muni == "São Paulo/SP", ] |>
  st_transform(crs = 31983) |>
  st_make_valid() |>
  st_union()

rmsp <- read_metro_area(code_state = "SP", year = 2018)
rmsp <- rmsp[rmsp$name_metro == "RM São Paulo", ] |>
  st_transform(crs = 31983)

metro_linhas <- st_read(
  "Shapefiles estações metro SP/SIRGAS_SHP_linhametro_line.shp"
) |>
  st_set_crs(31983) |>
  mutate(
    cores = case_when(
      lmt_nome == "AZUL" ~ "#1F51FF",
      lmt_nome == "VERDE" ~ "green",
      lmt_nome == "VERMELHA" ~ "#EE3E34",
      lmt_nome == "AMARELA" ~ "#FFD700",
      lmt_nome == "LILAS" ~ "#BF00FF",
      lmt_nome == "PRATA" ~ "#c8c8c8c1",
      TRUE ~ "black"
    )
  )

metro_linhas_proj <- st_read(
  "Shapefiles estações metro SP/SIRGAS_SHP_linhametroprojeto.shp"
) |>
  st_set_crs(31983) |>
  filter(lmtp_nome %in% c("LARANJA", "MARROM", "VIOLETA", "CELESTE")) |>
  mutate(
    cores = case_when(
      lmtp_nome == "LARANJA" ~ "#FFA500",
      lmtp_nome == "MARROM" ~ "#8B4513",
      lmtp_nome == "VIOLETA" ~ "#61217dff",
      lmtp_nome == "CELESTE" ~ "#096c7cff",
      TRUE ~ "black"
    )
  )

classific_zmc <- base_robustez_linhas |>
  group_by(ZMC) |>
  summarise(
    zona_nucleo = any(tratamento == 1, na.rm = TRUE),
    zona_vizinha = any(tratamento2 == 1, na.rm = TRUE),
    zona_controle = any(tratamento == 0, na.rm = TRUE)
  ) |>
  mutate(
    grupo_mapa = case_when(
      zona_nucleo & !zona_vizinha & !zona_controle ~ "Treatment - Core",
      zona_vizinha & !zona_nucleo ~ "Treatment - Adjacent",
      !zona_nucleo & !zona_vizinha & zona_controle ~ "Control",
      TRUE ~ "None"
    )
  )

join_class_zonas <- zmc_polygons |>
  inner_join(classific_zmc, by = "ZMC")

join_class_zonas_urb <- st_intersection(
  st_make_valid(join_class_zonas),
  st_make_valid(mancha_urbana_sp)
)

zonas_relevantes <- join_class_zonas_urb |>
  filter(
    grupo_mapa %in% c("Treatment - Core", "Treatment - Adjacent", "Control")
  )

table(classific_zmc$grupo_mapa, useNA = "ifany")
table(join_class_zonas_urb$grupo_mapa, useNA = "ifany")


mapa_robustez_vizinhos <- ggplot() +
  geom_sf(
    data = join_class_zonas_urb,
    aes(fill = grupo_mapa),
    color = "white",
    size = 0.05,
    alpha = 0.8
  ) +
  geom_sf(data = rmsp, fill = NA, color = "grey50", linewidth = 0.3) +
  geom_sf(
    data = metro_linhas,
    color = 'grey50',
    linewidth = 1.5,
    show.legend = FALSE
  ) +
  geom_sf(
    data = metro_linhas,
    aes(color = cores, linetype = "Subway (Existing)"),
    linewidth = 1,
    show.legend = "line"
  ) +
  geom_sf(
    data = metro_linhas_proj,
    aes(color = cores, linetype = "Subway (Planned by 2036)"),
    linewidth = 1
  ) +
  scale_color_identity() +
  scale_linetype_manual(
    name = "Lines",
    values = c(
      "Subway (Existing)" = "solid",
      "Subway (Planned by 2036)" = "twodash"
    ),
    guide = guide_legend(
      title.position = 'top',
      title.hjust = 0.5
    )
  ) +
  scale_fill_manual(
    values = c(
      "Treatment - Core" = "#FF6B6B",
      "Treatment - Adjacent" = "#FFB3A7",
      "Control" = "#4ECDC4"
    ),
    name = "Group",
    na.value = "grey90",
    breaks = function(x) na.omit(x),
    guide = guide_legend(title.position = 'top', title.hjust = 0.5)
  ) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.box = "horizontal",
    legend.title = element_text(face = 'bold'),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(t = 3, r = 3, b = 3, l = 3, unit = "mm")
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)

print(mapa_robustez_vizinhos)

##### Fazendo as Estimações de Event Study de Robustez para o Y = Metrô - Tratamento 1 #####

#### Estimação sem covariadas ####

event_study_linhas_metro_robust_1 <- feols(
  metro ~ i(ano, tratamento, ref = 2017) |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_metro_robust_1)

#### Estimação com covariadas, mas sem os pesos ####

event_study_linhas_metro_robust_2 <- feols(
  metro ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_metro_robust_2)

#### Estimação com covariadas e pesos ####

event_study_linhas_metro_robust_3 <- feols(
  metro ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC,
  weights = ~FE_PESS
)

etable(event_study_linhas_metro_robust_3)

#### Estimação com covariadas, pesos, e PSM ####

set.seed(42)

match_robustez_1 <- matchit(
  tratamento ~ IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI +
    ln_renda_f +
    VIAGEM_TRA,
  data = base_reg_linhas,
  method = "nearest"
)

summary(match_robustez_1)

base_reg_linhas_psm_1 <- match.data(match_robustez_1)

event_study_linhas_metro_robust_4 <- feols(
  metro ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_psm_1,
  cluster = ~ZMC,
  weights = ~weights
)

etable(event_study_linhas_metro_robust_4)

##### Fazendo as Estimações de Event Study de Robustez para o Y = Metrô - Tratamento 2 #####

#### Estimação sem covariadas ####

event_study_linhas_metro_robust_5 <- feols(
  metro ~ i(ano, tratamento2, ref = 2017) |
    ZMC + ano,
  data = base_robustez_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_metro_robust_5)

#### Estimação com covariadas, mas sem os pesos ####

event_study_linhas_metro_robust_6 <- feols(
  metro ~ i(ano, tratamento2, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_robustez_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_metro_robust_6)

#### Estimação com covariadas e pesos ####

event_study_linhas_metro_robust_7 <- feols(
  metro ~ i(ano, tratamento2, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_robustez_linhas,

  cluster = ~ZMC,
  weights = ~FE_PESS
)

etable(event_study_linhas_metro_robust_7)

#### Estimação com covariadas, pesos, e PSM ####

set.seed(42)

match_robustez_2 <- matchit(
  tratamento2 ~ IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI +
    ln_renda_f +
    VIAGEM_TRANS_PUBL,
  data = base_robustez_linhas,
  method = "nearest"
)

summary(match_robustez_2)

base_reg_linhas_psm_2 <- match.data(match_robustez_2)

event_study_linhas_metro_robust_8 <- feols(
  metro ~ i(ano, tratamento2, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_psm_2,
  cluster = ~ZMC,
  weights = ~weights
)

etable(event_study_linhas_metro_robust_8)

#### Armazenando os resultados ####

### Obtendo tanto o y médio pré-tratamento faltante quanto os efeitos relativos para as novas estimações ###

efeitos_relativos_1 <- function(estimacao, y_pre) {
  coef <- round(coef(estimacao)['ano::2023:tratamento'], 4)

  efeito_relativo <- round((coef / y_pre) * 100, 2)

  return(efeito_relativo)
}

efeito_relativo_linhas_metro_1 <- efeitos_relativos_1(
  event_study_linhas_metro_robust_1,
  y_pre_linhas_metro
)

efeito_relativo_linhas_metro_2 <- efeitos_relativos_1(
  event_study_linhas_metro_robust_2,
  y_pre_linhas_metro
)

efeito_relativo_linhas_metro_3 <- efeitos_relativos_1(
  event_study_linhas_metro_robust_3,
  y_pre_linhas_metro
)

efeito_relativo_linhas_metro_4 <- efeitos_relativos_1(
  event_study_linhas_metro_robust_4,
  y_pre_linhas_metro
)

efeitos_relativos_2 <- function(estimacao, y_pre) {
  coef <- round(coef(estimacao)['ano::2023:tratamento2'], 4)

  efeito_relativo <- round((coef / y_pre) * 100, 2)

  return(efeito_relativo)
}

efeito_relativo_linhas_metro_5 <- efeitos_relativos_2(
  event_study_linhas_metro_robust_5,
  y_pre_linhas_metro_2
)

efeito_relativo_linhas_metro_6 <- efeitos_relativos_2(
  event_study_linhas_metro_robust_6,
  y_pre_linhas_metro_2
)

efeito_relativo_linhas_metro_7 <- efeitos_relativos_2(
  event_study_linhas_metro_robust_7,
  y_pre_linhas_metro_2
)

efeito_relativo_linhas_metro_8 <- efeitos_relativos_2(
  event_study_linhas_metro_robust_8,
  y_pre_linhas_metro_2
)


lista_event_study_linhas_robustez <- list(
  'Sem Covariadas - Trat 1' = event_study_linhas_metro_robust_1,
  'Com Covariadas - Trat 1' = event_study_linhas_metro_robust_2,
  'Com Covariadas e Pesos - Trat 1' = event_study_linhas_metro_robust_3,
  'Com Covariadas, Pesos e PSM - Trat 1' = event_study_linhas_metro_robust_4,
  'Sem Covariadas - Trat 2' = event_study_linhas_metro_robust_5,
  'Com Covariadas - Trat 2' = event_study_linhas_metro_robust_6,
  'Com Covariadas e Pesos - Trat 2' = event_study_linhas_metro_robust_7,
  'Com Covariadas, Pesos e PSM - Trat 2' = event_study_linhas_metro_robust_8
)

lista_extra_completa_robustez <- list(
  "Média Pré-Tratamento sobre Tratados" = c(
    y_pre_linhas_metro,
    y_pre_linhas_metro,
    y_pre_linhas_metro,
    y_pre_linhas_metro,
    y_pre_linhas_metro_2,
    y_pre_linhas_metro_2,
    y_pre_linhas_metro_2,
    y_pre_linhas_metro_2
  ),
  "Efeito relativo (%)" = c(
    efeito_relativo_linhas_metro_1,
    efeito_relativo_linhas_metro_2,
    efeito_relativo_linhas_metro_3,
    efeito_relativo_linhas_metro_4,
    efeito_relativo_linhas_metro_5,
    efeito_relativo_linhas_metro_6,
    efeito_relativo_linhas_metro_7,
    efeito_relativo_linhas_metro_8
  )
)

tabela_result_es_robustez <- etable(
  lista_event_study_linhas_robustez,
  tex = FALSE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados para a utilização do Metrô - Event Study - Grupo de Controle Linhas Futuras',
  extralines = lista_extra_completa_robustez
)

tabela_result_es_robustez_latex <- etable(
  lista_event_study_linhas_robustez,
  tex = TRUE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados para a utilização do Metrô - Event Study - Grupo de Controle Linhas Futuras',
  extralines = lista_extra_completa_robustez
)


print(tabela_result_es_robustez_latex)
