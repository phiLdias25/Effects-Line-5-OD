########### Estimações alternativas de Event Study para os grupos de tratamento e controle criados ######

rm(list = ls())

##### Abrindo bibliotecas #####

library(tidyverse)
library(rio)
library(fixest)
library(broom)

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
  filter(tratamento == 1 & ano < 2018) |>
  summarise(media_pre = round(mean(metro, na.rm = TRUE), 4))

y_medio_pre_match_onibus <- base_reg_matching |>
  filter(tratamento == 1 & ano < 2018) |>
  summarise(media_pre = round(mean(onibus, na.rm = TRUE), 4))

y_medio_pre_match_carro <- base_reg_matching |>
  filter(tratamento == 1 & ano < 2018) |>
  summarise(media_pre = round(mean(carro, na.rm = TRUE), 4))

y_medio_pre_cptm_metro <- base_reg_cptm |>
  filter(tratamento == 1 & ano < 2018) |>
  summarise(media_pre = round(mean(metro, na.rm = TRUE), 4))

y_medio_pre_cptm_onibus <- base_reg_cptm |>
  filter(tratamento == 1 & ano < 2018) |>
  summarise(media_pre = round(mean(onibus, na.rm = TRUE), 4))

y_medio_pre_cptm_carro <- base_reg_cptm |>
  filter(tratamento == 1 & ano < 2018) |>
  summarise(media_pre = round(mean(carro, na.rm = TRUE), 4))

y_medio_pre_linhas_metro <- base_reg_linhas |>
  filter(tratamento == 1 & ano < 2018) |>
  summarise(media_pre = round(mean(metro, na.rm = TRUE), 4))

y_medio_pre_linhas_onibus <- base_reg_linhas |>
  filter(tratamento == 1 & ano < 2018) |>
  summarise(media_pre = round(mean(onibus, na.rm = TRUE), 4))

y_medio_pre_linhas_carro <- base_reg_linhas |>
  filter(tratamento == 1 & ano < 2018) |>
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
  'Trabalho' = lista_original$`Trabalho Centro Expandido`,
  'Educação' = lista_original$`Educação Centro Expandido`,
  'Buscar Emprego' = lista_original$`Buscar Emprego Centro Expandido`,
  'Trab/Educ/Emp' = lista_original$`Trabalho ou Educação ou Buscar Emprego Centro Expandido`,
  'Laz/Comp/Saúde' = lista_original$`Lazer ou Compras ou Saúde Centro Expandido`,

  'Metrô' = event_study_linhas_metro,
  'Carro' = event_study_linhas_carro,
  'Ônibus' = event_study_linhas_onibus
)


lista_extra_completa <- list(
  "Média Pré-Tratamento sobre Tratados" = c(
    lista_original$`Média Pré-Tratamento`[1],
    lista_original$`Média Pré-Tratamento`[2],
    lista_original$`Média Pré-Tratamento`[3],
    lista_original$`Média Pré-Tratamento`[7],
    lista_original$`Média Pré-Tratamento`[8],
    lista_original$`Média Pré-Tratamento`[11],
    y_medio_pre_linhas_metro$media_pre,
    y_medio_pre_linhas_onibus$media_pre,
    y_medio_pre_linhas_carro$media_pre
  ),
  "Efeito relativo (%)" = c(
    lista_original[[8]][c(1, 2, 3, 7, 8, 11)],
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

##### Realizando estimações somente com indivíduos ocupados (CD_ATIVI = Trabalho Regular e CD_ATIVI = BICO) #####

#### Filtrando as bases com controles sem pareamento ####

base_reg_cptm_ocupados <- base_reg_cptm |>
  filter(CD_ATIVI %in% c('Trabalho Regular', 'Bico'))

base_reg_linhas_ocupados <- base_reg_linhas |>
  filter(CD_ATIVI %in% c('Trabalho Regular', 'Bico'))

#### Fazendo as estimações ####

event_study_linhas_dest_ocup <- feols(
  indic_dest ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)

etable(event_study_linhas_dest_ocup)

event_study_linhas_trab_ocup <- feols(
  trab_cen ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)
etable(event_study_linhas_trab_ocup)

event_study_linhas_educ_ocup <- feols(
  educ_cen ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)

etable(event_study_linhas_educ_ocup)

event_study_linhas_saude_ocup <- feols(
  saude_cen ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)

etable(event_study_linhas_saude_ocup)

event_study_linhas_lazer_ocup <- feols(
  lazer_cen ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)

etable(event_study_linhas_lazer_ocup)

event_study_linhas_comp_ocup <- feols(
  comp_cen ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)

etable(event_study_linhas_comp_ocup)

event_study_linhas_emp_ocup <- feols(
  emp_cen ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)

etable(event_study_linhas_emp_ocup)

event_study_linhas_trabeducem_ocup <- feols(
  trabeducem ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)

etable(event_study_linhas_trabeducem_ocup)

event_study_linhas_trabemp_ocup <- feols(
  trabemp ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)

etable(event_study_linhas_trabemp_ocup)

event_study_linhas_trabeduc_ocup <- feols(
  trabeduc ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)

etable(event_study_linhas_trabeduc_ocup)

event_study_linhas_lazcompsa_ocup <- feols(
  lazcompsa ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)

etable(event_study_linhas_lazcompsa_ocup)

event_study_linhas_lazcomp_ocup <- feols(
  lazcomp ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)

etable(event_study_linhas_lazcomp_ocup)

event_study_linhas_metro_ocup <- feols(
  metro ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)

event_study_linhas_carro_ocup <- feols(
  carro ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)

event_study_linhas_onibus_ocup <- feols(
  onibus ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas_ocupados,

  cluster = ~ZMC
)

lista_event_study_linhas_ocup <- list(
  'Destino Centro Expandido' = event_study_linhas_dest_ocup,
  'Metrô' = event_study_linhas_metro_ocup,
  'Carro' = event_study_linhas_carro_ocup,
  'Ônibus' = event_study_linhas_onibus_ocup,
  'Trabalho Centro Expandido' = event_study_linhas_trab_ocup,
  'Educação Centro Expandido' = event_study_linhas_educ_ocup,
  'Saúde Centro Expandido' = event_study_linhas_saude_ocup,
  'Lazer Centro Expandido' = event_study_linhas_lazer_ocup,
  'Compras Centro Expandido' = event_study_linhas_comp_ocup,
  'Buscar Emprego Centro Expandido' = event_study_linhas_emp_ocup,
  'Trabalho ou Educação ou Buscar Emprego Centro Expandido' = event_study_linhas_trabeducem_ocup,
  'Trabalho ou Educação Centro Expandido' = event_study_linhas_trabeduc_ocup,
  'Trabalho ou Buscar Emprego Centro Expandido' = event_study_linhas_trabemp_ocup,
  'Lazer ou Compras ou Saúde Centro Expandido' = event_study_linhas_lazcompsa_ocup,
  'Lazer ou Compras Centro Expandido' = event_study_linhas_lazcomp_ocup
)

tabela_result_es_linhas_ocup <- etable(
  lista_event_study_linhas_ocup,
  tex = FALSE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Event Study - Grupo de Controle Linhas Futuras'
)

tabela_result_es_linhas_ocup_latex <- etable(
  lista_event_study_linhas_ocup,
  tex = TRUE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Event Study - Grupo de Controle Linhas Futuras'
)
