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

tabela_result_es_linhas <- etable(
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

### Organizando os resultados ###

lista_event_study_matching <- list(
  'Metrô' = event_study_matching_metro,
  'Carro' = event_study_matching_carro,
  'Ônibus' = event_study_matching_onibus
)

tabela_result_es_matching <- etable(
  lista_event_study_matching,
  tex = FALSE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Event Study - Grupo de Controle Pareado'
)
