###### Estimação de Diferenças-em-Diferenças com os grupos de controle paramétricos e pareados ######

rm(list = ls())

##### Abrindo bibliotecas #####

library(tidyverse)
library(rio)
library(fixest)
library(did)
library(bacondecomp)
library(broom)

##### Abrindo bases de dados #####

base_matching <- import('base_pareamento.dbf')
base_linhas_futuras <- import('base_linhas_futuras.dbf')
base_cptm <- import('base_cptm.dbf')

##### Criando as variáveis de pré e pós e realizando a estimação com o grupo de controle 1 - Linhas Futuras #####

base_reg_linhas <- base_linhas_futuras |>
  mutate(
    post = if_else(ano >= 2018, 1, 0)
  )

### Média simples do efeito ###

did_agregado_linhas_dest <- feols(
  indic_dest ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(did_agregado_linhas_dest)

did_agregado_linhas_trab <- feols(
  trab_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_linhas,
  cluster = ~ZMC
)

etable(did_agregado_linhas_trab)

did_agregado_linhas_educ <- feols(
  educ_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(did_agregado_linhas_educ)

did_agregado_linhas_saude <- feols(
  saude_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(did_agregado_linhas_saude)

did_agregado_linhas_lazer <- feols(
  lazer_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(did_agregado_linhas_lazer)

did_agregado_linhas_comp <- feols(
  comp_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(did_agregado_linhas_comp)

did_agregado_linhas_emp <- feols(
  emp_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(did_agregado_linhas_emp)

did_agregado_linhas_trabeducem <- feols(
  trabeducem ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(did_agregado_linhas_trabeducem)

did_agregado_linhas_trabemp <- feols(
  trabemp ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(did_agregado_linhas_trabemp)

did_agregado_linhas_trabeduc <- feols(
  trabeduc ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(did_agregado_linhas_trabeduc)

did_agregado_linhas_lazcompsa <- feols(
  lazcompsa ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(did_agregado_linhas_lazcompsa)

did_agregado_linhas_lazcomp <- feols(
  lazcomp ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(did_agregado_linhas_lazcomp)

### Event Study ###

event_study_linhas_dest <- feols(
  indic_dest ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_dest)

event_study_linhas_trab <- feols(
  trab_cen ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)
etable(event_study_linhas_trab)

event_study_linhas_educ <- feols(
  educ_cen ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_educ)

event_study_linhas_saude <- feols(
  saude_cen ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_saude)

event_study_linhas_lazer <- feols(
  lazer_cen ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_lazer)

event_study_linhas_comp <- feols(
  comp_cen ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_comp)

event_study_linhas_emp <- feols(
  emp_cen ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_emp)

event_study_linhas_trabeducem <- feols(
  trabeducem ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_trabeducem)

event_study_linhas_trabemp <- feols(
  trabemp ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_trabemp)

event_study_linhas_trabeduc <- feols(
  trabeduc ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_trabeduc)

event_study_linhas_lazcompsa <- feols(
  lazcompsa ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_lazcompsa)

event_study_linhas_lazcomp <- feols(
  lazcomp ~ i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_linhas,

  cluster = ~ZMC
)

etable(event_study_linhas_lazcomp)

#### Agregando todos os resultados em duas tabelas, uma para cada tipo de estimação ####

### Diferença em Diferenças - Média Simples do Efeito ###

lista_did_agregado_linhas <- list(
  'Destino Centro Expandido' = did_agregado_linhas_dest,
  'Trabalho Centro Expandido' = did_agregado_linhas_trab,
  'Educação Centro Expandido' = did_agregado_linhas_educ,
  'Saúde Centro Expandido' = did_agregado_linhas_saude,
  'Lazer Centro Expandido' = did_agregado_linhas_lazer,
  'Compras Centro Expandido' = did_agregado_linhas_comp,
  'Buscar Emprego Centro Expandido' = did_agregado_linhas_emp,
  'Trabalho ou Educação ou Buscar Emprego Centro Expandido' = did_agregado_linhas_trabeducem,
  'Trabalho ou Educação Centro Expandido' = did_agregado_linhas_trabeduc,
  'Trabalho ou Buscar Emprego Centro Expandido' = did_agregado_linhas_trabemp,
  'Lazer ou Compras ou Saúde Centro Expandido' = did_agregado_linhas_lazcompsa,
  'Lazer ou Compras Centro Expandido' = did_agregado_linhas_lazcomp
)

tabela_result_did_linhas <- etable(
  lista_did_agregado_linhas,
  tex = FALSE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Diferença em Diferenças - Grupo de Controle Linhas Futuras'
)

tabela_result_did_linhas_latex <- etable(
  lista_did_agregado_linhas,
  tex = TRUE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Diferença em Diferenças - Grupo de Controle Linhas Futuras'
)

### Event Study ###

lista_event_study_linhas <- list(
  'Destino Centro Expandido' = event_study_linhas_dest,
  'Trabalho Centro Expandido' = event_study_linhas_trab,
  'Educação Centro Expandido' = event_study_linhas_educ,
  'Saúde Centro Expandido' = event_study_linhas_saude,
  'Lazer Centro Expandido' = event_study_linhas_lazer,
  'Compras Centro Expandido' = event_study_linhas_comp,
  'Buscar Emprego Centro Expandido' = event_study_linhas_emp,
  'Trabalho ou Educação ou Buscar Emprego Centro Expandido' = event_study_linhas_trabeducem,
  'Trabalho ou Educação Centro Expandido' = event_study_linhas_trabeduc,
  'Trabalho ou Buscar Emprego Centro Expandido' = event_study_linhas_trabemp,
  'Lazer ou Compras ou Saúde Centro Expandido' = event_study_linhas_lazcompsa,
  'Lazer ou Compras Centro Expandido' = event_study_linhas_lazcomp
)

tabela_result_es_linhas <- etable(
  lista_event_study_linhas,
  tex = FALSE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Event Study - Grupo de Controle Linhas Futuras'
)

tabela_result_es_linhas_latex <- etable(
  lista_event_study_linhas,
  tex = TRUE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Event Study - Grupo de Controle Linhas Futuras'
)

##### Criando as variáveis de pré e pós e realizando a estimação com o grupo de controle 2 - CPTM #####

base_reg_cptm <- base_cptm |>
  mutate(
    post = if_else(ano >= 2018, 1, 0)
  )

### Média simples do efeito ###

did_agregado_cptm_dest <- feols(
  indic_dest ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(did_agregado_cptm_dest)

did_agregado_cptm_trab <- feols(
  trab_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(did_agregado_cptm_trab)

did_agregado_cptm_educ <- feols(
  educ_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(did_agregado_cptm_educ)

did_agregado_cptm_saude <- feols(
  saude_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(did_agregado_cptm_saude)

did_agregado_cptm_lazer <- feols(
  lazer_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(did_agregado_cptm_lazer)

did_agregado_cptm_comp <- feols(
  comp_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(did_agregado_cptm_comp)

did_agregado_cptm_emp <- feols(
  emp_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(did_agregado_cptm_emp)

did_agregado_cptm_trabeducem <- feols(
  trabeducem ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(did_agregado_cptm_trabeducem)

did_agregado_cptm_trabemp <- feols(
  trabemp ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(did_agregado_cptm_trabemp)

did_agregado_cptm_trabeduc <- feols(
  trabeduc ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(did_agregado_cptm_trabeduc)

did_agregado_cptm_lazcompsa <- feols(
  lazcompsa ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(did_agregado_cptm_lazcompsa)

did_agregado_cptm_lazcomp <- feols(
  lazcomp ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(did_agregado_cptm_lazcomp)

### Event Study ###

event_study_cptm_dest <- feols(
  indic_dest ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(event_study_cptm_dest)

event_study_cptm_trab <- feols(
  trab_cen ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)
etable(event_study_cptm_trab)

event_study_cptm_educ <- feols(
  educ_cen ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(event_study_cptm_educ)

event_study_cptm_saude <- feols(
  saude_cen ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(event_study_cptm_saude)

event_study_cptm_lazer <- feols(
  lazer_cen ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(event_study_cptm_lazer)

event_study_cptm_comp <- feols(
  comp_cen ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(event_study_cptm_comp)

event_study_cptm_emp <- feols(
  emp_cen ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(event_study_cptm_emp)

event_study_cptm_trabeducem <- feols(
  trabeducem ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(event_study_cptm_trabeducem)

event_study_cptm_trabemp <- feols(
  trabemp ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(event_study_cptm_trabemp)

event_study_cptm_trabeduc <- feols(
  trabeduc ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(event_study_cptm_trabeduc)

event_study_cptm_lazcompsa <- feols(
  lazcompsa ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(event_study_cptm_lazcompsa)

event_study_cptm_lazcomp <- feols(
  lazcomp ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_cptm,

  cluster = ~ZMC
)

etable(event_study_cptm_lazcomp)

#### Agregando todos os resultados em duas tabelas, uma para cada tipo de estimação ####

### Diferença em Diferenças - Média Simples do Efeito ###

lista_did_agregado_cptm <- list(
  'Destino Centro Expandido' = did_agregado_cptm_dest,
  'Trabalho Centro Expandido' = did_agregado_cptm_trab,
  'Educação Centro Expandido' = did_agregado_cptm_educ,
  'Saúde Centro Expandido' = did_agregado_cptm_saude,
  'Lazer Centro Expandido' = did_agregado_cptm_lazer,
  'Compras Centro Expandido' = did_agregado_cptm_comp,
  'Buscar Emprego Centro Expandido' = did_agregado_cptm_emp,
  'Trabalho ou Educação ou Buscar Emprego Centro Expandido' = did_agregado_cptm_trabeducem,
  'Trabalho ou Educação Centro Expandido' = did_agregado_cptm_trabeduc,
  'Trabalho ou Buscar Emprego Centro Expandido' = did_agregado_cptm_trabemp,
  'Lazer ou Compras ou Saúde Centro Expandido' = did_agregado_cptm_lazcompsa,
  'Lazer ou Compras Centro Expandido' = did_agregado_cptm_lazcomp
)

tabela_result_did_cptm <- etable(
  lista_did_agregado_cptm,
  tex = FALSE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Diferença em Diferenças - Grupo de Controle CPTM'
)

tabela_result_did_cptm_latex <- etable(
  lista_did_agregado_cptm,
  tex = TRUE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Diferença em Diferenças - Grupo de Controle CPTM'
)

### Event Study ###

lista_event_study_cptm <- list(
  'Destino Centro Expandido' = event_study_cptm_dest,
  'Trabalho Centro Expandido' = event_study_cptm_trab,
  'Educação Centro Expandido' = event_study_cptm_educ,
  'Saúde Centro Expandido' = event_study_cptm_saude,
  'Lazer Centro Expandido' = event_study_cptm_lazer,
  'Compras Centro Expandido' = event_study_cptm_comp,
  'Buscar Emprego Centro Expandido' = event_study_cptm_emp,
  'Trabalho ou Educação ou Buscar Emprego Centro Expandido' = event_study_cptm_trabeducem,
  'Trabalho ou Educação Centro Expandido' = event_study_cptm_trabeduc,
  'Trabalho ou Buscar Emprego Centro Expandido' = event_study_cptm_trabemp,
  'Lazer ou Compras ou Saúde Centro Expandido' = event_study_cptm_lazcompsa,
  'Lazer ou Compras Centro Expandido' = event_study_cptm_lazcomp
)

tabela_result_es_cptm <- etable(
  lista_event_study_cptm,
  tex = FALSE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Event Study - Grupo de Controle CPTM'
)

tabela_result_es_cptm_latex <- etable(
  lista_event_study_cptm,
  tex = TRUE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Event Study - Grupo de Controle CPTM'
)

##### Criando as variáveis de pré e pós e realizando a estimação com o grupo de controle pareado #####

base_reg_matching <- base_matching |>
  mutate(
    post = if_else(ano >= 2018, 1, 0)
  )

### Média simples do efeito ###

did_agregado_match_dest <- feols(
  indic_dest ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(did_agregado_match_dest)

did_agregado_match_trab <- feols(
  trab_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(did_agregado_match_trab)

did_agregado_match_educ <- feols(
  educ_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(did_agregado_match_educ)

did_agregado_match_saude <- feols(
  saude_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(did_agregado_match_saude)

did_agregado_match_lazer <- feols(
  lazer_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(did_agregado_match_lazer)

did_agregado_match_comp <- feols(
  comp_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(did_agregado_match_comp)

did_agregado_match_emp <- feols(
  emp_cen ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(did_agregado_match_emp)

did_agregado_match_trabeducem <- feols(
  trabeducem ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(did_agregado_match_trabeducem)

did_agregado_match_trabemp <- feols(
  trabemp ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(did_agregado_match_trabemp)

did_agregado_match_trabeduc <- feols(
  trabeduc ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(did_agregado_match_trabeduc)

did_agregado_match_lazcompsa <- feols(
  lazcompsa ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(did_agregado_match_lazcompsa)

did_agregado_match_lazcomp <- feols(
  lazcomp ~ tratamento:post + IDADE + SEXO + GRAU_INS + CD_ATIVI | ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(did_agregado_match_lazcomp)

### Event Study ###

event_study_match_dest <- feols(
  indic_dest ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(event_study_match_dest)

event_study_match_trab <- feols(
  trab_cen ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)
etable(event_study_match_trab)

event_study_match_educ <- feols(
  educ_cen ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(event_study_match_educ)

event_study_match_saude <- feols(
  saude_cen ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(event_study_match_saude)

event_study_match_lazer <- feols(
  lazer_cen ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(event_study_match_lazer)

event_study_match_comp <- feols(
  comp_cen ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(event_study_match_comp)

event_study_match_emp <- feols(
  emp_cen ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(event_study_match_emp)

event_study_match_trabeducem <- feols(
  trabeducem ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(event_study_match_trabeducem)

event_study_match_trabemp <- feols(
  trabemp ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(event_study_match_trabemp)

event_study_match_trabeduc <- feols(
  trabeduc ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(event_study_match_trabeduc)

event_study_match_lazcompsa <- feols(
  lazcompsa ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(event_study_match_lazcompsa)

event_study_match_lazcomp <- feols(
  lazcomp ~
    i(ano, tratamento, ref = 2017) +
    IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI |
    ZMC + ano,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZMC
)

etable(event_study_match_lazcomp)

#### Agregando todos os resultados em duas tabelas, uma para cada tipo de estimação ####

### Diferença em Diferenças - Média Simples do Efeito ###

lista_did_agregado_match <- list(
  'Destino Centro Expandido' = did_agregado_match_dest,
  'Trabalho Centro Expandido' = did_agregado_match_trab,
  'Educação Centro Expandido' = did_agregado_match_educ,
  'Saúde Centro Expandido' = did_agregado_match_saude,
  'Lazer Centro Expandido' = did_agregado_match_lazer,
  'Compras Centro Expandido' = did_agregado_match_comp,
  'Buscar Emprego Centro Expandido' = did_agregado_match_emp,
  'Trabalho ou Educação ou Buscar Emprego Centro Expandido' = did_agregado_match_trabeducem,
  'Trabalho ou Educação Centro Expandido' = did_agregado_match_trabeduc,
  'Trabalho ou Buscar Emprego Centro Expandido' = did_agregado_match_trabemp,
  'Lazer ou Compras ou Saúde Centro Expandido' = did_agregado_match_lazcompsa,
  'Lazer ou Compras Centro Expandido' = did_agregado_match_lazcomp
)

tabela_result_did_match <- etable(
  lista_did_agregado_match,
  tex = FALSE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Diferença em Diferenças - Grupo de Controle Pareado'
)

tabela_result_did_match_latex <- etable(
  lista_did_agregado_match,
  tex = TRUE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Diferença em Diferenças - Grupo de Controle Pareado'
)

### Event Study ###

lista_event_study_match <- list(
  'Destino Centro Expandido' = event_study_match_dest,
  'Trabalho Centro Expandido' = event_study_match_trab,
  'Educação Centro Expandido' = event_study_match_educ,
  'Saúde Centro Expandido' = event_study_match_saude,
  'Lazer Centro Expandido' = event_study_match_lazer,
  'Compras Centro Expandido' = event_study_match_comp,
  'Buscar Emprego Centro Expandido' = event_study_match_emp,
  'Trabalho ou Educação ou Buscar Emprego Centro Expandido' = event_study_match_trabeducem,
  'Trabalho ou Educação Centro Expandido' = event_study_match_trabeduc,
  'Trabalho ou Buscar Emprego Centro Expandido' = event_study_match_trabemp,
  'Lazer ou Compras ou Saúde Centro Expandido' = event_study_match_lazcompsa,
  'Lazer ou Compras Centro Expandido' = event_study_match_lazcomp
)

tabela_result_es_match <- etable(
  lista_event_study_match,
  tex = FALSE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Event Study - Grupo de Controle Pareado'
)

tabela_result_es_match_latex <- etable(
  lista_event_study_match,
  tex = TRUE,
  signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  headers = 'auto',
  title = 'Resultados - Event Study - Grupo de Controle Pareado'
)
