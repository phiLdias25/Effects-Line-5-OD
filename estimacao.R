###### Estimação de Diferenças-em-Diferenças com os grupos de controle paramétricos e pareados ######

rm(list = ls())

##### Abrindo bibliotecas #####

library(tidyverse)
library(rio)
library(fixest)
library(did)
library(bacondecomp)
library(modelsummary)

##### Abrindo bases de dados #####

base_matching <- import('base_pareamento.dbf')

##### Criando as variáveis de pré e pós e realizando a estimação com o grupo de controle pareado #####

base_reg_matching <- base_matching |>
  mutate(
    post = if_else(ano >= 2018, 1, 0)
  )

#### Média simples do efeito ####

did_agregado_match_dest <- feols(
  indic_dest ~ tratamento * post,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZONA_O
)

etable(did_agregado_match_dest)

did_agregado_match_trab <- feols(
  trab_cen ~ tratamento * post,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZONA_O
)

etable(did_agregado_match_trab)

did_agregado_match_educ <- feols(
  educ_cen ~ tratamento * post,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZONA_O
)

etable(did_agregado_match_educ)

did_agregado_match_saude <- feols(
  saude_cen ~ tratamento * post,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZONA_O
)

etable(did_agregado_match_saude)

did_agregado_match_lazer <- feols(
  lazer_cen ~ tratamento * post,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZONA_O
)

etable(did_agregado_match_lazer)

did_agregado_match_comp <- feols(
  comp_cen ~ tratamento * post,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZONA_O
)

etable(did_agregado_match_comp)

did_agregado_match_emp <- feols(
  emp_cen ~ tratamento * post,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZONA_O
)

etable(did_agregado_match_emp)

did_agregado_match_trabeducem <- feols(
  trabeducem ~ tratamento * post,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZONA_O
)

etable(did_agregado_match_trabeducem)

did_agregado_match_trabemp <- feols(
  trabemp ~ tratamento * post,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZONA_O
)

etable(did_agregado_match_trabemp)

did_agregado_match_trabeduc <- feols(
  trabeduc ~ tratamento * post,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZONA_O
)

etable(did_agregado_match_trabeduc)

did_agregado_match_lazcompsa <- feols(
  lazcompsa ~ tratamento * post,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZONA_O
)

etable(did_agregado_match_lazcompsa)

did_agregado_match_lazcomp <- feols(
  lazcomp ~ tratamento * post,
  data = base_reg_matching,
  weights = ~weights,
  cluster = ~ZONA_O
)

etable(did_agregado_match_lazcomp)
