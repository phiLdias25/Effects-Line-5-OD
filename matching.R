###### Criação do grupo de controle por MatchIt ######

rm(list = ls())

##### Abrindo bibliotecas #####

library(tidyverse)
library(MatchIt)
library(cobalt)
library(stargazer)
library(rio)
library(sf)
library(kableExtra)
library(scales)
library(scico)

##### Abrindo base de dados #####

od_completa <- import('od_base_completa.dbf')


#### Filtrando o grupo de controle paramétrico da base para criar o grupo de controle por pareamento ####

od_matching <- od_completa |>
  filter(tipo_grupo != 'Controle_Parametrico') |>
  mutate(tratamento_binario = ifelse(tipo_grupo == 'Tratamento', 1, 0))
