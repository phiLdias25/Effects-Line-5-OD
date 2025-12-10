##### Código - Artigo APS #####

##### Abrindo Bibliotecas #####

library(tidyverse)
library(rio)
library(sf)
library(fixest)
library(did)
library(bacondecomp)
library(MatchIt)
library(cobalt)
library(stargazer)

##### Importando base de dados #####

od_2007 <- import('OD_2007_v2d.dbf')

od_2017 <- import('OD_2017_v1.dbf')

od_2023 <- import('Banco2023_divulgacao_190225.dbf')
