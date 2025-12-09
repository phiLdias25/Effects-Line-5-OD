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

od_2007 <- import(OD_2007_v2d.dbf)
