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

##### Filtrando as variáveis importantes de cada base #####

od_2007_filtrada <- od_2007 |>
    select(
        ZONA, # Zona do domicílio
        MUNI_DOM, # Município do domicílio
        CO_DOM_X, # Coordenada X do domicílio
        CO_DOM_Y, # Coordenada Y do domicílio
        ID_DOM, # Identificação do domicílio
        TIPO_DOM, # Tipo do domicílio: 1 - Particular; 2 - Coletivo, 3 - Favela
        CONDMORA, # Condição de moradia: 1 - Alugada; 2 - Própria; 3 - Cedida, 4 - Outros; 5 - Não respondeu
        CRITERIOBR, # Critério de Classificação Econômica Brasil
        ID_PESS, # Identificação da pessoa
        SIT_FAM, # Situação Familiar: 1 - Responsável; 2 - Cônjuge/companheiro(a); 3 - Filho(a)/Enteado(a); 4 - Outro Parente; 5 - Agregado; 6 - Empregado Residente; 7 - Parente do Empregado
        IDADE,
        SEXO,
        ESTUDA, # 1 - Não; 2 - Creche/Pré-Escola; 3 - Ensino Fundamental; 4 - Ensino Médio; 6 - Ensino Superior; 7 - Outros
        GRAU_INS, # Grau de Instrução: 1 - Não Alfabetizado/ Fund 1 Incompleto ; 2 - Fund 1 Completo/Fund 2 Incompleto; 3 - Fund 2 Completo/ Médio Incompleto; 4 - Médio Completo/ Superior Incompleto; 5 - Superior Completo
        CD_ATIVI, # Condição de atividade: 1 - Trabalho Regular; 2 - Bico; 3 - Licença Médica; 4 - Aposentado/Pensionista; 5 - Desempregado; 6 - Nunca trabalhou; 7 - Dona de Casa; 8 - Estudante
        VL_REN_I, # Renda Individual
        ZONATRA1, # Zona de Trabalho 1
        MUNITRA1, # Município de Trabalho 1
        CO_TR1_X, # Coordenada X do Trabalho 1
        CO_TR1_Y, # Coordenada Y do Trabalho 1
        OCUP1, # Ocupação do Trabalho 1
        SETOR1, # Setor do Trabalho 1
        VINC1, # Vínculo do Trabalho 1
        TOT_VIAG, # Total de Viagens do Indivíduo
        MODO1, # Modo de Transporte da Viagem 1
        DURACAO, # Duração da Viagem (MINUTOS)
        MODOPRIN, # Modo Principal da Viagem
        DISTANCIA, # Distância da Viagem (Linha reta entre a coord de origem e coord de destino)
    )
