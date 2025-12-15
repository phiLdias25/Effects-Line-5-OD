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

od_1997 <- import('OD97Zona.dbf')

od_2007 <- import('OD_2007_v2d.dbf')

od_2017 <- import('OD_2017_v1.dbf')

od_2023 <- import('Banco2023_divulgacao_190225.dbf')

##### Filtrando as variáveis importantes de cada base #####

od_1997_filtrada <- od_1997 |>
    select(
        ZONA, # Zona do domicílio
        MUNI_DOM, # Município do domicílio
        ID_DOM, # Identificação do domicílio
        TIPO_DOM, # Tipo do domicílio: 1 - Particular; 2 - Coletivo; 3 - Favela
        CONDMORA, # Condição de moradia: 1 - Alugada; 2 - Própria; 3 - Cedida, 4 - Outros; 5 - Não respondeu
        ABIPEME, # Critério de Classificação Econômica Brasil (antiga, chamada de ABIPEME)
        ID_PESS, # Identificação da pessoa
        SIT_FAM, # Situação Familiar: 1 - Responsável; 2 - Cônjuge/companheiro(a); 3 - Filho(a)/Enteado(a); 4 - Outro Parente; 5 - Empregado Residente; 6 - Visitante não residente
        IDADE,
        SEXO,
        SE_ESTUD, # 1 - Não; 2 - Creche/Pré-Escola; 3 - Ensino Básico/Fundamental/Médio; 4 - Outros
        GRAU_INS, # Grau de Instrução: 1 - Não Alfabetizado; 2 - Pré-Escola; 3 - Fundamental Incompleto; 4 - Fundamental Completo; 5 - Médio Incompleto; 6 - Médio Completo; 7 - Superior Incompleto; 8 - Superior Completo
        CD_ATIVI, # Condição de atividade: 1 - Trabalho Regular; 2 - Bico; 3 - Licença Médica; 4 - Desempregado; 5 - Aposentado/Pensionista; 6 - Nunca trabalhou; 7 - Dona de Casa; 8 - Estudante
        VL_REN_I, # Renda Individual
        ZONATRA1, # Zona de Trabalho 1
        MUNITRA1, # Município de Trabalho 1
        SET_ATIV, # Setor do Trabalho 1
        OCUP_PRI, # Vínculo do Trabalho 1
        MODO1, # Modo de Transporte da Viagem 1
        DURACAO, # Duração da Viagem (MINUTOS)
        MODOPRIN, # Modo Principal da Viagem
        ZONA_O, # Zona de Origem da Viagem
        ZONA_D, # Zona de Destino da Viagem
        MOTIVO_O, # Motivo da Viagem de Origem: 1 - Trabalho/Indústria; 2 - Trabalho/Comércio; 3 - Trabalho/Serviços; 4 - Educação; 5 - Compras; 6 - Saúde; 7 - Lazer; 8 - Residência; 9 - Outros
        MOTIVO_D # Motivo da Viagem de Origem: 1 - Trabalho/Indústria; 2 - Trabalho/Comércio; 3 - Trabalho/Serviços; 4 - Educação; 5 - Compras; 6 - Saúde; 7 - Lazer; 8 - Residência; 9 - Outros
    ) |>
    mutate(
        ano = 1997,
        TOT_VIAG = NA,
        CO_DOM_X = NA,
        CO_DOM_Y = NA,
        CO_TR1_X = NA,
        CO_TR1_Y = NA,
        DISTANCIA = NA,
        CO_O_X = NA,
        CO_O_Y = NA,
        CO_D_X = NA,
        CO_D_Y = NA,
        ID_DOM = as.character(ID_DOM)
    )

# Problemas: 1997 não possui coordenadas, critério econômico antigo, grau de instrução limitado, sem total de viagens, ocupação do trabalho 1 e setor do trabalho 1 diferentes

## Trocando nomes diferentes da base de 1997

od_1997_filtrada <- od_1997_filtrada |>
    rename(
        CRITERIOBR = ABIPEME,
        ESTUDA = SE_ESTUD,
        VINC1 = OCUP_PRI,
        SETOR1 = SET_ATIV
    )

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
        ZONA_O, # Zona de Origem da Viagem
        CO_O_X, # Coordenada X da Origem da Viagem
        CO_O_Y, # Coordenada Y da Origem da Viagem
        ZONA_D, # Zona de Destino da Viagem
        CO_D_X, # Coordenada X do Destino da Viagem
        CO_D_Y, # Coordenada Y do Destino da Viagem
        MOTIVO_O, # Motivo da Viagem de Origem: 1 - Trabalho/Indústria; 2 - Trabalho/Comércio; 3 - Trabalho/Serviços; 4 - Educação; 5 - Compras; 6 - Saúde; 7 - Lazer; 8 - Residência; 9 - Procurar Emprego; 10 - Assuntos Pessoais
        MOTIVO_D # Motivo da Viagem de Origem: 1 - Trabalho/Indústria; 2 - Trabalho/Comércio; 3 - Trabalho/Serviços; 4 - Educação; 5 - Compras; 6 - Saúde; 7 - Lazer; 8 - Residência; 9 - Procurar Emprego; 10 - Assuntos Pessoais
    ) |>
    mutate(ano = 2007, VL_REN_I = as.numeric(VL_REN_I))

od_2017_filtrada <- od_2017 |>
    select(
        ZONA, # Zona do domicílio
        MUNI_DOM, # Município do domicílio
        CO_DOM_X, # Coordenada X do domicílio
        CO_DOM_Y, # Coordenada Y do domicílio
        ID_DOM, # Identificação do domicílio
        TIPO_DOM, # Tipo do domicílio: 1 - Particular; 2 - Coletivo
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
        ZONA_O, # Zona de Origem da Viagem
        CO_O_X, # Coordenada X da Origem da Viagem
        CO_O_Y, # Coordenada Y da Origem da Viagem
        ZONA_D, # Zona de Destino da Viagem
        CO_D_X, # Coordenada X do Destino da Viagem
        CO_D_Y, # Coordenada Y do Destino da Viagem
        MOTIVO_O, # Motivo da Viagem de Origem: 1 - Trabalho/Indústria; 2 - Trabalho/Comércio; 3 - Trabalho/Serviços; 4 - Educação; 5 - Compras; 6 - Saúde; 7 - Lazer; 8 - Residência; 9 - Procurar Emprego; 10 - Assuntos Pessoais
        MOTIVO_D # Motivo da Viagem de Origem: 1 - Trabalho/Indústria; 2 - Trabalho/Comércio; 3 - Trabalho/Serviços; 4 - Educação; 5 - Compras; 6 - Saúde; 7 - Lazer; 8 - Residência; 9 - Procurar Emprego; 10 - Assuntos Pessoais
    ) |>
    mutate(ano = 2017)


od_2023_filtrada <- od_2023 |>
    select(
        ZONA, # Zona do domicílio
        MUNI_DOM, # Município do domicílio
        CO_DOM_X, # Coordenada X do domicílio
        CO_DOM_Y, # Coordenada Y do domicílio
        ID_DOM, # Identificação do domicílio
        TIPO_DOM, # Tipo do domicílio: 1 - Particular; 2 - Coletivo
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
        DS_OCUP_TR, # Ocupação do Trabalho 1
        SETOR1, # Setor do Trabalho 1
        VINC1, # Vínculo do Trabalho 1
        TOT_VIAG, # Total de Viagens do Indivíduo
        MODO1, # Modo de Transporte da Viagem 1
        DURACAO, # Duração da Viagem (MINUTOS)
        MODOPRIN, # Modo Principal da Viagem
        DISTANCIA, # Distância da Viagem (Linha reta entre a coord de origem e coord de destino)
        ZONA_O, # Zona de Origem da Viagem
        CO_O_X, # Coordenada X da Origem da Viagem
        CO_O_Y, # Coordenada Y da Origem da Viagem
        ZONA_D, # Zona de Destino da Viagem
        CO_D_X, # Coordenada X do Destino da Viagem
        CO_D_Y, # Coordenada Y do Destino da Viagem
        MOTIVO_O, # Motivo da Viagem de Origem: 1 - Trabalho/Indústria; 2 - Trabalho/Comércio; 3 - Trabalho/Serviços; 4 - Educação; 5 - Compras; 6 - Saúde; 7 - Lazer; 8 - Residência; 9 - Procurar Emprego; 10 - Assuntos Pessoais
        MOTIVO_D # Motivo da Viagem de Origem: 1 - Trabalho/Indústria; 2 - Trabalho/Comércio; 3 - Trabalho/Serviços; 4 - Educação; 5 - Compras; 6 - Saúde; 7 - Lazer; 8 - Residência; 9 - Procurar Emprego; 10 - Assuntos Pessoais
    ) |>
    mutate(
        ano = 2023,
        ID_PESS = as.character(ID_PESS),
        VL_REN_I = as.numeric(VL_REN_I),
        OCUP1 = DS_OCUP_TR
    )

##### Arrumando diferenças entre as pesquisas de cada base #####

### Em 1997 e 2007, a variável de tipo de domicílio engloba favelas como um tipo diferente, enquanto as outras não fazem a separação. Vamos separá-las como domicílios particulares ###

od_1997_filtrada <- od_1997_filtrada |>
    mutate(
        TIPO_DOM = ifelse(TIPO_DOM == 3, 1, TIPO_DOM)
    )

od_2007_filtrada <- od_2007_filtrada |>
    mutate(
        TIPO_DOM = ifelse(TIPO_DOM == 3, 1, TIPO_DOM)
    )

### Em 1997, o grau de instrução estava mais detalhado. Vamos adequá-lo às outras pesquisas ###

od_1997_filtrada <- od_1997_filtrada |>
    mutate(
        GRAU_INS_ANTIGO = GRAU_INS,
        GRAU_INS = case_when(
            GRAU_INS %in% c(1L, 2L, 3L) ~ 1L, # não alfabetizado / pré-escola / fund. incompleto
            GRAU_INS == 4L ~ 2L, # fund. completo
            GRAU_INS == 5L ~ 3L, # médio incompleto
            GRAU_INS %in% c(6L, 7L) ~ 4L, # médio completo / superior incompleto
            GRAU_INS == 8L ~ 5L, # superior completo
            TRUE ~ NA_integer_
        )
    )


##### Unindo as bases filtradas #####

od_completa <- bind_rows(
    od_2007_filtrada,
    od_2017_filtrada,
    od_2023_filtrada
)

##### Fazendo uma variável indicadora se a viagem dos indivíduos foi realizada ao centro expandido de SP #####
