###### Tratamento das bases da OD ######

rm(list = ls())

##### Abrindo Bibliotecas #####

library(tidyverse)
library(rio)
library(sf)
library(deflateBR)

##### Importando base de dados #####

#od_1997 <- import('OD97Zona.dbf')

od_2007 <- import('Bases de dados OD/OD_2007_v2d.dbf')

od_2017 <- import('Bases de dados OD/OD_2017_v1.dbf')

od_2023 <- import('Bases de dados OD/Banco2023_divulgacao_190225.dbf')

##### Filtrando as variáveis importantes de cada base #####

#od_1997_filtrada <- od_1997 |>
#    select(
#        ZONA, # Zona do domicílio
#        MUNI_DOM, # Município do domicílio
#        ID_DOM, # Identificação do domicílio
#        TIPO_DOM, # Tipo do domicílio: 1 - Particular; 2 - Coletivo; 3 - Favela
#        CONDMORA, # Condição de moradia: 1 - Alugada; 2 - Própria; 3 - Cedida, 4 - Outros; 5 - Não respondeu
#        ABIPEME, # Critério de Classificação Econômica Brasil (antiga, chamada de ABIPEME)
#        ID_PESS, # Identificação da pessoa
#        SIT_FAM, # Situação Familiar: 1 - Responsável; 2 - Cônjuge/companheiro(a); 3 - Filho(a)/Enteado(a); 4 - Outro Parente; 5 - Empregado Residente; 6 - Visitante não residente
#        IDADE,
#       SEXO,
#        SE_ESTUD, # 1 - Não; 2 - Creche/Pré-Escola; 3 - Ensino Básico/Fundamental/Médio; 4 - Outros
#        GRAU_INS, # Grau de Instrução: 1 - Não Alfabetizado; 2 - Pré-Escola; 3 - Fundamental Incompleto; 4 - Fundamental Completo; 5 - Médio Incompleto; 6 - Médio Completo; 7 - Superior Incompleto; 8 - Superior Completo
#        CD_ATIVI, # Condição de atividade: 1 - Trabalho Regular; 2 - Bico; 3 - Licença Médica; 4 - Desempregado; 5 - Aposentado/Pensionista; 6 - Nunca trabalhou; 7 - Dona de Casa; 8 - Estudante
#        VL_REN_I, # Renda Individual
#        ZONATRA1, # Zona de Trabalho 1
#        MUNITRA1, # Município de Trabalho 1
#        SET_ATIV, # Setor do Trabalho 1
#        OCUP_PRI, # Vínculo do Trabalho 1
#        MODO1, # Modo de Transporte da Viagem 1
#        DURACAO, # Duração da Viagem (MINUTOS)
#        MODOPRIN, # Modo Principal da Viagem
#        ZONA_O, # Zona de Origem da Viagem
#        ZONA_D, # Zona de Destino da Viagem
#        MOTIVO_O, # Motivo da Viagem de Origem: 1 - Trabalho/Indústria; 2 - Trabalho/Comércio; 3 - Trabalho/Serviços; 4 - Educação; 5 - Compras; 6 - Saúde; 7 - Lazer; 8 - Residência; 9 - Outros
#        MOTIVO_D # Motivo da Viagem de Origem: 1 - Trabalho/Indústria; 2 - Trabalho/Comércio; 3 - Trabalho/Serviços; 4 - Educação; 5 - Compras; 6 - Saúde; 7 - Lazer; 8 - Residência; 9 - Outros
#    ) |>
#    mutate(
#        ano = 1997,
#        TOT_VIAG = NA,
#       CO_DOM_X = NA,
#       CO_DOM_Y = NA,
#        CO_TR1_X = NA,
#        CO_TR1_Y = NA,
#       DISTANCIA = NA,
#        CO_O_X = NA,
#        CO_O_Y = NA,
#        CO_D_X = NA,
#        CO_D_Y = NA,
#        ID_DOM = as.character(ID_DOM)
#    )

# Problemas: 1997 não possui coordenadas, critério econômico antigo, grau de instrução limitado, sem total de viagens, ocupação do trabalho 1 e setor do trabalho 1 diferentes

## Trocando nomes diferentes da base de 1997

#od_1997_filtrada <- od_1997_filtrada |>
#    rename(
#        CRITERIOBR = ABIPEME,
#        ESTUDA = SE_ESTUD,
#       VINC1 = OCUP_PRI,
#       SETOR1 = SET_ATIV
#    )

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
        MOTIVO_D, # Motivo da Viagem de Origem: 1 - Trabalho/Indústria; 2 - Trabalho/Comércio; 3 - Trabalho/Serviços; 4 - Educação; 5 - Compras; 6 - Saúde; 7 - Lazer; 8 - Residência; 9 - Procurar Emprego; 10 - Assuntos Pessoais,
        TIPVG # Tipo de Viagem: 1 - Coletivo; 2 - Individual; 3 - A pé; 4 - Bicicleta
    ) |>
    mutate(
        ano = 2007,
        VL_REN_I = as.numeric(VL_REN_I),
        CRITERIOBR = case_when(
            CRITERIOBR %in% c(1, 2) ~ 1,
            CRITERIOBR == 3 ~ 2,
            CRITERIOBR == 4 ~ 3,
            CRITERIOBR == 5 ~ 4,
            CRITERIOBR == 6 ~ 5,
            CRITERIOBR %in% c(7, 8) ~ 6
        ),
        ESTUDA = case_when(
            ESTUDA == 6 ~ 5,
            ESTUDA == 7 ~ 6,
            TRUE ~ ESTUDA
        ),
        MODOPRIN = case_when(
            MODOPRIN %in% c(1, 9) ~ 4,
            MODOPRIN %in% c(2, 10) ~ 5,
            MODOPRIN %in% c(3, 11) ~ 6,
            MODOPRIN == 4 ~ 7,
            MODOPRIN == 5 ~ 8,
            MODOPRIN == 6 ~ 9,
            MODOPRIN == 7 ~ 10,
            MODOPRIN == 8 ~ 11,
            MODOPRIN == 12 ~ 1,
            MODOPRIN == 13 ~ 2,
            MODOPRIN == 14 ~ 13,
            TRUE ~ MODOPRIN
        )
    )

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
        MOTIVO_D, # Motivo da Viagem de Origem: 1 - Trabalho/Indústria; 2 - Trabalho/Comércio; 3 - Trabalho/Serviços; 4 - Educação; 5 - Compras; 6 - Saúde; 7 - Lazer; 8 - Residência; 9 - Procurar Emprego; 10 - Assuntos Pessoais
        TIPVG # Tipo de Viagem: 1 - Coletivo; 2 - Individual; 3 - A pé; 4 - Bicicleta
    ) |>
    mutate(
        ano = 2017,
        CO_O_X_SIRGAS = CO_O_X,
        CO_O_Y_SIRGAS = CO_O_Y,
        CO_D_X_SIRGAS = CO_D_X,
        CO_D_Y_SIRGAS = CO_D_Y,
        CO_DOM_X_SIRGAS = CO_DOM_X,
        CO_DOM_Y_SIRGAS = CO_DOM_Y,
        MODOPRIN = case_when(
            MODOPRIN == 3 ~ 17,
            MODOPRIN == 12 ~ 11,
            MODOPRIN == 14 ~ 13,
            TRUE ~ MODOPRIN
        )
    )


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
        MOTIVO_D, # Motivo da Viagem de Origem: 1 - Trabalho/Indústria; 2 - Trabalho/Comércio; 3 - Trabalho/Serviços; 4 - Educação; 5 - Compras; 6 - Saúde; 7 - Lazer; 8 - Residência; 9 - Procurar Emprego; 10 - Assuntos Pessoais
        TIPVG # Tipo de Viagem: 1 - Coletivo; 2 - Individual; 3 - A pé; 4 - Bicicleta
    ) |>
    mutate(
        ano = 2023,
        ID_PESS = as.character(ID_PESS),
        VL_REN_I = as.numeric(VL_REN_I),
        CO_O_X_SIRGAS = CO_O_X,
        CO_O_Y_SIRGAS = CO_O_Y,
        CO_D_X_SIRGAS = CO_D_X,
        CO_D_Y_SIRGAS = CO_D_Y,
        CO_DOM_X_SIRGAS = CO_DOM_X,
        CO_DOM_Y_SIRGAS = CO_DOM_Y,
        VINC1 = case_when(
            VINC1 %in% c(5, 6) ~ 4,
            VINC1 == 4 ~ 6,
            VINC1 == 7 ~ 5,
            VINC1 == 8 ~ 7,
            VINC1 == 9 ~ 8,
            TRUE ~ VINC1
        ),
        MODOPRIN = case_when(
            MODOPRIN == 3 ~ 17,
            MODOPRIN == 12 ~ 11,
            MODOPRIN == 14 ~ 13,
            MODOPRIN == 15 ~ 13,
            MODOPRIN == 16 ~ 15,
            MODOPRIN == 17 ~ 16,
            MODOPRIN == 18 ~ 17,
            TRUE ~ MODOPRIN
        )
    )

##### Arrumando diferenças entre as pesquisas de cada base #####

### Em 1997 e 2007, a variável de tipo de domicílio engloba favelas como um tipo diferente, enquanto as outras não fazem a separação. Vamos separá-las como domicílios particulares ###

#od_1997_filtrada <- od_1997_filtrada |>
#    mutate(
#        TIPO_DOM = ifelse(TIPO_DOM == 3, 1, TIPO_DOM)
#    )

od_2007_filtrada <- od_2007_filtrada |>
    mutate(
        TIPO_DOM = ifelse(TIPO_DOM == 3, 1, TIPO_DOM)
    )

### Em 1997, o grau de instrução estava mais detalhado. Vamos adequá-lo às outras pesquisas ###

#od_1997_filtrada <- od_1997_filtrada |>
#    mutate(
#        GRAU_INS_ANTIGO = GRAU_INS,
#       GRAU_INS = case_when(
#           GRAU_INS %in% c(1L, 2L, 3L) ~ 1L, # não alfabetizado / pré-escola / fund. incompleto
#           GRAU_INS == 4L ~ 2L, # fund. completo
#           GRAU_INS == 5L ~ 3L, # médio incompleto
#           GRAU_INS %in% c(6L, 7L) ~ 4L, # médio completo / superior incompleto
#           GRAU_INS == 8L ~ 5L, # superior completo
#           TRUE ~ NA_integer_
#      )
#   )

##### Fazendo uma variável indicadora se a viagem dos indivíduos foi realizada ao centro expandido de SP #####

centro_exp_shp <- st_read(
    'Shapefile centro exp SP/SIRGAS_SHP_restricaoveiculomian.shp'
)

centro_exp_shp <- centro_exp_shp |>
    st_set_crs(31983)

#### Transformando as coordenadas da base de 2007 de SAD69 para SIRGAS ####

## Coordenadas de origem da viagem ##

od_2007_filtrada <- od_2007_filtrada |>
    mutate(id_temp_row = row_number())

od_2007_coord_origem <- od_2007_filtrada |>
    filter(!is.na(CO_O_X) & !is.na(CO_O_Y)) |>
    select(id_temp_row, CO_O_X, CO_O_Y) |>
    st_as_sf(coords = c('CO_O_X', 'CO_O_Y'), crs = 29193, remove = FALSE) |>
    st_transform(crs = 31983) |>
    mutate(
        CO_O_X_SIRGAS = st_coordinates(geometry)[, 1],
        CO_O_Y_SIRGAS = st_coordinates(geometry)[, 2]
    ) |>
    st_drop_geometry() |>
    select(id_temp_row, CO_O_X_SIRGAS, CO_O_Y_SIRGAS)

od_2007_filtrada <- od_2007_filtrada |>
    left_join(od_2007_coord_origem, by = 'id_temp_row')

## Coordenadas de destino da viagem ##

od_2007_filtrada <- od_2007_filtrada |>
    mutate(id_temp_row = row_number())

od_2007_coord_destino <- od_2007_filtrada |>
    filter(!is.na(CO_D_X) & !is.na(CO_D_Y)) |>
    select(id_temp_row, CO_D_X, CO_D_Y) |>
    st_as_sf(coords = c('CO_D_X', 'CO_D_Y'), crs = 29193, remove = FALSE) |>
    st_transform(crs = 31983) |>
    mutate(
        CO_D_X_SIRGAS = st_coordinates(geometry)[, 1],
        CO_D_Y_SIRGAS = st_coordinates(geometry)[, 2]
    ) |>
    st_drop_geometry() |>
    select(id_temp_row, CO_D_X_SIRGAS, CO_D_Y_SIRGAS)

od_2007_filtrada <- od_2007_filtrada |>
    left_join(od_2007_coord_destino, by = 'id_temp_row')


## Coordenadas do domicílio ##

od_2007_filtrada <- od_2007_filtrada |>
    mutate(id_temp_row = row_number())

od_2007_coord_dom <- od_2007_filtrada |>
    filter(!is.na(CO_DOM_X) & !is.na(CO_DOM_Y)) |>
    select(id_temp_row, CO_DOM_X, CO_DOM_Y) |>
    st_as_sf(coords = c('CO_DOM_X', 'CO_DOM_Y'), crs = 29193, remove = FALSE) |>
    st_transform(crs = 31983) |>
    mutate(
        CO_DOM_X_SIRGAS = st_coordinates(geometry)[, 1],
        CO_DOM_Y_SIRGAS = st_coordinates(geometry)[, 2]
    ) |>
    st_drop_geometry() |>
    select(id_temp_row, CO_DOM_X_SIRGAS, CO_DOM_Y_SIRGAS)

od_2007_filtrada <- od_2007_filtrada |>
    left_join(od_2007_coord_dom, by = 'id_temp_row')

#### Criando variável indicadora do destino da viagem ser no centro expandido ####

### 2007 ###

bool_centro_exp_07 <- od_2007_filtrada |>
    filter(!is.na(CO_D_X_SIRGAS) & !is.na(CO_D_Y_SIRGAS)) |>
    select(id_temp_row, CO_D_X_SIRGAS, CO_D_Y_SIRGAS) |>
    st_as_sf(
        coords = c('CO_D_X_SIRGAS', 'CO_D_Y_SIRGAS'),
        crs = 31983,
        remove = FALSE
    ) |>
    mutate(
        bool_destino_centro_exp = lengths(st_intersects(
            geometry,
            centro_exp_shp
        )) >
            0
    ) |>
    st_drop_geometry() |>
    select(id_temp_row, bool_destino_centro_exp)

od_2007_filtrada <- od_2007_filtrada |>
    left_join(bool_centro_exp_07, by = 'id_temp_row') |>
    mutate(
        indic_destino_centro_exp = ifelse(
            is.na(bool_destino_centro_exp),
            0,
            ifelse(bool_destino_centro_exp == TRUE, 1, 0)
        )
    ) |>
    select(-id_temp_row)

### 2017 ###

od_2017_filtrada <- od_2017_filtrada |>
    mutate(id_temp_row = row_number())

bool_centro_exp_17 <- od_2017_filtrada |>
    filter(!is.na(CO_D_X_SIRGAS) & !is.na(CO_D_Y_SIRGAS)) |>
    select(id_temp_row, CO_D_X_SIRGAS, CO_D_Y_SIRGAS) |>
    st_as_sf(
        coords = c('CO_D_X_SIRGAS', 'CO_D_Y_SIRGAS'),
        crs = 31983,
        remove = FALSE
    ) |>
    mutate(
        bool_destino_centro_exp = lengths(st_intersects(
            geometry,
            centro_exp_shp
        )) >
            0
    ) |>
    st_drop_geometry() |>
    select(id_temp_row, bool_destino_centro_exp)

od_2017_filtrada <- od_2017_filtrada |>
    left_join(bool_centro_exp_17, by = 'id_temp_row') |>
    mutate(
        indic_destino_centro_exp = ifelse(
            is.na(bool_destino_centro_exp),
            0,
            ifelse(bool_destino_centro_exp == TRUE, 1, 0)
        )
    ) |>
    select(-id_temp_row)

### 2023 ###

od_2023_filtrada <- od_2023_filtrada |>
    mutate(id_temp_row = row_number())

bool_centro_exp_23 <- od_2023_filtrada |>
    filter(!is.na(CO_D_X_SIRGAS) & !is.na(CO_D_Y_SIRGAS)) |>
    select(id_temp_row, CO_D_X_SIRGAS, CO_D_Y_SIRGAS) |>
    st_as_sf(
        coords = c('CO_D_X_SIRGAS', 'CO_D_Y_SIRGAS'),
        crs = 31983,
        remove = FALSE
    ) |>
    mutate(
        bool_destino_centro_exp = lengths(st_intersects(
            geometry,
            centro_exp_shp
        )) >
            0
    ) |>
    st_drop_geometry() |>
    select(id_temp_row, bool_destino_centro_exp)

od_2023_filtrada <- od_2023_filtrada |>
    left_join(bool_centro_exp_23, by = 'id_temp_row') |>
    mutate(
        indic_destino_centro_exp = ifelse(
            is.na(bool_destino_centro_exp),
            0,
            ifelse(bool_destino_centro_exp == TRUE, 1, 0)
        )
    ) |>
    select(-id_temp_row)

##### Unindo todas as bases filtradas #####

od_completa <- bind_rows(
    #od_1997_filtrada,
    od_2007_filtrada,
    od_2017_filtrada,
    od_2023_filtrada
)

#### Criando variáveis relacionando a viagem ao centro ao motivo da viagem ####

od_completa <- od_completa |>
    mutate(
        trab_centro = ifelse(
            indic_destino_centro_exp == 1 & MOTIVO_D %in% c(1, 2, 3),
            1,
            0
        ),
        educ_centro = ifelse(
            indic_destino_centro_exp == 1 & MOTIVO_D == 4,
            1,
            0
        ),
        saude_centro = ifelse(
            indic_destino_centro_exp == 1 & MOTIVO_D == 6,
            1,
            0
        ),
        lazer_centro = ifelse(
            indic_destino_centro_exp == 1 & MOTIVO_D == 7,
            1,
            0
        ),
        compras_centro = ifelse(
            indic_destino_centro_exp == 1 & MOTIVO_D == 5,
            1,
            0
        ),
        emp_centro = ifelse(
            indic_destino_centro_exp == 1 & MOTIVO_D == 9,
            1,
            0
        ),
        trab_educ_emp_centro = ifelse(
            indic_destino_centro_exp == 1 & MOTIVO_D %in% c(1, 2, 3, 4, 9),
            1,
            0
        ),
        trab_emp_centro = ifelse(
            indic_destino_centro_exp == 1 & MOTIVO_D %in% c(1, 2, 3, 9),
            1,
            0
        ),
        lazer_comp_saude_centro = ifelse(
            indic_destino_centro_exp == 1 & MOTIVO_D %in% c(5, 6, 7),
            1,
            0
        ),
        lazer_comp_centro = ifelse(
            indic_destino_centro_exp == 1 & MOTIVO_D %in% c(5, 7),
            1,
            0
        ),
        trab_educ_centro = ifelse(
            indic_destino_centro_exp == 1 & MOTIVO_D %in% c(1, 2, 3, 4),
            1,
            0
        )
    )

##### Definindo o grupo de tratamento e grupos de controle paramétricos #####

#### Grupo de Tratamento: Indivíduos que, em 2007, moravam em domicílios contemplados pela criação da Linha 5 - Lilás do Metrô de SP ####
### As zonas da época contempladas, segundo o mapa da OD, são: 284, 285, 286, 292, 294, 300, 301 e 302 ###

## Abrindo o shapefile das zonas da OD de 2007 ##

zonas_2007 <- st_read('Shapefiles OD/Zonas2007_region.shp')

zonas_2007 <- zonas_2007 |>
    st_transform(31983)

## Filtrando as zonas tratadas e criando um polígono a partir delas ##

zonas_tratadas_2007 <- zonas_2007 |>
    filter(Zona07 %in% c(284, 285, 286, 292, 294, 300, 301, 302)) |>
    st_union() |>
    st_as_sf() |>
    mutate(tipo_regiao = 'Região Tratada')

#### Grupo de Controle Paramétrico 1: Indivíduos que residem em áreas que, futuramente, serão contempladas pelas linhas 6 - Laranja, 16 - Violeta, 19 - Celeste e 22 - Marrom ####

### Listando as estações futuras ###

estacoes_futuras <- tribble(
    ~linha    , ~nome_estacao      , ~lat     , ~lon     ,

    # Linha 6 - Laranja
    "Laranja" , "Brasilândia"      , -23.4673 , -46.6946 ,
    "Laranja" , "Vila Cardoso"     , -23.4764 , -46.6976 , # Antiga Maristela
    "Laranja" , "Itaberaba"        , -23.4839 , -46.7011 ,
    "Laranja" , "João Paulo I"     , -23.4962 , -46.7029 ,
    "Laranja" , "Freguesia do Ó"   , -23.4884 , -46.7032 ,
    "Laranja" , "Santa Marina"     , -23.5085 , -46.6934 ,
    "Laranja" , "Água Branca"      , -23.5159 , -46.6836 ,
    "Laranja" , "SESC-Pompeia"     , -23.5264 , -46.6816 ,
    "Laranja" , "Perdizes"         , -23.5354 , -46.6738 ,
    "Laranja" , "PUC-Cardoso"      , -23.5367 , -46.6687 ,
    "Laranja" , "FAAP-Pacaembu"    , -23.5434 , -46.6598 ,
    "Laranja" , "Higienópolis"     , -23.5489 , -46.6522 ,
    "Laranja" , "14 Bis"           , -23.5558 , -46.6457 ,
    "Laranja" , "Bela Vista"       , -23.5594 , -46.6416 ,
    "Laranja" , "São Joaquim"      , -23.5615 , -46.6385 ,

    ## Coordenadas definidas a partir dos locais reais das obras da estação

    # Linha 19 - Celeste
    "Celeste" , "Bosque Maia"      , -23.4474 , -46.5222 ,
    "Celeste" , "Guarulhos Centro" , -23.4635 , -46.5332 ,
    "Celeste" , "Vila Augusta"     , -23.4705 , -46.5458 ,
    "Celeste" , "Dutra"            , -23.4754 , -46.5516 ,
    "Celeste" , "Itapegica"        , -23.4851 , -46.5574 ,
    "Celeste" , "Jardim Julieta"   , -23.4925 , -46.5656 ,
    "Celeste" , "Vila Sabrina"     , -23.5012 , -46.5723 ,
    "Celeste" , "Cerejeiras"       , -23.4981 , -46.5784 ,
    "Celeste" , "Vila Maria"       , -23.5132 , -46.5910 ,
    "Celeste" , "Catumbi"          , -23.5324 , -46.6021 ,
    "Celeste" , "Silva Teles"      , -23.5365 , -46.6095 ,
    "Celeste" , "Cerealista"       , -23.5422 , -46.6231 ,
    "Celeste" , "São Bento"        , -23.5445 , -46.6339 ,
    "Celeste" , "Anhangabaú"       , -23.5492 , -46.6386 ,

    ## Coordenadas definidas a partir dos cruzamentos viários principais dos nomes das estações

    # Linha 16 - Violeta
    "Violeta" , "Oscar Freire"     , -23.5605 , -46.6716 ,
    "Violeta" , "Nove de Julho"    , -23.5668 , -46.6622 ,
    "Violeta" , "Jardim Paulista"  , -23.5714 , -46.6558 ,
    "Violeta" , "Ibirapuera"       , -23.5768 , -46.6514 , # Pq Ibirapuera
    "Violeta" , "Ana Rosa"         , -23.5816 , -46.6384 ,
    "Violeta" , "Aclimação"        , -23.5721 , -46.6242 ,
    "Violeta" , "Independência"    , -23.5804 , -46.6115 , # Pq Independencia
    "Violeta" , "São Carlos"       , -23.5683 , -46.6025 ,
    "Violeta" , "Paes de Barros"   , -23.5635 , -46.5886 ,
    "Violeta" , "Vila Bertioga"    , -23.5612 , -46.5784 ,
    "Violeta" , "Anália Franco"    , -23.5583 , -46.5623 ,
    "Violeta" , "Abel Ferreira"    , -23.5601 , -46.5452 ,

    ## Coordenadas definidas pelos centróides dos bairros ou regiões de referência da estação

    # Linha 22 - Marrom
    "Marrom"  , "Cotia Centro"     , -23.6033 , -46.9192 ,
    "Marrom"  , "Santo Antônio"    , -23.5992 , -46.8854 ,
    "Marrom"  , "Sabiá"            , -23.5965 , -46.8643 ,
    "Marrom"  , "Mesopotâmia"      , -23.5942 , -46.8456 , # Pq Alexandra/Embu
    "Marrom"  , "Granja Viana"     , -23.5935 , -46.8335 ,
    "Marrom"  , "Santa Maria"      , -23.5898 , -46.8152 ,
    "Marrom"  , "Victor Civita"    , -23.5864 , -46.7956 ,
    "Marrom"  , "Jardim Boa Vista" , -23.5822 , -46.7750 ,
    "Marrom"  , "Monte Belo"       , -23.5765 , -46.7589 ,
    "Marrom"  , "Rio Pequeno"      , -23.5724 , -46.7456 ,
    "Marrom"  , "USP"              , -23.5662 , -46.7324 ,
    "Marrom"  , "Vital Brasil"     , -23.5689 , -46.7125 ,
    "Marrom"  , "Faria Lima"       , -23.5677 , -46.6934 ,
    "Marrom"  , "Sumaré"           , -23.5516 , -46.6775

    ## Coordenadas definidas pelos centróides dos bairros ou regiões de referência da estação
)

### Convertendo as coordenadas em shapefile ###

shp_estacoes_futuras <- estacoes_futuras |>
    st_as_sf(coords = c('lon', 'lat'), crs = 4674) |>
    st_transform(31983)

#### Grupo de Controle Paramétrico 2: Indivíduos que residem em áreas que já são contempladas por estações de trem da CPTM até 2017, ano antes da conexão da linha lilás ####

### Abrindo o shapefile das estações mais recentes ###

cptm_atual <- st_read(
    'Shapefile estações trem SP/SIRGAS_SHP_estacaotrem.shp'
) |>
    st_set_crs(31983)

### Filtrando as estações que não existiam em 2017 ###

nomes <- c('MENDES-VILA NATAL', 'JOÃO DIAS')

estacoes_2017 <- cptm_atual |>
    filter(!etr_linha == 'JADE') |>
    filter(!etr_nome %in% nomes) |>
    select(etr_nome, etr_linha, geometry)

#### Criando uma distância limite para definir os indivíduos de cada grupo controle ####

limite_dist <- 1000 # 1 km

#### Calculando as distâncias para cada grupo ####

od_grupos_sf <- od_completa |>
    filter(!is.na(CO_DOM_X_SIRGAS) & !is.na(CO_DOM_Y_SIRGAS)) |>
    st_as_sf(
        coords = c('CO_DOM_X_SIRGAS', 'CO_DOM_Y_SIRGAS'),
        crs = 31983,
        remove = FALSE
    )

### Grupo de Controle Paramétrico 1: Linhas Futuras ###

indica_futuro <- st_nearest_feature(
    od_grupos_sf,
    shp_estacoes_futuras
)

dist_futuro <- st_distance(
    od_grupos_sf,
    shp_estacoes_futuras[indica_futuro, ],
    by_element = TRUE
)

### Grupo de Controle Paramétrico 2: CPTM ###

indica_cptm <- st_nearest_feature(
    od_grupos_sf,
    estacoes_2017
)

dist_cptm <- st_distance(
    od_grupos_sf,
    estacoes_2017[indica_cptm, ],
    by_element = TRUE
)

### Criando variável indicadora se o indivíduo mora no centro expandido ###

indica_centro <- st_intersects(
    od_grupos_sf,
    centro_exp_shp,
    sparse = FALSE
)

#### Criando a base final com as dummies que definem cada grupo ####

od_grupos <- od_grupos_sf |>
    st_join(zonas_tratadas_2007) |>
    mutate(
        dist_m_futuro = as.numeric(dist_futuro),
        dist_m_cptm = as.numeric(dist_cptm),
        mora_centro_exp = ifelse(indica_centro[, 1] == TRUE, 1, 0),
        tipo_grupo = case_when(
            ZONA %in% c(284, 285, 286, 292, 294, 300, 301, 302) ~ 'Tratamento',
            !(ZONA %in% c(284, 285, 286, 292, 294, 300, 301, 302)) &
                dist_m_futuro <= limite_dist &
                mora_centro_exp == 0 ~ 'Controle_Linhas_Futuras',
            !(ZONA %in% c(284, 285, 286, 292, 294, 300, 301, 302)) &
                dist_m_cptm <= limite_dist &
                mora_centro_exp == 0 ~ 'Controle_CPTM',
            mora_centro_exp == 1 ~ 'Morador_Centro_Exp',
            TRUE ~ 'Candidatos_Controle_MatchIt'
        )
    ) |>
    st_drop_geometry()

##### Deflacionando a renda individual #####

od_grupos <- od_grupos |>
    mutate(
        data_original = case_when(
            ano == 2007 ~ '10/2007',
            ano == 2017 ~ '09/2017',
            ano == 2023 ~ '09/2023',
            TRUE ~ NA_character_
        ),
        VL_REN_I_DEF = ifelse(
            !is.na(VL_REN_I) & !is.na(data_original),
            deflate(
                nominal_values = VL_REN_I,
                nominal_dates = as.Date(
                    paste0('01/', data_original),
                    format = '%d/%m/%Y'
                ),
                real_date = '11/2025',
                index = 'ipca'
            ),
            VL_REN_I
        )
    )

##### Filtrando os indivíduos que possuem como motivo da origem da viagem somente residência #####

od_grupos <- od_grupos |>
    filter(MOTIVO_O == 8)

##### Decompondo a variável indicadora de viagem com destino ao centro (indic_dest) em viagens realizadas de metrô e de carro #####

od_grupos <- od_grupos |>
    mutate(
        metro = ifelse(indic_destino_centro_exp == 1 & MODOPRIN == 1, 1, 0),
        carro = ifelse(
            indic_destino_centro_exp == 1 & MODOPRIN %in% c(9, 10),
            1,
            0
        )
    )

##### Salvando a base final #####

export(od_grupos, 'od_base_completa.dbf')
