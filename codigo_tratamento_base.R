###### Tratamento das bases da OD ######

rm(list = ls())

##### Abrindo Bibliotecas #####

library(tidyverse)
library(rio)
library(sf)
library(deflateBR)
library(igraph)
library(lwgeom)

##### Importando base de dados #####

#od_1997 <- import('OD97Zona.dbf')

od_2007 <- import('Bases de dados OD/OD_2007_v2d.dbf')

od_2017 <- import('Bases de dados OD/OD_2017_v1.dbf')

od_2023 <- import('Bases de dados OD/Banco2023_divulgacao_190225.dbf')

##### Filtrando as variáveis importantes de cada base #####

od_2007_filtrada <- od_2007 |>
    select(
        ZONA, # Zona do domicílio
        CO_DOM_X, # Coordenada X do domicílio
        CO_DOM_Y, # Coordenada Y do domicílio
        ID_PESS, # Identificação da pessoa
        IDADE,
        SEXO,
        NO_MORAF, # Número de moradores da família
        VL_REN_I, # Renda Individual
        GRAU_INS, # Grau de Instrução: 1 - Não Alfabetizado/ Fund 1 Incompleto ; 2 - Fund 1 Completo/Fund 2 Incompleto; 3 - Fund 2 Completo/ Médio Incompleto; 4 - Médio Completo/ Superior Incompleto; 5 - Superior Completo
        CD_ATIVI, # Condição de atividade: 1 - Trabalho Regular; 2 - Bico; 3 - Licença Médica; 4 - Aposentado/Pensionista; 5 - Desempregado; 6 - Nunca trabalhou; 7 - Dona de Casa; 8 - Estudante
        RENDA_FA, # Renda Familiar
        TOT_VIAG, # Total de Viagens do Indivíduo
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
        ano = 2007,
        RENDA_FA = as.numeric(RENDA_FA),
        VL_REN_I = as.numeric(VL_REN_I),
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
        ),
        declarou_renda = ifelse(!is.na(VL_REN_I) & VL_REN_I > 0, 1, 0)
    ) |>
    # Filtrando as viagens para: Viagens que começam em casa (MOTIVO_O == 8) e as pessoas que NÃO VIAJARAM (MOTIVO_O == NA)
    filter(MOTIVO_O == 8 | is.na(MOTIVO_O)) |>
    # Filtrando as pessoas com menos de 14 anos
    filter(IDADE >= 14)


od_2017_filtrada <- od_2017 |>
    select(
        ZONA, # Zona do domicílio
        CO_DOM_X, # Coordenada X do domicílio
        CO_DOM_Y, # Coordenada Y do domicílio
        ID_PESS, # Identificação da pessoa
        IDADE,
        SEXO,
        NO_MORAF, # Número de moradores da família
        VL_REN_I, # Renda Individual
        GRAU_INS, # Grau de Instrução: 1 - Não Alfabetizado/ Fund 1 Incompleto ; 2 - Fund 1 Completo/Fund 2 Incompleto; 3 - Fund 2 Completo/ Médio Incompleto; 4 - Médio Completo/ Superior Incompleto; 5 - Superior Completo
        CD_ATIVI, # Condição de atividade: 1 - Trabalho Regular; 2 - Bico; 3 - Licença Médica; 4 - Aposentado/Pensionista; 5 - Desempregado; 6 - Nunca trabalhou; 7 - Dona de Casa; 8 - Estudante
        RENDA_FA, # Renda Familiar
        TOT_VIAG, # Total de Viagens do Indivíduo
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
        VL_REN_I = as.numeric(VL_REN_I),
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
        ),
        declarou_renda = ifelse(!is.na(VL_REN_I) & VL_REN_I > 0, 1, 0)
    ) |>
    # Filtrando as viagens para: Viagens que começam em casa (MOTIVO_O == 8) e as pessoas que NÃO VIAJARAM (MOTIVO_O == NA)
    filter(MOTIVO_O == 8 | is.na(MOTIVO_O)) |>
    # Filtrando as pessoas com menos de 14 anos
    filter(IDADE >= 14)


od_2023_filtrada <- od_2023 |>
    select(
        ZONA, # Zona do domicílio
        CO_DOM_X, # Coordenada X do domicílio
        CO_DOM_Y, # Coordenada Y do domicílio
        ID_PESS, # Identificação da pessoa
        IDADE,
        SEXO,
        NO_MORAF, # Número de moradores da família
        VL_REN_I, # Renda Individual
        GRAU_INS, # Grau de Instrução: 1 - Não Alfabetizado/ Fund 1 Incompleto ; 2 - Fund 1 Completo/Fund 2 Incompleto; 3 - Fund 2 Completo/ Médio Incompleto; 4 - Médio Completo/ Superior Incompleto; 5 - Superior Completo
        CD_ATIVI, # Condição de atividade: 1 - Trabalho Regular; 2 - Bico; 3 - Licença Médica; 4 - Aposentado/Pensionista; 5 - Desempregado; 6 - Nunca trabalhou; 7 - Dona de Casa; 8 - Estudante
        RENDA_FA, # Renda Familiar
        TOT_VIAG, # Total de Viagens do Indivíduo
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
        RENDA_FA = as.numeric(RENDA_FA),
        VL_REN_I = as.numeric(VL_REN_I),
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
            MODOPRIN == 15 ~ 13,
            MODOPRIN == 16 ~ 15,
            MODOPRIN == 17 ~ 16,
            MODOPRIN == 18 ~ 17,
            TRUE ~ MODOPRIN
        ),
        declarou_renda = ifelse(!is.na(VL_REN_I) & VL_REN_I > 0, 1, 0)
    ) |>
    # Filtrando as viagens para: Viagens que começam em casa (MOTIVO_O == 8) e as pessoas que NÃO VIAJARAM (MOTIVO_O == NA)
    filter(MOTIVO_O == 8 | is.na(MOTIVO_O)) |>
    # Filtrandoa as pessoas com menos de 14 anos
    filter(IDADE >= 14)

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

### Como as zonas mudam conforme os anos, importante criar "Zonas mínimas comparáveis" ###

## Abrindo o shapefile das zonas das ODs ##

zonas_2007 <- st_read('Shapefiles OD/Zonas2007_region.shp')

zonas_2007 <- zonas_2007 |>
    st_zm(drop = TRUE) |>
    st_transform(31983)

z07 <- zonas_2007 |>
    select(ZONA = Zona07) |>
    mutate(id_poly = paste0("2007_", ZONA), area_orig = st_area(geometry)) |>
    st_make_valid()

z17 <- st_read('Shapefiles OD/Zonas_2017_region.shp') |>
    st_zm(drop = TRUE) |>
    st_transform(31983) |>
    select(ZONA = NumeroZona) |>
    mutate(id_poly = paste0("2017_", ZONA), area_orig = st_area(geometry)) |>
    st_make_valid()

z23 <- st_read('Shapefiles OD/Zonas_2023.shp') |>
    st_zm(drop = TRUE) |>
    st_transform(31983) |>
    select(ZONA = NumeroZona) |>
    mutate(id_poly = paste0("2023_", ZONA), area_orig = st_area(geometry)) |>
    st_make_valid()

### Criando uma função para encontrar as zonas que se sobrepõem em diferentes anos ###

links <- function(shp_a, shp_b) {
    inter <- st_intersection(shp_a, shp_b)
    inter$area_inter <- st_area(inter)

    inter |>
        st_drop_geometry() |>
        filter(
            as.numeric(area_inter / area_orig) > 0.05 |
                as.numeric(area_inter / area_orig.1) > 0.05
        ) |>
        select(id_poly, id_poly.1)
}

links_07_17 <- links(z07, z17)
links_17_23 <- links(z17, z23)
links_07_23 <- links(z07, z23)

arestas <- bind_rows(links_07_17, links_17_23, links_07_23)

### Criando um grafo pelas arestas e encontrar a ZMC ###

grafo <- graph_from_data_frame(arestas, directed = FALSE)
zmc_grupos <- components(grafo)$membership

### Fazendo o de-para ###

de_para_zmc <- data.frame(
    id_poly = names(zmc_grupos),
    ZMC = as.integer(zmc_grupos)
) |>
    separate(id_poly, into = c("ano", "ZONA"), sep = "_", remove = FALSE) |>
    mutate(
        ano = as.numeric(ano),
        ZONA = as.numeric(ZONA)
    ) |>
    select(ano, ZONA, ZMC)

od_completa <- od_completa |>
    left_join(de_para_zmc, by = c("ano", "ZONA"))

#### Definindo o grupo de tratamento a partir das ZMCs e da linha 5 - Lilás que existia em 2007 ####

### Abrindo os shapefiles da linha 5 - Lilás ###

shp_linhas <- st_read(
    "Shapefiles estações metro SP/SIRGAS_SHP_linhametro_line.shp"
) |>
    st_set_crs(31983)

shp_estac <- st_read(
    "Shapefiles estações metro SP/SIRGAS_SHP_estacaometro.shp"
) |>
    st_set_crs(31983)

shp_lilas <- shp_linhas |>
    filter(str_detect(lmt_nome, "LILAS"))

### Manter somente as extensões das linhas que existiam em 2007 ###

est_largo_treze <- shp_estac |>
    filter(str_detect(emt_nome, "LARGO TREZE"))

corte_linha <- function(linha, est_limite) {
    tesoura <- st_buffer(est_limite, dist = 10) |> st_union()

    linha_cortada <- st_difference(linha, tesoura)

    linha_fatiada <- st_cast(linha_cortada, "LINESTRING") |>
        st_as_sf()

    return(linha_fatiada)
}

lilas_cortada <- corte_linha(shp_lilas, est_largo_treze)

lilas_2007 <- lilas_cortada[2, ]

ggplot() + geom_sf(data = lilas_2007, color = "purple", size = 2) + theme_void()

outras_linhas <- shp_linhas |>
    filter(!str_detect(lmt_nome, "LILAS"))

### Criando os polígonos específicos de cada ZMC ###

zonas_completo <- bind_rows(
    z07 |>
        mutate(ano = 2007),
    z17 |>
        mutate(ano = 2017),
    z23 |>
        mutate(ano = 2023)
) |>
    left_join(de_para_zmc, by = c("ano", "ZONA")) |>
    st_make_valid()

zmc_polygons <- zonas_completo |>
    group_by(ZMC) |>
    summarise(geometry = st_union(geometry)) |>
    st_make_valid()

### Filtrando as ZMCs que se intersectam com a linha 5 - Lilás e as que se intersectam com outras linhas ###

zmc_toca_lilas_07 <- st_filter(
    zmc_polygons,
    lilas_2007,
    .predicate = st_intersects
)

zmc_toca_outras <- st_filter(
    zmc_polygons,
    outras_linhas,
    .predicate = st_intersects
)

### Criando o grupo de ZMCs que definem o grupo de tratamento ###

zmcs_tratadas <- zmc_toca_lilas_07 |>
    filter(!ZMC %in% zmc_toca_outras$ZMC) |>
    pull(ZMC)

#### Grupo de Controle Paramétrico 1: Indivíduos que residem em áreas que, futuramente, serão contempladas pelas linhas 6 - Laranja, 16 - Violeta, 19 - Celeste e 22 - Marrom ####

### Listando as estações futuras ###

estacoes_futuras <- tribble(
    ~linha    , ~nome_estacao      , ~lat     , ~lon     ,

    # Linha 6 - Laranja
    "Laranja" , "Brasilândia"      , -23.4673 , -46.6946 ,
    "Laranja" , "Vila Cardoso"     , -23.4764 , -46.6976 ,
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
    "Violeta" , "Ibirapuera"       , -23.5768 , -46.6514 ,
    "Violeta" , "Ana Rosa"         , -23.5816 , -46.6384 ,
    "Violeta" , "Aclimação"        , -23.5721 , -46.6242 ,
    "Violeta" , "Independência"    , -23.5804 , -46.6115 ,
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
    "Marrom"  , "Mesopotâmia"      , -23.5942 , -46.8456 ,
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

##### Colapsando a base por indivíduo, já que a base original está por viagens #####

od_grupos_sf <- od_grupos_sf |>
    mutate(
        dist_m_futuro = as.numeric(dist_futuro),
        dist_m_cptm = as.numeric(dist_cptm),
        mora_centro_exp = ifelse(indica_centro[, 1] == TRUE, 1, 0)
    ) |>
    st_drop_geometry() |>
    group_by(ID_PESS, ano, ZONA, ZMC) |>
    summarise(
        IDADE = first(IDADE),
        SEXO = first(SEXO),
        VL_REN_I = first(VL_REN_I),
        NO_MORAF = first(NO_MORAF),
        declarou_renda = first(declarou_renda),
        GRAU_INS = first(GRAU_INS),
        CD_ATIVI = first(CD_ATIVI),
        RENDA_FA = first(RENDA_FA),
        TIPVG = first(TIPVG),
        ZONA_O = first(ZONA_O),
        viaja = ifelse(sum(TOT_VIAG, na.rm = TRUE) > 0, 1, 0),
        indic_destino_centro_exp = max(indic_destino_centro_exp),
        trab_centro = max(trab_centro),
        educ_centro = max(educ_centro),
        saude_centro = max(saude_centro),
        lazer_centro = max(lazer_centro),
        compras_centro = max(compras_centro),
        emp_centro = max(emp_centro),
        trab_educ_emp_centro = max(trab_educ_emp_centro),
        trab_emp_centro = max(trab_emp_centro),
        lazer_comp_saude_centro = max(lazer_comp_saude_centro),
        lazer_comp_centro = max(lazer_comp_centro),
        trab_educ_centro = max(trab_educ_centro),
        MODOPRIN = max(MODOPRIN),
        DURACAO = if (all(is.na(MODOPRIN))) 0 else DURACAO[which.max(MODOPRIN)],
        DISTANCIA = if (all(is.na(MODOPRIN))) {
            0
        } else {
            DISTANCIA[which.max(MODOPRIN)]
        },
        CO_DOM_X_SIRGAS = first(CO_DOM_X_SIRGAS),
        CO_DOM_Y_SIRGAS = first(CO_DOM_Y_SIRGAS),
        dist_m_futuro = first(dist_m_futuro),
        dist_m_cptm = first(dist_m_cptm),
        mora_centro_exp = first(mora_centro_exp)
    ) |>
    ungroup()

#### Criando a base final com as dummies que definem cada grupo ####

od_grupos <- od_grupos_sf |>
    mutate(
        is_tratamento = ifelse(ZMC %in% zmcs_tratadas, TRUE, FALSE),
        tipo_grupo = case_when(
            is_tratamento ~ 'Tratamento',
            !is_tratamento &
                dist_m_futuro <= limite_dist &
                mora_centro_exp == 0 ~ 'Controle_Linhas_Futuras',
            !is_tratamento &
                dist_m_cptm <= limite_dist &
                mora_centro_exp == 0 ~ 'Controle_CPTM',
            mora_centro_exp == 1 ~ 'Morador_Centro_Exp',
            TRUE ~ 'Candidatos_Controle_MatchIt'
        )
    ) |>
    select(-is_tratamento)

##### Deflacionando a renda familiar e individual #####

od_grupos <- od_grupos |>
    mutate(
        data_original = case_when(
            ano == 2007 ~ '10/2007',
            ano == 2017 ~ '09/2017',
            ano == 2023 ~ '09/2023',
            TRUE ~ NA_character_
        ),
        RENDA_FA_DEF = ifelse(
            !is.na(RENDA_FA) & !is.na(data_original),
            deflate(
                nominal_values = RENDA_FA,
                nominal_dates = as.Date(
                    paste0('01/', data_original),
                    format = '%d/%m/%Y'
                ),
                real_date = '11/2025',
                index = 'ipca'
            ),
            RENDA_FA
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

##### Transformando a renda familiar em ln da renda familiar per capita #####

od_grupos <- od_grupos |>
    mutate(
        renda_fa_per_capita = RENDA_FA_DEF / NO_MORAF,
        ln_renda_fam_capita = log(renda_fa_per_capita + 1)
    )

##### Decompondo a variável indicadora de viagem com destino ao centro (indic_dest) em viagens realizadas de metrô e de carro #####

od_grupos <- od_grupos |>
    mutate(
        metro = ifelse(indic_destino_centro_exp == 1 & MODOPRIN == 1, 1, 0),
        carro = ifelse(
            indic_destino_centro_exp == 1 & MODOPRIN %in% c(9, 10),
            1,
            0
        ),
        onibus = ifelse(
            indic_destino_centro_exp == 1 & MODOPRIN %in% c(4, 6),
            1,
            0
        )
    )

##### Salvando a base final e os polígonos das ZMC's #####

od_grupos_dbf <- od_grupos |>
    mutate(across(where(is.list), as.character)) |>
    mutate(across(where(~ inherits(.x, "units")), as.numeric)) |>
    as.data.frame()

export(od_grupos_dbf, 'od_base_completa.dbf')

st_write(zmc_polygons, 'zmc_polygons.gpkg', append = FALSE)
