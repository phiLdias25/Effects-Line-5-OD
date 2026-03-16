###### Estatísticas Descritivas e formação do grupo de controle pelo MatchIt ######

rm(list = ls())

##### Abrindo Bibliotecas #####

library(tidyverse)
library(rio)
library(sf)
library(kableExtra)
library(scales)
library(scico)
library(classInt)
library(geobr)

##### Abrindo base de dados #####

od_completa <- import('od_base_completa.dbf')

##### Estatísticas Descritivas #####

#### Variáveis relevantes para serem analisadas: CRITERIOBR, IDADE, SEXO, GRAU_INS, CD_ATIVI, VL_REN_I, VINC1, TOT_VIAG, DURACAO, MODOPRIN ####

#### Tabela com descritivas sociais - Idade, sexo e grau de instrução ####

od_estat_sociais <- od_completa |>
  select(ano, IDADE, SEXO, GRAU_INS) |>
  mutate(
    SEXO = case_when(
      SEXO == 1 ~ 'Masculino',
      SEXO == 2 ~ 'Feminino',
      TRUE ~ NA_character_
    ),
    GRAU_INS = case_when(
      GRAU_INS == 1 ~ 'Não alfabetizado/ Fund 1 incompleto',
      GRAU_INS == 2 ~ 'Fund 1 completo/ Fund 2 incompleto',
      GRAU_INS == 3 ~ 'Fund 2 completo/ Médio incompleto',
      GRAU_INS == 4 ~ 'Médio completo/ Superior incompleto',
      GRAU_INS == 5 ~ 'Superior completo',
      TRUE ~ NA_character_
    ),
    ano = as.factor(ano)
  )

idade_anual <- od_estat_sociais |>
  group_by(ano) |>
  summarise(
    Valor = mean(IDADE, na.rm = TRUE),
    Variavel = 'Idade Média (Anos)'
  ) |>
  mutate(Valor = round(Valor, 1)) |>
  ungroup() |>
  select(ano, Variavel, Valor)

categorias_anual <- od_estat_sociais |>
  select(-IDADE) |>
  pivot_longer(
    cols = c(SEXO, GRAU_INS),
    names_to = 'origem',
    values_to = 'categoria'
  ) |>
  filter(!is.na(categoria)) |>
  count(ano, origem, categoria) |>
  group_by(ano, origem) |>
  mutate(
    prob = n / sum(n),
    Valor = round(prob, 3)
  ) |>
  ungroup() |>
  select(ano, Variavel = categoria, Valor)

tabela_descrit_sociais <- bind_rows(idade_anual, categorias_anual) |>
  pivot_wider(
    names_from = ano,
    values_from = Valor
  ) |>
  arrange(factor(
    Variavel,
    levels = c(
      'Idade Média (Anos)',
      'Masculino',
      'Feminino',
      'Não alfabetizado/ Fund 1 incompleto',
      'Fund 1 completo/ Fund 2 incompleto',
      'Fund 2 completo/ Médio incompleto',
      'Médio completo/ Superior incompleto',
      'Superior completo'
    )
  ))

tabela_descrit_sociais_latex <- tabela_descrit_sociais |>
  kbl(
    format = 'latex',
    booktabs = TRUE,
    label = 'tab:descritivas_sociais',
    align = 'lrrr'
  ) |>
  kable_styling(
    latex_options = c('striped', 'hold_position'),
    position = 'center'
  )

#### Tabela com o Critério Brasil ao longo do tempo ####

od_criteriobr <- od_completa |>
  select(ano, CRITERIOBR) |>
  mutate(
    CRITERIOBR = case_when(
      CRITERIOBR == 1 ~ 'A',
      CRITERIOBR == 2 ~ 'B1',
      CRITERIOBR == 3 ~ 'B2',
      CRITERIOBR == 4 ~ 'C1',
      CRITERIOBR == 5 ~ 'C2',
      CRITERIOBR == 6 ~ 'D e E',
      TRUE ~ NA_character_
    )
  )

tabela_critbr <- od_criteriobr |>
  filter(!is.na(CRITERIOBR)) |>
  count(ano, CRITERIOBR) |>
  group_by(ano) |>
  mutate(
    prob = n / sum(n),
    Valor = round(prob, 3)
  ) |>
  ungroup() |>
  select(ano, Variavel = CRITERIOBR, Valor) |>
  pivot_wider(
    names_from = ano,
    values_from = Valor
  ) |>
  arrange(factor(
    Variavel,
    levels = c('A', 'B1', 'B2', 'C1', 'C2', 'D e E')
  ))

tabela_latex_critbr <- tabela_critbr |>
  kbl(
    format = 'latex',
    booktabs = TRUE,
    label = 'tab:descritivas_criteriobr',
    align = 'lrrr'
  ) |>
  kable_styling(
    latex_options = c('striped', 'hold_position'),
    position = 'center'
  )

#### Tabela com descritivas econômicas - Renda, condição empregatícia e vínculo empregatício ####

od_estat_econo <- od_completa |>
  select(ano, VL_REN_I_D, CD_ATIVI, VINC1) |>
  mutate(
    CD_ATIVI = case_when(
      CD_ATIVI == 1 ~ 'Trabalho Regular',
      CD_ATIVI == 2 ~ 'Bico',
      CD_ATIVI == 3 ~ 'Licença Médica',
      CD_ATIVI == 4 ~ 'Aposentado/Pensionista',
      CD_ATIVI == 5 ~ 'Desempregado',
      CD_ATIVI == 6 ~ 'Nunca Trabalhou',
      CD_ATIVI == 7 ~ 'Dona de Casa',
      CD_ATIVI == 8 ~ 'Estudante',
      TRUE ~ NA_character_
    ),
    VINC1 = case_when(
      VINC1 == 1 ~ 'Assalariado com carteira',
      VINC1 == 2 ~ 'Assalariado sem carteira',
      VINC1 == 3 ~ 'Funcionário Público',
      VINC1 == 4 ~ 'Autônomo',
      VINC1 == 5 ~ 'Empregador',
      VINC1 == 6 ~ 'Profissional Liberal',
      VINC1 == 7 ~ 'Dono de Negócio Familiar',
      VINC1 == 8 ~ 'Trabalhador Familiar',
      TRUE ~ NA_character_
    ),
    ano = as.factor(ano)
  )

renda_anual <- od_estat_econo |>
  group_by(ano) |>
  filter(!is.na(VL_REN_I_D)) |>
  summarise(
    Valor = mean(VL_REN_I_D, na.rm = TRUE),
    Variavel = 'Renda Individual Média (R$)'
  ) |>
  ungroup() |>
  mutate(Valor = round(Valor, 2)) |>
  select(ano, Variavel, Valor)

categorias_econo_anual <- od_estat_econo |>
  select(-VL_REN_I_D) |>
  pivot_longer(
    cols = c(CD_ATIVI, VINC1),
    names_to = 'origem',
    values_to = 'categoria'
  ) |>
  filter(!is.na(categoria)) |>
  count(ano, origem, categoria) |>
  group_by(ano, origem) |>
  mutate(
    prob = n / sum(n),
    Valor = round(prob, 3)
  ) |>
  ungroup() |>
  select(ano, Variavel = categoria, Valor)

tabela_descrit_econo <- bind_rows(renda_anual, categorias_econo_anual) |>
  pivot_wider(
    names_from = ano,
    values_from = Valor
  ) |>
  arrange(factor(
    Variavel,
    levels = c(
      'Renda Individual Média (R$)',
      'Trabalho Regular',
      'Bico',
      'Licença Médica',
      'Aposentado/Pensionista',
      'Desempregado',
      'Nunca Trabalhou',
      'Dona de Casa',
      'Estudante',
      'Assalariado com carteira',
      'Assalariado sem carteira',
      'Funcionário Público',
      'Autônomo',
      'Empregador',
      'Profissional Liberal',
      'Dono de Negócio Familiar',
      'Trabalhador Familiar'
    )
  ))

tabela_latex_econo <- tabela_descrit_econo |>
  kbl(
    format = 'latex',
    booktabs = TRUE,
    label = 'tab:descritivas_economicas',
    align = 'lrrr'
  ) |>
  kable_styling(
    latex_options = c('striped', 'hold_position'),
    position = 'center'
  )


#### Tabela com descritivas de viagem - Total de viagens médio, duração média e modo principal ####

od_estat_viagens <- od_completa |>
  select(ano, TOT_VIAG, DURACAO, MODOPRIN) |>
  mutate(
    MODOPRIN = case_when(
      MODOPRIN == 1 ~ 'Metrô',
      MODOPRIN == 2 ~ 'Trem',
      MODOPRIN == 4 ~ 'Ônibus/Micro-ônibus/Van Municipal',
      MODOPRIN == 5 ~ 'Ônibus/Micro-ônibus/Van Intermunicipal',
      MODOPRIN == 6 ~ 'Ônibus/Micro-ônibus/Van Metropolitano',
      MODOPRIN == 7 ~ 'Transporte Fretado',
      MODOPRIN == 8 ~ 'Transporte Escolar',
      MODOPRIN == 9 ~ 'Dirigindo Automóvel',
      MODOPRIN == 10 ~ 'Passageiro de Automóvel',
      MODOPRIN == 11 ~ 'Táxi',
      MODOPRIN == 13 ~ 'Moto',
      MODOPRIN == 15 ~ 'Bicicleta',
      MODOPRIN == 16 ~ 'A Pé',
      MODOPRIN == 17 ~ 'Outros',
      TRUE ~ NA_character_
    ),
    ano = as.factor(ano)
  )

totviag_anual <- od_estat_viagens |>
  group_by(ano) |>
  filter(!is.na(TOT_VIAG)) |>
  summarise(
    Valor = mean(TOT_VIAG, na.rm = TRUE),
    Variavel = 'Média de Total de Viagens'
  ) |>
  ungroup() |>
  mutate(Valor = round(Valor, 2)) |>
  select(ano, Variavel, Valor)

duracao_anual <- od_estat_viagens |>
  group_by(ano) |>
  filter(!is.na(DURACAO)) |>
  summarise(
    Valor = mean(DURACAO, na.rm = TRUE),
    Variavel = 'Duração Média das Viagens (em minutos)'
  ) |>
  ungroup() |>
  mutate(Valor = round(Valor, 2)) |>
  select(ano, Variavel, Valor)

modo_viagem_anual <- od_estat_viagens |>
  select(-TOT_VIAG, DURACAO) |>
  pivot_longer(
    cols = MODOPRIN,
    names_to = 'origem',
    values_to = 'categoria'
  ) |>
  filter(!is.na(categoria)) |>
  count(ano, origem, categoria) |>
  group_by(ano, origem) |>
  mutate(
    prob = n / sum(n),
    Valor = round(prob, 3)
  ) |>
  ungroup() |>
  select(ano, Variavel = categoria, Valor)

tabela_descrit_viagens <- bind_rows(
  totviag_anual,
  duracao_anual,
  modo_viagem_anual
) |>
  pivot_wider(
    names_from = ano,
    values_from = Valor
  ) |>
  arrange(factor(
    Variavel,
    levels = c(
      'Média de Total de Viagens',
      'Duração Média das Viagens (em minutos)',
      'Metrô',
      'Trem',
      'Ônibus/Micro-ônibus/Van Municipal',
      'Ônibus/Micro-ônibus/Van Intermunicipal',
      'Ônibus/Micro-ônibus/Van Metropolitano',
      'Transporte Fretado',
      'Transporte Escolar',
      'Dirigindo Automóvel',
      'Passageiro de Automóvel',
      'Táxi',
      'Moto',
      'Bicicleta',
      'A Pé',
      'Outros'
    )
  ))

tabela_latex_viagens <- tabela_descrit_viagens |>
  kbl(
    format = 'latex',
    booktabs = TRUE,
    label = 'tab:descritivas_viagens',
    align = 'lrrr'
  ) |>
  kable_styling(
    latex_options = c('striped', 'hold_position'),
    position = 'center'
  )


#### Mapas de calor de renda média e duração média das viagens por zonas e ano ####

### Renda Média ###

## 2007 ##

zonas_2007 <- st_read('Shapefiles OD/Zonas2007_region.shp') |>
  st_buffer(dist = 0) |>
  st_make_valid() |>
  st_transform(31983)

pontos_2007 <- od_completa |>
  filter(ano == 2007) |>
  mutate(
    CO_DOM_X_S = as.numeric(CO_DOM_X_S),
    CO_DOM_Y_S = as.numeric(CO_DOM_Y_S)
  ) |>
  filter(
    !is.na(CO_DOM_X_S),
    !is.na(CO_DOM_Y_S),
    is.finite(CO_DOM_X_S),
    is.finite(CO_DOM_Y_S)
  ) |>
  st_as_sf(coords = c('CO_DOM_X_S', 'CO_DOM_Y_S'), crs = 31983) |>
  st_zm(drop = TRUE, what = "ZM")

any(st_is_empty(pontos_2007))

localiz_2007 <- st_join(pontos_2007, zonas_2007)

sp_urb_07 <- read_urban_area(year = 2005, code_state = "SP")

mancha_urbana_sp_07 <- sp_urb_07[sp_urb_07$name_muni == "São Paulo", ] |>
  st_transform(crs = 31983) |>
  st_make_valid() |>
  st_union()

mun_07 <- read_metro_area(code_state = 'SP', year = 2005)

mun_07 <- mun_07[mun_07$name_metro == "RM São Paulo", ]

renda_media_2007 <- localiz_2007 |>
  select(
    VL_REN_I_D,
    Zona07,
    NomeZona07,
    Municipio,
    NomeMunici,
    Area_ha,
    geometry
  ) |>
  st_drop_geometry() |>
  group_by(Zona07) |>
  summarise(
    Renda_Media = mean(VL_REN_I_D, na.rm = TRUE),
    n_zona = n()
  )

mapa_renda_2007 <- zonas_2007 |>
  left_join(renda_media_2007, by = 'Zona07')

quartis_renda_2007 <- c(
  0,
  1000,
  3000,
  6000,
  max(mapa_renda_2007$Renda_Media, na.rm = TRUE)
)

labels_quartis_renda <- c(
  'R$ 0 - R$ 1.000',
  'R$ 1.000 - R$ 3.000',
  'R$ 3.000 - R$ 6.000',
  paste0('Mais de R$ 6.000')
)

mapa_renda_2007_final <- mapa_renda_2007 |>
  mutate(
    Renda_Categoria = cut(
      Renda_Media,
      breaks = quartis_renda_2007,
      labels = labels_quartis_renda,
      include.lowest = TRUE,
      right = FALSE
    )
  ) |>
  st_make_valid(mapa_renda_2007_final)

mapa_renda_2007_final_2 <- st_intersection(
  st_make_valid(mapa_renda_2007_final),
  st_make_valid(mancha_urbana_sp_07)
)

bbox <- st_bbox(mapa_renda_2007_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.3
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.3

xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_renda_2007_final_2,
    aes(fill = Renda_Categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_07, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = 'Renda Real (R$)',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

## 2017 ##

zonas_2017 <- st_read('Shapefiles OD/Zonas_2017_region.shp') |>
  st_buffer(dist = 0) |>
  st_make_valid() |>
  st_transform(31983)

pontos_2017 <- od_completa |>
  filter(ano == 2017) |>
  mutate(
    CO_DOM_X_S = as.numeric(CO_DOM_X_S),
    CO_DOM_Y_S = as.numeric(CO_DOM_Y_S)
  ) |>
  filter(
    !is.na(CO_DOM_X_S),
    !is.na(CO_DOM_Y_S),
    is.finite(CO_DOM_X_S),
    is.finite(CO_DOM_Y_S)
  ) |>
  st_as_sf(coords = c('CO_DOM_X_S', 'CO_DOM_Y_S'), crs = 31983) |>
  st_zm(drop = TRUE, what = "ZM")

any(st_is_empty(pontos_2017))

localiz_2017 <- st_join(pontos_2017, zonas_2017)

sp_urb_17 <- read_urban_area(year = 2015, code_state = "SP")

mancha_urbana_sp_17 <- sp_urb_17[sp_urb_17$name_muni == "São Paulo/SP", ] |>
  st_transform(crs = 31983) |>
  st_make_valid() |>
  st_union()

mun_17 <- read_metro_area(code_state = 'SP', year = 2017)

mun_17 <- mun_17[mun_17$name_metro == "RM São Paulo", ]

renda_media_2017 <- localiz_2017 |>
  select(
    VL_REN_I_D,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  group_by(NumeroZona) |>
  summarise(
    Renda_Media = mean(VL_REN_I_D, na.rm = TRUE),
    n_zona = n()
  )

mapa_renda_2017 <- zonas_2017 |>
  left_join(renda_media_2017, by = 'NumeroZona')

quartis_renda_2017 <- c(
  0,
  1000,
  3000,
  6000,
  max(mapa_renda_2017$Renda_Media, na.rm = TRUE)
)

labels_quartis_renda <- c(
  'R$ 0 - R$ 1.000',
  'R$ 1.000 - R$ 3.000',
  'R$ 3.000 - R$ 6.000',
  paste0('Mais de R$ 6.000')
)

mapa_renda_2017_final <- mapa_renda_2017 |>
  mutate(
    Renda_Categoria = cut(
      Renda_Media,
      breaks = quartis_renda_2017,
      labels = labels_quartis_renda,
      include.lowest = TRUE,
      right = FALSE
    )
  ) |>
  st_make_valid(mapa_renda_2017_final)

mapa_renda_2017_final_2 <- st_intersection(
  st_make_valid(mapa_renda_2017_final),
  st_make_valid(mancha_urbana_sp_17)
)

bbox <- st_bbox(mapa_renda_2017_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.00005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.00005

xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_renda_2017_final_2,
    aes(fill = Renda_Categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_17, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = 'Renda Real (R$)',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

## 2023 ##

zonas_2023 <- st_read('Shapefiles OD/Zonas_2023.shp') |>
  st_buffer(dist = 0) |>
  st_make_valid() |>
  st_transform(31983)

pontos_2023 <- od_completa |>
  filter(ano == 2023) |>
  mutate(
    CO_DOM_X_S = as.numeric(CO_DOM_X_S),
    CO_DOM_Y_S = as.numeric(CO_DOM_Y_S)
  ) |>
  filter(
    !is.na(CO_DOM_X_S),
    !is.na(CO_DOM_Y_S),
    is.finite(CO_DOM_X_S),
    is.finite(CO_DOM_Y_S)
  ) |>
  st_as_sf(coords = c('CO_DOM_X_S', 'CO_DOM_Y_S'), crs = 31983) |>
  st_zm(drop = TRUE, what = "ZM")

any(st_is_empty(pontos_2023))

localiz_2023 <- st_join(pontos_2023, zonas_2023)

sp_urb_23 <- read_urban_area(year = 2015, code_state = "SP")

mancha_urbana_sp_23 <- sp_urb_23[sp_urb_23$name_muni == "São Paulo/SP", ] |>
  st_transform(crs = 31983) |>
  st_make_valid() |>
  st_union()

mun_23 <- read_metro_area(code_state = 'SP', year = 2018)

mun_23 <- mun_23[mun_23$name_metro == "RM São Paulo", ]

renda_media_2023 <- localiz_2023 |>
  select(
    VL_REN_I_D,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  group_by(NumeroZona) |>
  summarise(
    Renda_Media = mean(VL_REN_I_D, na.rm = TRUE),
    n_zona = n()
  )

mapa_renda_2023 <- zonas_2023 |>
  left_join(renda_media_2023, by = 'NumeroZona')

quartis_renda_2023 <- c(
  0,
  1000,
  3000,
  6000,
  max(mapa_renda_2023$Renda_Media, na.rm = TRUE)
)

labels_quartis_renda <- c(
  'R$ 0 - R$ 1.000',
  'R$ 1.000 - R$ 3.000',
  'R$ 3.000 - R$ 6.000',
  paste0('Mais de R$ 6.000')
)

mapa_renda_2023_final <- mapa_renda_2023 |>
  mutate(
    Renda_Categoria = cut(
      Renda_Media,
      breaks = quartis_renda_2023,
      labels = labels_quartis_renda,
      include.lowest = TRUE,
      right = FALSE
    )
  ) |>
  st_make_valid(mapa_renda_2023_final)

mapa_renda_2023_final_2 <- st_intersection(
  st_make_valid(mapa_renda_2023_final),
  st_make_valid(mancha_urbana_sp_23)
)

bbox <- st_bbox(mapa_renda_2023_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.00005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.00005

xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_renda_2023_final_2,
    aes(fill = Renda_Categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_23, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = 'Renda Real (R$)',
    na.value = 'grey90',
    na.translate = FALSE,

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

### Duração Média das Viagens ###

## 2007 ##

duracao_media_2007 <- localiz_2007 |>
  select(
    DURACAO,
    Zona07,
    NomeZona07,
    Municipio,
    NomeMunici,
    Area_ha,
    geometry
  ) |>
  st_drop_geometry() |>
  group_by(Zona07) |>
  summarise(
    Duracao_media = mean(DURACAO, na.rm = TRUE),
    n_zona = n()
  )

mapa_duracao_2007 <- zonas_2007 |>
  left_join(duracao_media_2007, by = 'Zona07')

quartis_duracao_2007 <- c(
  0,
  15,
  30,
  45,
  max(mapa_duracao_2007$Duracao_media, na.rm = TRUE)
)

labels_quartis_duracao <- c(
  '0 - 15 minutos',
  '15 - 30 minutos',
  '30 - 45 minutos',
  paste0('Mais de 45 minutos')
)

mapa_duracao_2007_final <- mapa_duracao_2007 |>
  mutate(
    Duracao_Categoria = cut(
      Duracao_media,
      breaks = quartis_duracao_2007,
      labels = labels_quartis_duracao,
      include.lowest = TRUE,
      right = FALSE
    )
  ) |>
  st_make_valid(mapa_duracao_2007_final)

mapa_duracao_2007_final_2 <- st_intersection(
  st_make_valid(mapa_duracao_2007_final),
  st_make_valid(mancha_urbana_sp_07)
)

bbox <- st_bbox(mapa_duracao_2007_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.3
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.3

xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_duracao_2007_final_2,
    aes(fill = Duracao_Categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_07, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'managua',
    direction = -1,
    name = 'Tempo Médio de Viagem (Minutos)',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

## 2017 ##

duracao_media_2017 <- localiz_2017 |>
  select(
    DURACAO,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  group_by(NumeroZona) |>
  summarise(
    Duracao_Media = mean(DURACAO, na.rm = TRUE),
    n_zona = n()
  )

mapa_duracao_2017 <- zonas_2017 |>
  left_join(duracao_media_2017, by = 'NumeroZona')

quartis_duracao_2017 <- c(
  0,
  15,
  30,
  45,
  max(mapa_duracao_2017$Duracao_Media, na.rm = TRUE)
)

labels_quartis_duracao <- c(
  '0 - 15 minutos',
  '15 - 30 minutos',
  '30 - 45 minutos',
  paste0('Mais de 45 minutos')
)

mapa_duracao_2017_final <- mapa_duracao_2017 |>
  mutate(
    Duracao_Categoria = cut(
      Duracao_Media,
      breaks = quartis_duracao_2017,
      labels = labels_quartis_duracao,
      include.lowest = TRUE,
      right = FALSE
    )
  ) |>
  st_make_valid(mapa_duracao_2017_final)

mapa_duracao_2017_final_2 <- st_intersection(
  st_make_valid(mapa_duracao_2017_final),
  st_make_valid(mancha_urbana_sp_17)
)

bbox <- st_bbox(mapa_duracao_2017_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.00005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.00005

xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_duracao_2017_final_2,
    aes(fill = Duracao_Categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_17, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'managua',
    direction = -1,
    name = 'Tempo Médio de Viagem (Minutos)',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

## 2023 ##

duracao_media_2023 <- localiz_2023 |>
  select(
    DURACAO,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  group_by(NumeroZona) |>
  summarise(
    Duracao_Media = mean(DURACAO, na.rm = TRUE),
    n_zona = n()
  )

mapa_duracao_2023 <- zonas_2023 |>
  left_join(duracao_media_2023, by = 'NumeroZona')

quartis_duracao_2023 <- c(
  0,
  15,
  30,
  45,
  max(mapa_duracao_2023$Duracao_Media, na.rm = TRUE)
)

labels_quartis_duracao <- c(
  '0 - 15 minutos',
  '15 - 30 minutos',
  '30 - 45 minutos',
  paste0('Mais de 45 minutos')
)

mapa_duracao_2023_final <- mapa_duracao_2023 |>
  mutate(
    Duracao_Categoria = cut(
      Duracao_Media,
      breaks = quartis_duracao_2023,
      labels = labels_quartis_duracao,
      include.lowest = TRUE,
      right = FALSE
    )
  ) |>
  st_make_valid(mapa_duracao_2023_final)

mapa_duracao_2023_final_2 <- st_intersection(
  st_make_valid(mapa_duracao_2023_final),
  st_make_valid(mancha_urbana_sp_23)
)

bbox <- st_bbox(mapa_duracao_2023_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.00005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.00005

xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_duracao_2023_final_2,
    aes(fill = Duracao_Categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_23, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'managua',
    direction = -1,
    name = 'Tempo Médio de Viagem (Minutos)',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

#### Mapas de calor de algumas variáveis indicadoras ####

### Frequência de indivíduos em trabalho regular e desempregados por ano ###

indicadores_cd <- od_completa |>
  select(ano, CO_DOM_X_S, CO_DOM_Y_S, CD_ATIVI) |>
  mutate(
    trab_reg = ifelse(CD_ATIVI == 1, 1, 0),
    desempregado = ifelse(CD_ATIVI == 5, 1, 0)
  )

## 2007 ##

pontos_2007_indic <- indicadores_cd |>
  filter(ano == 2007) |>
  mutate(
    CO_DOM_X_S = as.numeric(CO_DOM_X_S),
    CO_DOM_Y_S = as.numeric(CO_DOM_Y_S)
  ) |>
  filter(
    !is.na(CO_DOM_X_S),
    !is.na(CO_DOM_Y_S),
    is.finite(CO_DOM_X_S),
    is.finite(CO_DOM_Y_S),
    !is.na(trab_reg),
    !is.na(desempregado)
  ) |>
  st_as_sf(coords = c('CO_DOM_X_S', 'CO_DOM_Y_S'), crs = 31983) |>
  st_zm(drop = TRUE, what = "ZM")

any(st_is_empty(pontos_2007_indic))

localiz_2007_indic <- st_join(pontos_2007_indic, zonas_2007)

empregados_2007 <- localiz_2007_indic |>
  select(
    trab_reg,
    Zona07,
    NomeZona07,
    Municipio,
    NomeMunici,
    Area_ha,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(trab_reg)) |>
  group_by(Zona07) |>
  summarise(
    freq_trab_reg = mean(trab_reg, na.rm = TRUE)
  )

desempregados_2007 <- localiz_2007_indic |>
  select(
    desempregado,
    Zona07,
    NomeZona07,
    Municipio,
    NomeMunici,
    Area_ha,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(desempregado)) |>
  group_by(Zona07) |>
  summarise(
    freq_desempregado = mean(desempregado, na.rm = TRUE)
  )

mapa_trabreg_2007 <- zonas_2007 |>
  left_join(empregados_2007, by = 'Zona07')

quartis_trabreg_2007 <- c(
  0,
  0.25,
  0.50,
  0.75,
  max(mapa_trabreg_2007$freq_trab_reg, na.rm = TRUE)
)

labels_quartis_trabreg <- c(
  '0 - 25%',
  '25% - 50%',
  '50% - 75%',
  paste0('Mais de 75%')
)

mapa_trabreg_2007_final <- mapa_trabreg_2007 |>
  mutate(
    categoria = cut(
      freq_trab_reg,
      breaks = quartis_trabreg_2007,
      labels = labels_quartis_trabreg,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_trabreg_2007_final)

mapa_trabreg_2007_final_2 <- st_intersection(
  st_make_valid(mapa_trabreg_2007_final),
  st_make_valid(mancha_urbana_sp_07)
)

bbox <- st_bbox(mapa_trabreg_2007_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.3
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.3

xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_trabreg_2007_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_07, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = '% de pessoas em trabalho regular',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_desemp_2007 <- zonas_2007 |>
  left_join(desempregados_2007, by = 'Zona07')

quartis_desemp_2007 <- c(
  0,
  0.03,
  0.06,
  0.09,
  max(mapa_desemp_2007$freq_desempregado, na.rm = TRUE)
)

labels_quartis_desemp <- c(
  '0 - 3%',
  '3% - 6%',
  '6% - 9%',
  paste0('Mais de 9%')
)

mapa_desemp_2007_final <- mapa_desemp_2007 |>
  mutate(
    categoria = cut(
      freq_desempregado,
      breaks = quartis_desemp_2007,
      labels = labels_quartis_desemp,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_desemp_2007_final)

mapa_desemp_2007_final_2 <- st_intersection(
  st_make_valid(mapa_desemp_2007_final),
  st_make_valid(mancha_urbana_sp_07)
)

bbox <- st_bbox(mapa_desemp_2007_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.3
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.3

xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_desemp_2007_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_07, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = '% de pessoas desempregadas',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

## 2017 ##

pontos_2017_indic <- indicadores_cd |>
  filter(ano == 2017) |>
  mutate(
    CO_DOM_X_S = as.numeric(CO_DOM_X_S),
    CO_DOM_Y_S = as.numeric(CO_DOM_Y_S)
  ) |>
  filter(
    !is.na(CO_DOM_X_S),
    !is.na(CO_DOM_Y_S),
    is.finite(CO_DOM_X_S),
    is.finite(CO_DOM_Y_S),
    !is.na(trab_reg),
    !is.na(desempregado)
  ) |>
  st_as_sf(coords = c('CO_DOM_X_S', 'CO_DOM_Y_S'), crs = 31983) |>
  st_zm(drop = TRUE, what = "ZM")

any(st_is_empty(pontos_2017_indic))

localiz_2017_indic <- st_join(pontos_2017_indic, zonas_2017)

empregados_2017 <- localiz_2017_indic |>
  select(
    trab_reg,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(trab_reg)) |>
  group_by(NumeroZona) |>
  summarise(
    freq_trab_reg = mean(trab_reg, na.rm = TRUE)
  )

desempregados_2017 <- localiz_2017_indic |>
  select(
    desempregado,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(desempregado)) |>
  group_by(NumeroZona) |>
  summarise(
    freq_desempregado = mean(desempregado, na.rm = TRUE)
  )

mapa_trabreg_2017 <- zonas_2017 |>
  left_join(empregados_2017, by = 'NumeroZona')

quartis_trabreg_2017 <- c(
  0,
  0.25,
  0.50,
  0.75,
  max(mapa_trabreg_2017$freq_trab_reg, na.rm = TRUE)
)

labels_quartis_trabreg <- c(
  '0 - 25%',
  '25% - 50%',
  '50% - 75%',
  paste0('Mais de 75%')
)

mapa_trabreg_2017_final <- mapa_trabreg_2017 |>
  mutate(
    categoria = cut(
      freq_trab_reg,
      breaks = quartis_trabreg_2017,
      labels = labels_quartis_trabreg,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_trabreg_2017_final)

mapa_trabreg_2017_final_2 <- st_intersection(
  st_make_valid(mapa_trabreg_2017_final),
  st_make_valid(mancha_urbana_sp_17)
)

bbox <- st_bbox(mapa_trabreg_2017_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.00005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.00005

xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_trabreg_2017_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_17, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = '% de pessoas em trabalho regular',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_desemp_2017 <- zonas_2017 |>
  left_join(desempregados_2017, by = 'NumeroZona')

quartis_desemp_2017 <- c(
  0,
  0.03,
  0.06,
  0.09,
  max(mapa_desemp_2017$freq_desempregado, na.rm = TRUE)
)

labels_quartis_desemp <- c(
  '0 - 3%',
  '3% - 6%',
  '6% - 9%',
  paste0('Mais de 9%')
)

mapa_desemp_2017_final <- mapa_desemp_2017 |>
  mutate(
    categoria = cut(
      freq_desempregado,
      breaks = quartis_desemp_2017,
      labels = labels_quartis_desemp,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_desemp_2017_final)

mapa_desemp_2017_final_2 <- st_intersection(
  st_make_valid(mapa_desemp_2017_final),
  st_make_valid(mancha_urbana_sp_17)
)

bbox <- st_bbox(mapa_desemp_2017_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.00005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.00005
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_desemp_2017_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_17, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = '% de pessoas desempregadas',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

## 2023 ##

pontos_2023_indic <- indicadores_cd |>
  filter(ano == 2017) |>
  mutate(
    CO_DOM_X_S = as.numeric(CO_DOM_X_S),
    CO_DOM_Y_S = as.numeric(CO_DOM_Y_S)
  ) |>
  filter(
    !is.na(CO_DOM_X_S),
    !is.na(CO_DOM_Y_S),
    is.finite(CO_DOM_X_S),
    is.finite(CO_DOM_Y_S),
    !is.na(trab_reg),
    !is.na(desempregado)
  ) |>
  st_as_sf(coords = c('CO_DOM_X_S', 'CO_DOM_Y_S'), crs = 31983) |>
  st_zm(drop = TRUE, what = "ZM")

any(st_is_empty(pontos_2023_indic))

localiz_2023_indic <- st_join(pontos_2023_indic, zonas_2023)

empregados_2023 <- localiz_2023_indic |>
  select(
    trab_reg,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(trab_reg)) |>
  group_by(NumeroZona) |>
  summarise(
    freq_trab_reg = mean(trab_reg, na.rm = TRUE)
  )

desempregados_2023 <- localiz_2023_indic |>
  select(
    desempregado,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(desempregado)) |>
  group_by(NumeroZona) |>
  summarise(
    freq_desempregado = mean(desempregado, na.rm = TRUE)
  )

mapa_trabreg_2023 <- zonas_2023 |>
  left_join(empregados_2017, by = 'NumeroZona')

quartis_trabreg_2023 <- c(
  0,
  0.25,
  0.50,
  0.75,
  max(mapa_trabreg_2023$freq_trab_reg, na.rm = TRUE)
)

labels_quartis_trabreg <- c(
  '0 - 25%',
  '25% - 50%',
  '50% - 75%',
  paste0('Mais de 75%')
)

mapa_trabreg_2023_final <- mapa_trabreg_2023 |>
  mutate(
    categoria = cut(
      freq_trab_reg,
      breaks = quartis_trabreg_2023,
      labels = labels_quartis_trabreg,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_trabreg_2023_final)

mapa_trabreg_2023_final_2 <- st_intersection(
  st_make_valid(mapa_trabreg_2023_final),
  st_make_valid(mancha_urbana_sp_23)
)

bbox <- st_bbox(mapa_trabreg_2023_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.00005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.00005
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_trabreg_2023_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_23, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = '% de pessoas em trabalho regular',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_desemp_2023 <- zonas_2023 |>
  left_join(desempregados_2017, by = 'NumeroZona')

quartis_desemp_2023 <- c(
  0,
  0.03,
  0.06,
  0.09,
  max(mapa_desemp_2023$freq_desempregado, na.rm = TRUE)
)

labels_quartis_desemp <- c(
  '0 - 3%',
  '3% - 6%',
  '6% - 9%',
  paste0('Mais de 9%')
)

mapa_desemp_2023_final <- mapa_desemp_2023 |>
  mutate(
    categoria = cut(
      freq_desempregado,
      breaks = quartis_desemp_2023,
      labels = labels_quartis_desemp,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_desemp_2023_final)

mapa_desemp_2023_final_2 <- st_intersection(
  st_make_valid(mapa_desemp_2023_final),
  st_make_valid(mancha_urbana_sp_23)
)

bbox <- st_bbox(mapa_desemp_2023_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.00005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.00005
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_desemp_2023_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_23, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = '% de pessoas desempregadas',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

### Frequência de indivíduos em trabalho com carteira assinada e autônomos por ano ###

indicadores_vinc <- od_completa |>
  select(ano, CO_DOM_X_S, CO_DOM_Y_S, VINC1) |>
  mutate(
    carteira = ifelse(VINC1 == 1, 1, 0),
    autonomo = ifelse(VINC1 == 4, 1, 0)
  )

## 2007 ##

pontos_2007_vinc <- indicadores_vinc |>
  filter(ano == 2007) |>
  mutate(
    CO_DOM_X_S = as.numeric(CO_DOM_X_S),
    CO_DOM_Y_S = as.numeric(CO_DOM_Y_S)
  ) |>
  filter(
    !is.na(CO_DOM_X_S),
    !is.na(CO_DOM_Y_S),
    is.finite(CO_DOM_X_S),
    is.finite(CO_DOM_Y_S),
    !is.na(carteira),
    !is.na(autonomo)
  ) |>
  st_as_sf(coords = c('CO_DOM_X_S', 'CO_DOM_Y_S'), crs = 31983) |>
  st_zm(drop = TRUE, what = "ZM")

any(st_is_empty(pontos_2007_vinc))

localiz_2007_vinc <- st_join(pontos_2007_vinc, zonas_2007)

carteira_2007 <- localiz_2007_vinc |>
  select(
    carteira,
    Zona07,
    NomeZona07,
    Municipio,
    NomeMunici,
    Area_ha,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(carteira)) |>
  group_by(Zona07) |>
  summarise(
    freq_carteira = mean(carteira, na.rm = TRUE)
  )

autonomo_2007 <- localiz_2007_vinc |>
  select(
    autonomo,
    Zona07,
    NomeZona07,
    Municipio,
    NomeMunici,
    Area_ha,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(autonomo)) |>
  group_by(Zona07) |>
  summarise(
    freq_autonomo = mean(autonomo, na.rm = TRUE)
  )

mapa_carteira_2007 <- zonas_2007 |>
  left_join(carteira_2007, by = 'Zona07')

quartis_carteira_2007 <- c(
  0,
  0.25,
  0.50,
  0.75,
  max(mapa_carteira_2007$freq_carteira, na.rm = TRUE)
)

labels_quartis_carteira <- c(
  '0 - 25%',
  '25% - 50%',
  '50% - 75%',
  paste0('Mais de 75%')
)

mapa_carteira_2007_final <- mapa_carteira_2007 |>
  mutate(
    categoria = cut(
      freq_carteira,
      breaks = quartis_carteira_2007,
      labels = labels_quartis_carteira,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_carteira_2007_final)

mapa_carteira_2007_final_2 <- st_intersection(
  st_make_valid(mapa_carteira_2007_final),
  st_make_valid(mancha_urbana_sp_07)
)

bbox <- st_bbox(mapa_carteira_2007_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.3
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.3
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_carteira_2007_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_07, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = '% de pessoas trabalhando com carteira assinada',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_autonomo_2007 <- zonas_2007 |>
  left_join(autonomo_2007, by = 'Zona07')

quartis_autonomo_2007 <- c(
  0,
  0.10,
  0.25,
  0.35,
  max(mapa_carteira_2007$freq_carteira, na.rm = TRUE)
)

labels_quartis_autonomo <- c(
  '0 - 10%',
  '10% - 25%',
  '25% - 35%',
  paste0('Mais de 35%')
)

mapa_autonomo_2007_final <- mapa_autonomo_2007 |>
  mutate(
    categoria = cut(
      freq_autonomo,
      breaks = quartis_autonomo_2007,
      labels = labels_quartis_autonomo,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_autonomo_2007_final)

mapa_autonomo_2007_final_2 <- st_intersection(
  st_make_valid(mapa_autonomo_2007_final),
  st_make_valid(mancha_urbana_sp_07)
)

bbox <- st_bbox(mapa_autonomo_2007_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.3
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.3
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_autonomo_2007_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_07, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = '% de pessoas em trabalho autônomo',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

## 2017 ##

pontos_2017_vinc <- indicadores_vinc |>
  filter(ano == 2017) |>
  mutate(
    CO_DOM_X_S = as.numeric(CO_DOM_X_S),
    CO_DOM_Y_S = as.numeric(CO_DOM_Y_S)
  ) |>
  filter(
    !is.na(CO_DOM_X_S),
    !is.na(CO_DOM_Y_S),
    is.finite(CO_DOM_X_S),
    is.finite(CO_DOM_Y_S),
    !is.na(carteira),
    !is.na(autonomo)
  ) |>
  st_as_sf(coords = c('CO_DOM_X_S', 'CO_DOM_Y_S'), crs = 31983) |>
  st_zm(drop = TRUE, what = "ZM")

any(st_is_empty(pontos_2017_vinc))

localiz_2017_vinc <- st_join(pontos_2017_vinc, zonas_2017)

carteira_2017 <- localiz_2017_vinc |>
  select(
    carteira,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(carteira)) |>
  group_by(NumeroZona) |>
  summarise(
    freq_carteira = mean(carteira, na.rm = TRUE)
  )

autonomo_2017 <- localiz_2017_vinc |>
  select(
    autonomo,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(autonomo)) |>
  group_by(NumeroZona) |>
  summarise(
    freq_autonomo = mean(autonomo, na.rm = TRUE)
  )

mapa_carteira_2017 <- zonas_2017 |>
  left_join(carteira_2017, by = 'NumeroZona')

quartis_carteira_2017 <- c(
  0,
  0.25,
  0.50,
  0.75,
  max(mapa_carteira_2017$freq_carteira, na.rm = TRUE)
)

labels_quartis_carteira <- c(
  '0 - 25%',
  '25% - 50%',
  '50% - 75%',
  paste0('Mais de 75%')
)

mapa_carteira_2017_final <- mapa_carteira_2017 |>
  mutate(
    categoria = cut(
      freq_carteira,
      breaks = quartis_carteira_2017,
      labels = labels_quartis_carteira,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_carteira_2017_final)

mapa_carteira_2017_final_2 <- st_intersection(
  st_make_valid(mapa_carteira_2017_final),
  st_make_valid(mancha_urbana_sp_17)
)

bbox <- st_bbox(mapa_carteira_2017_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.0005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.0005
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_carteira_2017_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_17, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = '% de pessoas trabalhando com carteira assinada',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_autonomo_2017 <- zonas_2017 |>
  left_join(autonomo_2017, by = 'NumeroZona')

quartis_autonomo_2017 <- c(
  0,
  0.10,
  0.25,
  0.35,
  max(mapa_carteira_2017$freq_carteira, na.rm = TRUE)
)

labels_quartis_autonomo <- c(
  '0 - 10%',
  '10% - 25%',
  '25% - 35%',
  paste0('Mais de 35%')
)

mapa_autonomo_2017_final <- mapa_autonomo_2017 |>
  mutate(
    categoria = cut(
      freq_autonomo,
      breaks = quartis_autonomo_2017,
      labels = labels_quartis_autonomo,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_autonomo_2017_final)

mapa_autonomo_2017_final_2 <- st_intersection(
  st_make_valid(mapa_autonomo_2017_final),
  st_make_valid(mancha_urbana_sp_17)
)

bbox <- st_bbox(mapa_autonomo_2017_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.0005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.0005
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_autonomo_2017_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_17, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = '% de pessoas em trabalho autônomo',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

## 2023 ##

pontos_2023_vinc <- indicadores_vinc |>
  filter(ano == 2023) |>
  mutate(
    CO_DOM_X_S = as.numeric(CO_DOM_X_S),
    CO_DOM_Y_S = as.numeric(CO_DOM_Y_S)
  ) |>
  filter(
    !is.na(CO_DOM_X_S),
    !is.na(CO_DOM_Y_S),
    is.finite(CO_DOM_X_S),
    is.finite(CO_DOM_Y_S),
    !is.na(carteira),
    !is.na(autonomo)
  ) |>
  st_as_sf(coords = c('CO_DOM_X_S', 'CO_DOM_Y_S'), crs = 31983) |>
  st_zm(drop = TRUE, what = "ZM")

any(st_is_empty(pontos_2023_vinc))

localiz_2023_vinc <- st_join(pontos_2023_vinc, zonas_2023)

carteira_2023 <- localiz_2023_vinc |>
  select(
    carteira,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(carteira)) |>
  group_by(NumeroZona) |>
  summarise(
    freq_carteira = mean(carteira, na.rm = TRUE)
  )

autonomo_2023 <- localiz_2023_vinc |>
  select(
    autonomo,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(autonomo)) |>
  group_by(NumeroZona) |>
  summarise(
    freq_autonomo = mean(autonomo, na.rm = TRUE)
  )

mapa_carteira_2023 <- zonas_2023 |>
  left_join(carteira_2017, by = 'NumeroZona')

quartis_carteira_2023 <- c(
  0,
  0.25,
  0.50,
  0.75,
  max(mapa_carteira_2023$freq_carteira, na.rm = TRUE)
)

labels_quartis_carteira <- c(
  '0 - 25%',
  '25% - 50%',
  '50% - 75%',
  paste0('Mais de 75%')
)

mapa_carteira_2023_final <- mapa_carteira_2023 |>
  mutate(
    categoria = cut(
      freq_carteira,
      breaks = quartis_carteira_2023,
      labels = labels_quartis_carteira,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_carteira_2023_final)

mapa_carteira_2023_final_2 <- st_intersection(
  st_make_valid(mapa_carteira_2023_final),
  st_make_valid(mancha_urbana_sp_23)
)

bbox <- st_bbox(mapa_carteira_2023_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.0005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.0005
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_carteira_2023_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_23, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = '% de pessoas trabalhando com carteira assinada',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_autonomo_2023 <- zonas_2023 |>
  left_join(autonomo_2023, by = 'NumeroZona')

quartis_autonomo_2023 <- c(
  0,
  0.10,
  0.25,
  0.35,
  max(mapa_carteira_2023$freq_carteira, na.rm = TRUE)
)

labels_quartis_autonomo <- c(
  '0 - 10%',
  '10% - 25%',
  '25% - 35%',
  paste0('Mais de 35%')
)

mapa_autonomo_2023_final <- mapa_autonomo_2023 |>
  mutate(
    categoria = cut(
      freq_autonomo,
      breaks = quartis_autonomo_2023,
      labels = labels_quartis_autonomo,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_autonomo_2023_final)

mapa_autonomo_2023_final_2 <- st_intersection(
  st_make_valid(mapa_autonomo_2023_final),
  st_make_valid(mancha_urbana_sp_23)
)

bbox <- st_bbox(mapa_autonomo_2023_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.0005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.0005
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_autonomo_2023_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_23, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'bamako',
    direction = -1,
    name = '% de pessoas em trabalho autônomo',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

### Frequência de pessoas que utilizam ou metrô, ou ônibus municipal ou carro como meio de transporte principal ###

indicadores_transp <- od_completa |>
  select(ano, CO_DOM_X_S, CO_DOM_Y_S, MODOPRIN) |>
  mutate(
    metro = ifelse(MODOPRIN == 1, 1, 0),
    onibus = ifelse(MODOPRIN == c(4, 5, 6), 1, 0),
    carro = ifelse(MODOPRIN == c(9, 10), 1, 0)
  )

## 2007 ##

pontos_2007_transp <- indicadores_transp |>
  filter(ano == 2007) |>
  mutate(
    CO_DOM_X_S = as.numeric(CO_DOM_X_S),
    CO_DOM_Y_S = as.numeric(CO_DOM_Y_S)
  ) |>
  filter(
    !is.na(CO_DOM_X_S),
    !is.na(CO_DOM_Y_S),
    is.finite(CO_DOM_X_S),
    is.finite(CO_DOM_Y_S),
  ) |>
  st_as_sf(coords = c('CO_DOM_X_S', 'CO_DOM_Y_S'), crs = 31983) |>
  st_zm(drop = TRUE, what = "ZM")

any(st_is_empty(pontos_2007_transp))

localiz_2007_transp <- st_join(pontos_2007_transp, zonas_2007)

metro_2007 <- localiz_2007_transp |>
  select(
    metro,
    Zona07,
    NomeZona07,
    Municipio,
    NomeMunici,
    Area_ha,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(metro)) |>
  group_by(Zona07) |>
  summarise(
    freq_metro = mean(metro, na.rm = TRUE)
  )

onibus_2007 <- localiz_2007_transp |>
  select(
    onibus,
    Zona07,
    NomeZona07,
    Municipio,
    NomeMunici,
    Area_ha,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(onibus)) |>
  group_by(Zona07) |>
  summarise(
    freq_onibus = mean(onibus, na.rm = TRUE)
  )

carro_2007 <- localiz_2007_transp |>
  select(
    carro,
    Zona07,
    NomeZona07,
    Municipio,
    NomeMunici,
    Area_ha,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(carro)) |>
  group_by(Zona07) |>
  summarise(
    freq_carro = mean(carro, na.rm = TRUE)
  )

mapa_metro_2007 <- zonas_2007 |>
  left_join(metro_2007, by = 'Zona07')

quartis_metro_2007 <- c(
  0,
  0.05,
  0.10,
  0.20,
  max(mapa_metro_2007$freq_metro, na.rm = TRUE)
)

labels_quartis_metro <- c(
  '0 - 5%',
  '5% - 10%',
  '10% - 20%',
  paste0('Mais de 20%')
)

mapa_metro_2007_final <- mapa_metro_2007 |>
  mutate(
    categoria = cut(
      freq_metro,
      breaks = quartis_metro_2007,
      labels = labels_quartis_metro,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_metro_2007_final)

mapa_metro_2007_final_2 <- st_intersection(
  st_make_valid(mapa_metro_2007_final),
  st_make_valid(mancha_urbana_sp_07)
)

bbox <- st_bbox(mapa_metro_2007_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.3
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.3
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_metro_2007_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_07, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'managua',
    direction = -1,
    name = '% de pessoas usando metrô como transporte principal',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_onibus_2007 <- zonas_2007 |>
  left_join(onibus_2007, by = 'Zona07')

quartis_onibus_2007 <- c(
  0,
  0.03,
  0.06,
  0.10,
  max(mapa_onibus_2007$freq_onibus, na.rm = TRUE)
)

labels_quartis_onibus <- c(
  '0 - 3%',
  '3% - 6%',
  '6% - 10%',
  paste0('Mais de 10%')
)

mapa_onibus_2007_final <- mapa_onibus_2007 |>
  mutate(
    categoria = cut(
      freq_onibus,
      breaks = quartis_onibus_2007,
      labels = labels_quartis_onibus,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_onibus_2007_final)

mapa_onibus_2007_final_2 <- st_intersection(
  st_make_valid(mapa_onibus_2007_final),
  st_make_valid(mancha_urbana_sp_07)
)

bbox <- st_bbox(mapa_onibus_2007_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.3
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.3
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_onibus_2007_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_07, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'managua',
    direction = -1,
    name = '% de pessoas usando ônibus como transporte principal',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_carro_2007 <- zonas_2007 |>
  left_join(carro_2007, by = 'Zona07')

quartis_carro_2007 <- c(
  0,
  0.05,
  0.10,
  0.20,
  max(mapa_carro_2007$freq_carro, na.rm = TRUE)
)

labels_quartis_carro <- c(
  '0 - 5%',
  '5% - 10%',
  '10% - 20%',
  paste0('Mais de 20%')
)

mapa_carro_2007_final <- mapa_carro_2007 |>
  mutate(
    categoria = cut(
      freq_carro,
      breaks = quartis_carro_2007,
      labels = labels_quartis_carro,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_carro_2007_final)

mapa_carro_2007_final_2 <- st_intersection(
  st_make_valid(mapa_carro_2007_final),
  st_make_valid(mancha_urbana_sp_07)
)

bbox <- st_bbox(mapa_carro_2007_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.3
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.3
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_carro_2007_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_07, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'managua',
    direction = -1,
    name = '% de pessoas usando carro como transporte principal',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

## 2017 ##

pontos_2017_transp <- indicadores_transp |>
  filter(ano == 2017) |>
  mutate(
    CO_DOM_X_S = as.numeric(CO_DOM_X_S),
    CO_DOM_Y_S = as.numeric(CO_DOM_Y_S)
  ) |>
  filter(
    !is.na(CO_DOM_X_S),
    !is.na(CO_DOM_Y_S),
    is.finite(CO_DOM_X_S),
    is.finite(CO_DOM_Y_S)
  ) |>
  st_as_sf(coords = c('CO_DOM_X_S', 'CO_DOM_Y_S'), crs = 31983) |>
  st_zm(drop = TRUE, what = "ZM")

any(st_is_empty(pontos_2017_transp))

localiz_2017_transp <- st_join(pontos_2017_transp, zonas_2017)

metro_2017 <- localiz_2017_transp |>
  select(
    metro,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(metro)) |>
  group_by(NumeroZona) |>
  summarise(
    freq_metro = mean(metro, na.rm = TRUE)
  )

onibus_2017 <- localiz_2017_transp |>
  select(
    onibus,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(onibus)) |>
  group_by(NumeroZona) |>
  summarise(
    freq_onibus = mean(onibus, na.rm = TRUE)
  )

carro_2017 <- localiz_2017_transp |>
  select(
    carro,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(carro)) |>
  group_by(NumeroZona) |>
  summarise(
    freq_carro = mean(carro, na.rm = TRUE)
  )

mapa_metro_2017 <- zonas_2017 |>
  left_join(metro_2017, by = 'NumeroZona')

quartis_metro_2017 <- c(
  0,
  0.05,
  0.10,
  0.20,
  max(mapa_metro_2017$freq_metro, na.rm = TRUE)
)

labels_quartis_metro <- c(
  '0 - 5%',
  '5% - 10%',
  '10% - 20%',
  paste0('Mais de 20%')
)

mapa_metro_2017_final <- mapa_metro_2017 |>
  mutate(
    categoria = cut(
      freq_metro,
      breaks = quartis_metro_2017,
      labels = labels_quartis_metro,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_metro_2017_final)

mapa_metro_2017_final_2 <- st_intersection(
  st_make_valid(mapa_metro_2017_final),
  st_make_valid(mancha_urbana_sp_17)
)

bbox <- st_bbox(mapa_metro_2017_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.0005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.0005
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_metro_2017_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_17, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'managua',
    direction = -1,
    name = '% de pessoas usando metrô como transporte principal',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_onibus_2017 <- zonas_2017 |>
  left_join(onibus_2017, by = 'NumeroZona')

quartis_onibus_2017 <- c(
  0,
  0.03,
  0.06,
  0.10,
  max(mapa_onibus_2017$freq_onibus, na.rm = TRUE)
)

labels_quartis_onibus <- c(
  '0 - 3%',
  '3% - 6%',
  '6% - 10%',
  paste0('Mais de 10%')
)

mapa_onibus_2017_final <- mapa_onibus_2017 |>
  mutate(
    categoria = cut(
      freq_onibus,
      breaks = quartis_onibus_2017,
      labels = labels_quartis_onibus,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_onibus_2017_final)

mapa_onibus_2017_final_2 <- st_intersection(
  st_make_valid(mapa_onibus_2017_final),
  st_make_valid(mancha_urbana_sp_17)
)

bbox <- st_bbox(mapa_onibus_2017_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.0005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.0005
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_onibus_2017_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_17, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'managua',
    direction = -1,
    name = '% de pessoas usando ônibus como transporte principal',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_carro_2017 <- zonas_2017 |>
  left_join(carro_2017, by = 'NumeroZona')

quartis_carro_2017 <- c(
  0,
  0.05,
  0.10,
  0.20,
  max(mapa_carro_2017$freq_carro, na.rm = TRUE)
)

labels_quartis_carro <- c(
  '0 - 5%',
  '5% - 10%',
  '10% - 20%',
  paste0('Mais de 20%')
)

mapa_carro_2017_final <- mapa_carro_2017 |>
  mutate(
    categoria = cut(
      freq_carro,
      breaks = quartis_carro_2017,
      labels = labels_quartis_carro,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_carro_2017_final)

mapa_carro_2017_final_2 <- st_intersection(
  st_make_valid(mapa_carro_2017_final),
  st_make_valid(mancha_urbana_sp_17)
)

bbox <- st_bbox(mapa_carro_2017_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.0005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.0005
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_carro_2017_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_17, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'managua',
    direction = -1,
    name = '% de pessoas usando carro como transporte principal',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

## 2023 ##

pontos_2023_transp <- indicadores_transp |>
  filter(ano == 2023) |>
  mutate(
    CO_DOM_X_S = as.numeric(CO_DOM_X_S),
    CO_DOM_Y_S = as.numeric(CO_DOM_Y_S)
  ) |>
  filter(
    !is.na(CO_DOM_X_S),
    !is.na(CO_DOM_Y_S),
    is.finite(CO_DOM_X_S),
    is.finite(CO_DOM_Y_S)
  ) |>
  st_as_sf(coords = c('CO_DOM_X_S', 'CO_DOM_Y_S'), crs = 31983) |>
  st_zm(drop = TRUE, what = "ZM")

any(st_is_empty(pontos_2023_transp))

localiz_2023_transp <- st_join(pontos_2023_transp, zonas_2023)

metro_2023 <- localiz_2023_transp |>
  select(
    metro,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(metro)) |>
  group_by(NumeroZona) |>
  summarise(
    freq_metro = mean(metro, na.rm = TRUE)
  )

onibus_2023 <- localiz_2023_transp |>
  select(
    onibus,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(onibus)) |>
  group_by(NumeroZona) |>
  summarise(
    freq_onibus = mean(onibus, na.rm = TRUE)
  )

carro_2023 <- localiz_2023_transp |>
  select(
    carro,
    NumeroZona,
    NomeZona,
    NumeroMuni,
    NomeMunici,
    Area_ha_2,
    geometry
  ) |>
  st_drop_geometry() |>
  filter(!is.na(carro)) |>
  group_by(NumeroZona) |>
  summarise(
    freq_carro = mean(carro, na.rm = TRUE)
  )

mapa_metro_2023 <- zonas_2023 |>
  left_join(metro_2023, by = 'NumeroZona')

quartis_metro_2023 <- c(
  0,
  0.05,
  0.10,
  0.20,
  max(mapa_metro_2023$freq_metro, na.rm = TRUE)
)

labels_quartis_metro <- c(
  '0 - 5%',
  '5% - 10%',
  '10% - 20%',
  paste0('Mais de 20%')
)

mapa_metro_2023_final <- mapa_metro_2023 |>
  mutate(
    categoria = cut(
      freq_metro,
      breaks = quartis_metro_2023,
      labels = labels_quartis_metro,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_metro_2023_final)

mapa_metro_2023_final_2 <- st_intersection(
  st_make_valid(mapa_metro_2023_final),
  st_make_valid(mancha_urbana_sp_23)
)

bbox <- st_bbox(mapa_metro_2023_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.0005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.0005
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_metro_2023_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_23, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'managua',
    direction = -1,
    name = '% de pessoas usando metrô como transporte principal',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_onibus_2023 <- zonas_2023 |>
  left_join(onibus_2023, by = 'NumeroZona')

quartis_onibus_2023 <- c(
  0,
  0.03,
  0.06,
  0.10,
  max(mapa_onibus_2023$freq_onibus, na.rm = TRUE)
)

labels_quartis_onibus <- c(
  '0 - 3%',
  '3% - 6%',
  '6% - 10%',
  paste0('Mais de 10%')
)

mapa_onibus_2023_final <- mapa_onibus_2023 |>
  mutate(
    categoria = cut(
      freq_onibus,
      breaks = quartis_onibus_2023,
      labels = labels_quartis_onibus,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_onibus_2023_final)

mapa_onibus_2023_final_2 <- st_intersection(
  st_make_valid(mapa_onibus_2023_final),
  st_make_valid(mancha_urbana_sp_23)
)

bbox <- st_bbox(mapa_onibus_2023_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.0005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.0005
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_onibus_2023_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_23, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'managua',
    direction = -1,
    name = '% de pessoas usando ônibus como transporte principal',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_carro_2023 <- zonas_2023 |>
  left_join(carro_2023, by = 'NumeroZona')

quartis_carro_2023 <- c(
  0,
  0.05,
  0.10,
  0.20,
  max(mapa_carro_2023$freq_carro, na.rm = TRUE)
)

labels_quartis_carro <- c(
  '0 - 5%',
  '5% - 10%',
  '10% - 20%',
  paste0('Mais de 20%')
)

mapa_carro_2023_final <- mapa_carro_2023 |>
  mutate(
    categoria = cut(
      freq_carro,
      breaks = quartis_carro_2023,
      labels = labels_quartis_carro,
      include.lowest = TRUE
    )
  ) |>
  st_make_valid(mapa_carro_2023_final)

mapa_carro_2023_final_2 <- st_intersection(
  st_make_valid(mapa_carro_2023_final),
  st_make_valid(mancha_urbana_sp_23)
)

bbox <- st_bbox(mapa_carro_2023_final_2)
margem_x <- (bbox["xmax"] - bbox["xmin"]) * 0.0005
margem_y <- (bbox["ymax"] - bbox["ymin"]) * 0.0005
xlim <- c(bbox["xmin"] - margem_x, bbox["xmax"] + margem_x)
ylim <- c(bbox["ymin"] - margem_y, bbox["ymax"] + margem_y)

ggplot() +
  geom_sf(
    data = mapa_carro_2023_final_2,
    aes(fill = categoria),
    color = 'grey',
    size = 0.1
  ) +
  geom_sf(data = mun_23, fill = NA, color = 'grey10', size = 1) +
  scale_fill_scico_d(
    palette = 'managua',
    direction = -1,
    name = '% de pessoas usando carro como transporte principal',
    na.value = 'grey90',
    breaks = function(x) na.omit(x),

    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', vjust = 1),
    legend.text = element_text(size = 9, margin = margin(r = 1, unit = "cm")),
    plot.background = element_rect(fill = "white", color = NA)
  )
