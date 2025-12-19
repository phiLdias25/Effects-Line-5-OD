###### Estatísticas Descritivas e formação do grupo de controle pelo MatchIt ######

rm(list = ls())

##### Abrindo Bibliotecas #####

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

tabela_latex <- tabela_descrit_sociais |>
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

ggplot(mapa_renda_2007) +
  geom_sf(aes(fill = Renda_Media), color = 'NA', size = 0.1) +
  scale_fill_scico(
    palette = 'lajolla',
    direction = -1,
    name = 'Renda Real (R$)',
    na.value = 'grey90',
    limits = c(0, 15000),
    oob = squish,
    breaks = seq(0, 15000, by = 5000),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 1.0,
      frame.colour = 'black',
      barwidth = 15,
      barheight = 1
    )
  ) +
  coord_sf(clip = 'off') +
  theme_void() +
  theme(legend.position = 'bottom', legend.key.width = unit(1, "cm"))

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

ggplot(mapa_renda_2017) +
  geom_sf(aes(fill = Renda_Media), color = 'NA', size = 0.1) +
  scale_fill_scico(
    palette = 'lajolla',
    direction = -1,
    name = 'Renda Real (R$)',
    na.value = 'grey90',
    limits = c(0, 15000),
    oob = squish,
    breaks = seq(0, 15000, by = 5000),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 1.0,
      frame.colour = 'black',
      barwidth = 15,
      barheight = 1
    )
  ) +
  coord_sf(clip = 'off') +
  theme_void() +
  theme(legend.position = 'bottom', legend.key.width = unit(1, "cm"))

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

ggplot(mapa_renda_2023) +
  geom_sf(aes(fill = Renda_Media), color = 'NA', size = 0.1) +
  scale_fill_scico(
    palette = 'lajolla',
    direction = -1,
    name = 'Renda Real (R$)',
    na.value = 'grey90',
    limits = c(0, 15000),
    oob = squish,
    breaks = seq(0, 15000, by = 5000),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 1.0,
      frame.colour = 'black',
      barwidth = 15,
      barheight = 1
    )
  ) +
  coord_sf(clip = 'off') +
  theme_void() +
  theme(legend.position = 'bottom', legend.key.width = unit(1, "cm"))

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

ggplot(mapa_duracao_2007) +
  geom_sf(aes(fill = Duracao_media), color = 'NA', size = 0.1) +
  scale_fill_scico(
    palette = 'oslo',
    direction = -1,
    name = 'Tempo Médio de Viagem (Minutos)',
    na.value = 'grey90',
    limits = c(2, 60),
    oob = squish,
    breaks = seq(0, 60, by = 10),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 1.0,
      frame.colour = 'black',
      barwidth = 15,
      barheight = 1
    )
  ) +
  coord_sf(clip = 'off') +
  theme_void() +
  theme(legend.position = 'bottom', legend.key.width = unit(1, "cm"))

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

ggplot(mapa_duracao_2017) +
  geom_sf(aes(fill = Duracao_Media), color = 'NA', size = 0.1) +
  scale_fill_scico(
    palette = 'oslo',
    direction = -1,
    name = 'Tempo Médio de Viagem (Minutos)',
    na.value = 'grey90',
    limits = c(2, 60),
    oob = squish,
    breaks = seq(0, 60, by = 10),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 1.0,
      frame.colour = 'black',
      barwidth = 15,
      barheight = 1
    )
  ) +
  coord_sf(clip = 'off') +
  theme_void() +
  theme(legend.position = 'bottom', legend.key.width = unit(1, "cm"))

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

ggplot(mapa_duracao_2023) +
  geom_sf(aes(fill = Duracao_Media), color = 'NA', size = 0.1) +
  scale_fill_scico(
    palette = 'oslo',
    direction = -1,
    name = 'Tempo Médio de Viagem (Minutos)',
    na.value = 'grey90',
    limits = c(2, 60),
    oob = squish,
    breaks = seq(0, 60, by = 10),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 1.0,
      frame.colour = 'black',
      barwidth = 15,
      barheight = 1
    )
  ) +
  coord_sf(clip = 'off') +
  theme_void() +
  theme(legend.position = 'bottom', legend.key.width = unit(1, "cm"))
