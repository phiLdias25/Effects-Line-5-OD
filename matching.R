###### Criação do grupo de controle por MatchIt ######

rm(list = ls())

##### Abrindo bibliotecas #####

library(tidyverse)
library(MatchIt)
library(cobalt)
library(rio)
library(sf)
library(kableExtra)
library(scales)
library(scico)

##### Abrindo base de dados #####

od_completa <- import('od_base_completa.dbf') |>
  rename(
    trab_cen = trab_centr,
    educ_cen = educ_centr,
    saude_cen = saude_cent,
    lazer_cen = lazer_cent,
    comp_cen = compras_ce,
    emp_cen = emp_centro,
    trabeducemp = trab_educ_,
    trabemp = trab_emp_c,
    lazcompsa = lazer_comp,
    lazcomp = lazer_comp.1,
    trabeduc = trab_educ_.1
  )


#### Filtrando o grupo de controle paramétrico da base para criar o grupo de controle por pareamento e adicionando as variáveis para o pareamento ####

od_matching <- od_completa |>
  filter(tipo_grupo != 'Morador_Centro_Exp') |>
  filter(tipo_grupo != 'Controle_Linhas_Futuras') |>
  filter(tipo_grupo != 'Controle_CPTM') |>
  mutate(tratamento_binario = ifelse(tipo_grupo == 'Tratamento', 1, 0)) |>
  select(
    tipo_grupo,
    tratamento_binario,
    IDADE,
    SEXO,
    GRAU_INS,
    CD_ATIVI,
    VL_REN_I_D,
    TIPVG,
    ano,
    indic_dest,
    trab_cen,
    educ_cen,
    saude_cen,
    lazer_cen,
    comp_cen,
    emp_cen,
    trabeducemp,
    trabemp,
    lazcompsa,
    lazcomp,
    trabeduc,
    ZONA_O
  ) |>
  mutate(
    SEXO = case_when(
      SEXO == 1 ~ 'Masculino',
      SEXO == 2 ~ 'Feminino',
      TRUE ~ NA_character_
    ),
    SEXO = as.factor(SEXO),
    GRAU_INS = case_when(
      GRAU_INS == 1 ~ 'Não alfabetizado/ Fund 1 incompleto',
      GRAU_INS == 2 ~ 'Fund 1 completo/ Fund 2 incompleto',
      GRAU_INS == 3 ~ 'Fund 2 completo/ Médio incompleto',
      GRAU_INS == 4 ~ 'Médio completo/ Superior incompleto',
      GRAU_INS == 5 ~ 'Superior completo',
      TRUE ~ NA_character_
    ),
    GRAU_INS = as.factor(GRAU_INS),
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
    CD_ATIVI = as.factor(CD_ATIVI)
  ) |>
  mutate(VIAGEM_TRANS_PUBL = ifelse(TIPVG == 1, 1, 0)) |>
  drop_na()

##### Fazendo o pareamento #####

set.seed(42)

match <- matchit(
  tratamento_binario ~ IDADE +
    SEXO +
    GRAU_INS +
    CD_ATIVI +
    VL_REN_I_D +
    VIAGEM_TRANS_PUBL,
  data = od_matching,
  method = "nearest"
)

summary(match)

base_completa_match <- match.data(match)

export(base_completa_match, 'base_pareamento.dbf')

##### Fazendo os outros grupos de controle paramétricos #####

#### Grupo de controle paramétrico 1 - Linhas Futuras ####

od_linhas_futuras <- od_completa |>
  filter(tipo_grupo != 'Morador_Centro_Exp') |>
  filter(tipo_grupo != 'Candidatos_Controle_MatchIt') |>
  filter(tipo_grupo != 'Controle_CPTM') |>
  mutate(tratamento_binario = ifelse(tipo_grupo == 'Tratamento', 1, 0)) |>
  select(
    tipo_grupo,
    tratamento_binario,
    IDADE,
    SEXO,
    GRAU_INS,
    CD_ATIVI,
    VL_REN_I_D,
    TIPVG,
    ano,
    indic_dest,
    trab_cen,
    educ_cen,
    saude_cen,
    lazer_cen,
    comp_cen,
    emp_cen,
    trabeducemp,
    trabemp,
    lazcompsa,
    lazcomp,
    trabeduc,
    ZONA_O
  ) |>
  mutate(
    SEXO = case_when(
      SEXO == 1 ~ 'Masculino',
      SEXO == 2 ~ 'Feminino',
      TRUE ~ NA_character_
    ),
    SEXO = as.factor(SEXO),
    GRAU_INS = case_when(
      GRAU_INS == 1 ~ 'Não alfabetizado/ Fund 1 incompleto',
      GRAU_INS == 2 ~ 'Fund 1 completo/ Fund 2 incompleto',
      GRAU_INS == 3 ~ 'Fund 2 completo/ Médio incompleto',
      GRAU_INS == 4 ~ 'Médio completo/ Superior incompleto',
      GRAU_INS == 5 ~ 'Superior completo',
      TRUE ~ NA_character_
    ),
    GRAU_INS = as.factor(GRAU_INS),
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
    CD_ATIVI = as.factor(CD_ATIVI)
  ) |>
  mutate(VIAGEM_TRANS_PUBL = ifelse(TIPVG == 1, 1, 0)) |>
  drop_na()

base_controle_linhas_futuras <- export(
  od_linhas_futuras,
  'base_linhas_futuras.dbf'
)

#### Grupo de controle paramétrico 2 - CPTM ####

od_cptm <- od_completa |>
  filter(tipo_grupo != 'Morador_Centro_Exp') |>
  filter(tipo_grupo != 'Candidatos_Controle_MatchIt') |>
  filter(tipo_grupo != 'Controle_Linhas_Futuras') |>
  mutate(tratamento_binario = ifelse(tipo_grupo == 'Tratamento', 1, 0)) |>
  select(
    tipo_grupo,
    tratamento_binario,
    IDADE,
    SEXO,
    GRAU_INS,
    CD_ATIVI,
    VL_REN_I_D,
    TIPVG,
    ano,
    indic_dest,
    trab_cen,
    educ_cen,
    saude_cen,
    lazer_cen,
    comp_cen,
    emp_cen,
    trabeducemp,
    trabemp,
    lazcompsa,
    lazcomp,
    trabeduc,
    ZONA_O
  ) |>
  mutate(
    SEXO = case_when(
      SEXO == 1 ~ 'Masculino',
      SEXO == 2 ~ 'Feminino',
      TRUE ~ NA_character_
    ),
    SEXO = as.factor(SEXO),
    GRAU_INS = case_when(
      GRAU_INS == 1 ~ 'Não alfabetizado/ Fund 1 incompleto',
      GRAU_INS == 2 ~ 'Fund 1 completo/ Fund 2 incompleto',
      GRAU_INS == 3 ~ 'Fund 2 completo/ Médio incompleto',
      GRAU_INS == 4 ~ 'Médio completo/ Superior incompleto',
      GRAU_INS == 5 ~ 'Superior completo',
      TRUE ~ NA_character_
    ),
    GRAU_INS = as.factor(GRAU_INS),
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
    CD_ATIVI = as.factor(CD_ATIVI)
  ) |>
  mutate(VIAGEM_TRANS_PUBL = ifelse(TIPVG == 1, 1, 0)) |>
  drop_na()

base_controle_cptm <- export(od_cptm, 'base_cptm.dbf')

##### Fazendo o diagnóstico do pareamento #####

tabela_comparativa <- bal.tab(match, un = TRUE, thresholds = c(m = .1))

print(tabela_comparativa)

plot_comparativo <- love.plot(
  match,
  binary = 'std',
  thresholds = c(m = .1),
  abs = TRUE,
  var.order = 'unadjusted',
  shapes = c('circle', 'triangle'),
  colors = c('red', 'blue'),
  sample.names = c('Antes do Pareamento', 'Depois do Pareamento')
)

print(plot_comparativo)

##### Analisando estatísticas descritivas dos grupos de controle criados, tanto os paramétricos quanto o por pareamento #####

#### Grupo de controle 1 - Linhas Futuras ####

od_estat_linhas <- od_completa |>
  filter(tipo_grupo == 'Controle_Linhas_Futuras') |>
  select(IDADE, SEXO, GRAU_INS, CD_ATIVI, VL_REN_I_D, TIPVG) |>
  mutate(
    SEXO = case_when(
      SEXO == 1 ~ 'Masculino',
      SEXO == 2 ~ 'Feminino',
      TRUE ~ NA_character_
    ),
    SEXO = as.factor(SEXO),
    GRAU_INS = case_when(
      GRAU_INS == 1 ~ 'Não alfabetizado/ Fund 1 incompleto',
      GRAU_INS == 2 ~ 'Fund 1 completo/ Fund 2 incompleto',
      GRAU_INS == 3 ~ 'Fund 2 completo/ Médio incompleto',
      GRAU_INS == 4 ~ 'Médio completo/ Superior incompleto',
      GRAU_INS == 5 ~ 'Superior completo',
      TRUE ~ NA_character_
    ),
    GRAU_INS = as.factor(GRAU_INS),
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
    CD_ATIVI = as.factor(CD_ATIVI),
    TIPVG = case_when(
      TIPVG == 1 ~ 'Coletivo',
      TIPVG == 2 ~ 'Particular',
      TIPVG == 3 ~ 'A pé',
      TIPVG == 4 ~ 'Bicicleta',
      TRUE ~ NA_character_
    ),
    TIPVG = as.factor(TIPVG)
  ) |>
  drop_na()

idade_anual_linhas <- od_estat_linhas |>
  summarise(
    Valor = mean(IDADE, na.rm = TRUE),
    Variavel = 'Idade Média (Anos)'
  ) |>
  mutate(Valor = round(Valor, 1)) |>
  ungroup() |>
  select(Variavel, Valor)

categorias_anual_linhas <- od_estat_linhas |>
  select(-IDADE) |>
  pivot_longer(
    cols = c(SEXO, GRAU_INS, CD_ATIVI, TIPVG),
    names_to = 'origem',
    values_to = 'categoria'
  ) |>
  filter(!is.na(categoria)) |>
  count(origem, categoria) |>
  group_by(origem) |>
  mutate(
    prob = n / sum(n),
    Valor = round(prob, 3)
  ) |>
  ungroup() |>
  select(Variavel = categoria, Valor)

renda_anual_linhas <- od_estat_linhas |>
  filter(!is.na(VL_REN_I_D)) |>
  summarise(
    Valor = mean(VL_REN_I_D, na.rm = TRUE),
    Variavel = 'Renda Individual Média (R$)'
  ) |>
  ungroup() |>
  mutate(Valor = round(Valor, 2)) |>
  select(Variavel, Valor)

tabela_descrit_linhas <- bind_rows(
  idade_anual_linhas,
  renda_anual_linhas,
  categorias_anual_linhas
) |>
  arrange(factor(
    Variavel,
    levels = c(
      'Idade Média (Anos)',
      'Renda Individual Média (R$)',
      'Masculino',
      'Feminino',
      'Não alfabetizado/ Fund 1 incompleto',
      'Fund 1 completo/ Fund 2 incompleto',
      'Fund 2 completo/ Médio incompleto',
      'Médio completo/ Superior incompleto',
      'Superior completo',
      'Trabalho Regular',
      'Bico',
      'Licença Médica',
      'Aposentado/Pensionista',
      'Desempregado',
      'Nunca Trabalhou',
      'Dona de Casa',
      'Estudante',
      'Coletivo',
      'Particular',
      'A pé',
      'Bicicleta'
    )
  ))

tabela_latex_linhas <- tabela_descrit_linhas |>
  kbl(
    format = 'latex',
    booktabs = TRUE,
    label = 'tab:descritivas_linhas_futuras'
  ) |>
  kable_styling(
    latex_options = c('striped', 'hold_position'),
    position = 'center'
  )

#### Grupo de controle 2 - CPTM ####

od_estat_cptm <- od_completa |>
  filter(tipo_grupo == 'Controle_CPTM') |>
  select(IDADE, SEXO, GRAU_INS, CD_ATIVI, VL_REN_I_D, TIPVG) |>
  mutate(
    SEXO = case_when(
      SEXO == 1 ~ 'Masculino',
      SEXO == 2 ~ 'Feminino',
      TRUE ~ NA_character_
    ),
    SEXO = as.factor(SEXO),
    GRAU_INS = case_when(
      GRAU_INS == 1 ~ 'Não alfabetizado/ Fund 1 incompleto',
      GRAU_INS == 2 ~ 'Fund 1 completo/ Fund 2 incompleto',
      GRAU_INS == 3 ~ 'Fund 2 completo/ Médio incompleto',
      GRAU_INS == 4 ~ 'Médio completo/ Superior incompleto',
      GRAU_INS == 5 ~ 'Superior completo',
      TRUE ~ NA_character_
    ),
    GRAU_INS = as.factor(GRAU_INS),
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
    CD_ATIVI = as.factor(CD_ATIVI),
    TIPVG = case_when(
      TIPVG == 1 ~ 'Coletivo',
      TIPVG == 2 ~ 'Particular',
      TIPVG == 3 ~ 'A pé',
      TIPVG == 4 ~ 'Bicicleta',
      TRUE ~ NA_character_
    ),
    TIPVG = as.factor(TIPVG)
  ) |>
  drop_na()

idade_anual_cptm <- od_estat_cptm |>
  summarise(
    Valor = mean(IDADE, na.rm = TRUE),
    Variavel = 'Idade Média (Anos)'
  ) |>
  mutate(Valor = round(Valor, 1)) |>
  ungroup() |>
  select(Variavel, Valor)

categorias_anual_cptm <- od_estat_cptm |>
  select(-IDADE) |>
  pivot_longer(
    cols = c(SEXO, GRAU_INS, CD_ATIVI, TIPVG),
    names_to = 'origem',
    values_to = 'categoria'
  ) |>
  filter(!is.na(categoria)) |>
  count(origem, categoria) |>
  group_by(origem) |>
  mutate(
    prob = n / sum(n),
    Valor = round(prob, 3)
  ) |>
  ungroup() |>
  select(Variavel = categoria, Valor)

renda_anual_cptm <- od_estat_cptm |>
  filter(!is.na(VL_REN_I_D)) |>
  summarise(
    Valor = mean(VL_REN_I_D, na.rm = TRUE),
    Variavel = 'Renda Individual Média (R$)'
  ) |>
  ungroup() |>
  mutate(Valor = round(Valor, 2)) |>
  select(Variavel, Valor)

tabela_descrit_cptm <- bind_rows(
  idade_anual_cptm,
  renda_anual_cptm,
  categorias_anual_cptm
) |>
  arrange(factor(
    Variavel,
    levels = c(
      'Idade Média (Anos)',
      'Renda Individual Média (R$)',
      'Masculino',
      'Feminino',
      'Não alfabetizado/ Fund 1 incompleto',
      'Fund 1 completo/ Fund 2 incompleto',
      'Fund 2 completo/ Médio incompleto',
      'Médio completo/ Superior incompleto',
      'Superior completo',
      'Trabalho Regular',
      'Bico',
      'Licença Médica',
      'Aposentado/Pensionista',
      'Desempregado',
      'Nunca Trabalhou',
      'Dona de Casa',
      'Estudante',
      'Coletivo',
      'Particular',
      'A pé',
      'Bicicleta'
    )
  ))

tabela_latex_cptm <- tabela_descrit_cptm |>
  kbl(
    format = 'latex',
    booktabs = TRUE,
    label = 'tab:descritivas_cptm'
  ) |>
  kable_styling(
    latex_options = c('striped', 'hold_position'),
    position = 'center'
  )

#### Grupo de controle pareado ####

od_pareada <- match.data(match) |>
  mutate(
    categoria = ifelse(tratamento_binario == 1, 'Tratados', 'Controle Pareado')
  ) |>
  filter(categoria == 'Controle Pareado') |>
  select(IDADE, SEXO, GRAU_INS, CD_ATIVI, VL_REN_I_D, TIPVG) |>
  mutate(
    TIPVG = case_when(
      TIPVG == 1 ~ 'Coletivo',
      TIPVG == 2 ~ 'Particular',
      TIPVG == 3 ~ 'A pé',
      TIPVG == 4 ~ 'Bicicleta',
      TRUE ~ NA_character_
    ),
    TIPVG = as.factor(TIPVG)
  ) |>
  drop_na()

idade_anual_match <- od_pareada |>
  summarise(
    Valor = mean(IDADE, na.rm = TRUE),
    Variavel = 'Idade Média (Anos)'
  ) |>
  mutate(Valor = round(Valor, 1)) |>
  ungroup() |>
  select(Variavel, Valor)

categorias_anual_match <- od_pareada |>
  select(-IDADE) |>
  pivot_longer(
    cols = c(SEXO, GRAU_INS, CD_ATIVI, TIPVG),
    names_to = 'origem',
    values_to = 'categoria'
  ) |>
  filter(!is.na(categoria)) |>
  count(origem, categoria) |>
  group_by(origem) |>
  mutate(
    prob = n / sum(n),
    Valor = round(prob, 3)
  ) |>
  ungroup() |>
  select(Variavel = categoria, Valor)

renda_anual_match <- od_pareada |>
  filter(!is.na(VL_REN_I_D)) |>
  summarise(
    Valor = mean(VL_REN_I_D, na.rm = TRUE),
    Variavel = 'Renda Individual Média (R$)'
  ) |>
  ungroup() |>
  mutate(Valor = round(Valor, 2)) |>
  select(Variavel, Valor)

tabela_descrit_match <- bind_rows(
  idade_anual_match,
  renda_anual_match,
  categorias_anual_match
) |>
  arrange(factor(
    Variavel,
    levels = c(
      'Idade Média (Anos)',
      'Renda Individual Média (R$)',
      'Masculino',
      'Feminino',
      'Não alfabetizado/ Fund 1 incompleto',
      'Fund 1 completo/ Fund 2 incompleto',
      'Fund 2 completo/ Médio incompleto',
      'Médio completo/ Superior incompleto',
      'Superior completo',
      'Trabalho Regular',
      'Bico',
      'Licença Médica',
      'Aposentado/Pensionista',
      'Desempregado',
      'Nunca Trabalhou',
      'Dona de Casa',
      'Estudante',
      'Coletivo',
      'Particular',
      'A pé',
      'Bicicleta'
    )
  ))

tabela_latex_match <- tabela_descrit_match |>
  kbl(
    format = 'latex',
    booktabs = TRUE,
    label = 'tab:descritivas_match'
  ) |>
  kable_styling(
    latex_options = c('striped', 'hold_position'),
    position = 'center'
  )

#### Grupo Tratado ####

od_tratado <- match.data(match) |>
  mutate(
    categoria = ifelse(tratamento_binario == 1, 'Tratados', 'Controle Pareado')
  ) |>
  filter(categoria == 'Tratados') |>
  select(IDADE, SEXO, GRAU_INS, CD_ATIVI, VL_REN_I_D, TIPVG) |>
  mutate(
    TIPVG = case_when(
      TIPVG == 1 ~ 'Coletivo',
      TIPVG == 2 ~ 'Particular',
      TIPVG == 3 ~ 'A pé',
      TIPVG == 4 ~ 'Bicicleta',
      TRUE ~ NA_character_
    ),
    TIPVG = as.factor(TIPVG)
  ) |>
  drop_na()

idade_anual_tratado <- od_tratado |>
  summarise(
    Valor = mean(IDADE, na.rm = TRUE),
    Variavel = 'Idade Média (Anos)'
  ) |>
  mutate(Valor = round(Valor, 1)) |>
  ungroup() |>
  select(Variavel, Valor)

categorias_anual_tratado <- od_tratado |>
  select(-IDADE) |>
  pivot_longer(
    cols = c(SEXO, GRAU_INS, CD_ATIVI, TIPVG),
    names_to = 'origem',
    values_to = 'categoria'
  ) |>
  filter(!is.na(categoria)) |>
  count(origem, categoria) |>
  group_by(origem) |>
  mutate(
    prob = n / sum(n),
    Valor = round(prob, 3)
  ) |>
  ungroup() |>
  select(Variavel = categoria, Valor)

renda_anual_tratado <- od_tratado |>
  filter(!is.na(VL_REN_I_D)) |>
  summarise(
    Valor = mean(VL_REN_I_D, na.rm = TRUE),
    Variavel = 'Renda Individual Média (R$)'
  ) |>
  ungroup() |>
  mutate(Valor = round(Valor, 2)) |>
  select(Variavel, Valor)

tabela_descrit_tratado <- bind_rows(
  idade_anual_tratado,
  renda_anual_tratado,
  categorias_anual_tratado
) |>
  arrange(factor(
    Variavel,
    levels = c(
      'Idade Média (Anos)',
      'Renda Individual Média (R$)',
      'Masculino',
      'Feminino',
      'Não alfabetizado/ Fund 1 incompleto',
      'Fund 1 completo/ Fund 2 incompleto',
      'Fund 2 completo/ Médio incompleto',
      'Médio completo/ Superior incompleto',
      'Superior completo',
      'Trabalho Regular',
      'Bico',
      'Licença Médica',
      'Aposentado/Pensionista',
      'Desempregado',
      'Nunca Trabalhou',
      'Dona de Casa',
      'Estudante',
      'Coletivo',
      'Particular',
      'A pé',
      'Bicicleta'
    )
  ))

tabela_latex_tratado <- tabela_descrit_tratado |>
  kbl(
    format = 'latex',
    booktabs = TRUE,
    label = 'tab:descritivas_tratado'
  ) |>
  kable_styling(
    latex_options = c('striped', 'hold_position'),
    position = 'center'
  )
