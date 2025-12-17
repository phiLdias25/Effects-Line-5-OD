###### Estatísticas Descritivas e formação do grupo de controle pelo MatchIt ######

##### Abrindo Bibliotecas #####

library(tidyverse)
library(MatchIt)
library(cobalt)
library(stargazer)
library(rio)
library(sf)
library(kableExtra)

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
  select(ano, VL_REN_I, CD_ATIVI, VINC1) |>
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
  filter(!is.na(VL_REN_I)) |>
  summarise(
    Valor = mean(VL_REN_I, na.rm = TRUE),
    Variavel = 'Renda Individual Média (R$)'
  ) |> 
  ungroup() |>
  mutate(Valor = round(Valor, 2)) |>
  select(ano, Variavel, Valor)

  