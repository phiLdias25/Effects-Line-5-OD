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
    ZMC,
    CO_DOM_X_S,
    CO_DOM_Y_S,
    ZONA,
    tipo_grupo,
    tratamento_binario,
    IDADE,
    SEXO,
    GRAU_INS,
    CD_ATIVI,
    ln_renda_f,
    viaja,
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
    metro,
    carro,
    onibus,
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
    ln_renda_f +
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
    ZMC,
    CO_DOM_X_S,
    CO_DOM_Y_S,
    ZONA,
    tipo_grupo,
    tratamento_binario,
    IDADE,
    SEXO,
    GRAU_INS,
    CD_ATIVI,
    ln_renda_f,
    viaja,
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
    metro,
    carro,
    onibus,
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
    ZMC,
    CO_DOM_X_S,
    CO_DOM_Y_S,
    ZONA,
    tipo_grupo,
    tratamento_binario,
    IDADE,
    SEXO,
    GRAU_INS,
    CD_ATIVI,
    ln_renda_f,
    viaja,
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
    metro,
    carro,
    onibus,
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

nomes_variaveis <- c(
  "distance" = "Distância das Médias Absolutas",
  "IDADE" = "Idade Média",
  "SEXO_Feminino" = "Sexo: Feminino",
  "SEXO_Masculino" = "Sexo: Masculino",
  "GRAU_INS_Não alfabetizado/ Fund 1 incompleto" = "Escolaridade: Não alfabetizado/Fund 1 inc.",
  "GRAU_INS_Fund 1 completo/ Fund 2 incompleto" = "Escolaridade: Fund 1 comp./Fund 2 inc.",
  "GRAU_INS_Fund 2 completo/ Médio incompleto" = "Escolaridade: Fund 2 comp./Médio inc.",
  "GRAU_INS_Médio completo/ Superior incompleto" = "Escolaridade: Médio comp./Superior inc.",
  "GRAU_INS_Superior completo" = "Escolaridade: Superior completo",
  "CD_ATIVI_Trabalho Regular" = "Atividade: Trabalho Regular",
  "CD_ATIVI_Bico" = "Atividade: Bico",
  "CD_ATIVI_Desempregado" = "Atividade: Desempregado",
  "CD_ATIVI_Aposentado/Pensionista" = "Atividade: Aposentado/Pensionista",
  "CD_ATIVI_Estudante" = "Atividade: Estudante",
  "CD_ATIVI_Dona de Casa" = "Atividade: Dona de Casa",
  "CD_ATIVI_Licença Médica" = "Atividade: Licença Médica",
  "CD_ATIVI_Nunca Trabalhou" = "Atividade: Nunca Trabalhou",
  "ln_renda_f" = "ln da Renda Familiar per capita (R$)",
  "VIAGEM_TRANS_PUBL" = "Uso de Transporte Público"
)

ordem_vars <- c(
  "Distância das Médias Absolutas",
  "Idade Média",
  "ln da Renda Familiar per capita (R$)",
  "Uso de Transporte Público",

  "Sexo: Masculino",
  "Sexo: Feminino",

  "Escolaridade: Não alfabetizado/Fund 1 inc.",
  "Escolaridade: Fund 1 comp./Fund 2 inc.",
  "Escolaridade: Fund 2 comp./Médio inc.",
  "Escolaridade: Médio comp./Superior inc.",
  "Escolaridade: Superior completo",

  "Atividade: Trabalho Regular",
  "Atividade: Estudante",
  "Atividade: Desempregado",
  "Atividade: Bico",
  "Atividade: Licença Médica",
  "Atividade: Aposentado/Pensionista",
  "Atividade: Nunca Trabalhou",
  "Atividade: Dona de Casa"
)

plot_comparativo <- love.plot(
  match,
  binary = 'std',
  thresholds = c(m = .1),
  abs = TRUE,
  var.order = 'unadjusted',
  shapes = c('circle', 'triangle'),
  colors = c('red', 'blue'),
  title = 'Balanceamento das Variáveis Pareadas',
  sample.names = c('Antes do Pareamento', 'Depois do Pareamento'),
  var.names = nomes_variaveis
) +
  scale_y_discrete(limits = rev(ordem_vars)) +
  labs(
    x = "Diferenças Médias Padronizadas Absolutas",
    color = "Tipos de Amostra",
    shape = "Tipos de Amostra"
  )


print(plot_comparativo)

##### Analisando estatísticas descritivas dos grupos de controle criados, tanto os paramétricos quanto o por pareamento #####

#### Criando função de simplificação das variáveis para as tabelas descritivas ####

simplificar_categorias <- function(df) {
  df |>
    mutate(
      # Recodificando Escolaridade
      GRAU_INS = case_when(
        GRAU_INS %in%
          c(
            "Não alfabetizado/ Fund 1 incompleto",
            "Fund 1 completo/ Fund 2 incompleto",
            "Fund 2 completo/ Médio incompleto",
            1,
            2,
            3
          ) ~ "Médio incompleto",
        GRAU_INS %in%
          c("Médio completo/ Superior incompleto", 4) ~ "Médio completo",
        GRAU_INS %in% c("Superior completo", 5) ~ "Superior completo",
        TRUE ~ as.character(GRAU_INS)
      ),
      # Recodificando Atividade
      CD_ATIVI = case_when(
        CD_ATIVI %in% c("Trabalho Regular", "Bico", 1, 2) ~ "Ocupado",
        CD_ATIVI %in%
          c(
            "Licença Médica",
            "Aposentado/Pensionista",
            "Desempregado",
            "Nunca Trabalhou",
            "Dona de Casa",
            3,
            4,
            5,
            6,
            7
          ) ~ "Sem Ocupação",
        CD_ATIVI %in% c("Estudante", 8) ~ "Estudante",
        TRUE ~ as.character(CD_ATIVI)
      ),
      # Recodificando Transporte
      TIPVG = case_when(
        TIPVG %in% c("Coletivo", 1) ~ "Coletivo",
        TIPVG %in% c("Particular", 2) ~ "Particular",
        TIPVG %in% c("A pé", "Bicicleta", 3, 4) ~ "Ativo",
        TRUE ~ as.character(TIPVG)
      )
    ) |>
    # Transformando de volta em Fator com as ordenações corretas
    mutate(
      GRAU_INS = factor(
        GRAU_INS,
        levels = c("Médio incompleto", "Médio completo", "Superior completo")
      ),
      CD_ATIVI = factor(
        CD_ATIVI,
        levels = c("Ocupado", "Sem Ocupação", "Estudante")
      ),
      TIPVG = factor(TIPVG, levels = c("Coletivo", "Particular", "Ativo"))
    )
}

#### Grupo de controle 1 - Linhas Futuras ####

od_estat_linhas <- od_completa |>
  filter(tipo_grupo == 'Controle_Linhas_Futuras') |>
  select(IDADE, SEXO, GRAU_INS, CD_ATIVI, ln_renda_f, TIPVG) |>
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

od_estat_linhas <- od_estat_linhas |>
  simplificar_categorias()

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
  filter(!is.na(ln_renda_f)) |>
  summarise(
    Valor = mean(ln_renda_f, na.rm = TRUE),
    Variavel = 'ln da Renda Familiar per capita (R$)'
  ) |>
  ungroup() |>
  mutate(Valor = round(Valor, 2)) |>
  select(Variavel, Valor)

n_obs_linhas <- data.frame(
  Valor = nrow(od_estat_linhas),
  Variavel = 'Número de Observações'
)

tabela_descrit_linhas <- bind_rows(
  n_obs_linhas,
  idade_anual_linhas,
  renda_anual_linhas,
  categorias_anual_linhas
) |>
  arrange(factor(
    Variavel,
    levels = c(
      'Idade Média (Anos)',
      'ln da Renda Familiar per capita (R$)',
      'Masculino',
      'Feminino',

      'Médio incompleto',
      'Médio completo',
      'Superior completo',

      'Ocupado',
      'Sem Ocupação',
      'Estudante',

      'Coletivo',
      'Particular',
      'Ativo',

      'Número de Observações'
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
  select(IDADE, SEXO, GRAU_INS, CD_ATIVI, ln_renda_f, TIPVG) |>
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

od_estat_cptm <- od_estat_cptm |>
  simplificar_categorias()

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
  filter(!is.na(ln_renda_f)) |>
  summarise(
    Valor = mean(ln_renda_f, na.rm = TRUE),
    Variavel = 'ln da Renda Familiar per capita (R$)'
  ) |>
  ungroup() |>
  mutate(Valor = round(Valor, 2)) |>
  select(Variavel, Valor)

n_obs_cptm <- data.frame(
  Valor = nrow(od_estat_cptm),
  Variavel = 'Número de Observações'
)

tabela_descrit_cptm <- bind_rows(
  n_obs_cptm,
  idade_anual_cptm,
  renda_anual_cptm,
  categorias_anual_cptm
) |>
  arrange(factor(
    Variavel,
    levels = c(
      'Idade Média (Anos)',
      'ln da Renda Familiar per capita (R$)',
      'Masculino',
      'Feminino',

      'Médio incompleto',
      'Médio completo',
      'Superior completo',

      'Ocupado',
      'Sem Ocupação',
      'Estudante',

      'Coletivo',
      'Particular',
      'Ativo',

      'Número de Observações'
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
  select(IDADE, SEXO, GRAU_INS, CD_ATIVI, ln_renda_f, TIPVG) |>
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

od_pareada <- od_pareada |>
  simplificar_categorias()

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
  filter(!is.na(ln_renda_f)) |>
  summarise(
    Valor = mean(ln_renda_f, na.rm = TRUE),
    Variavel = 'ln da Renda Familiar per capita (R$)'
  ) |>
  ungroup() |>
  mutate(Valor = round(Valor, 2)) |>
  select(Variavel, Valor)

n_obs_match <- data.frame(
  Valor = nrow(od_pareada),
  Variavel = 'Número de Observações'
)

tabela_descrit_match <- bind_rows(
  n_obs_match,
  idade_anual_match,
  renda_anual_match,
  categorias_anual_match
) |>
  arrange(factor(
    Variavel,
    levels = c(
      'Idade Média (Anos)',
      'ln da Renda Familiar per capita (R$)',
      'Masculino',
      'Feminino',

      'Médio incompleto',
      'Médio completo',
      'Superior completo',

      'Ocupado',
      'Sem Ocupação',
      'Estudante',

      'Coletivo',
      'Particular',
      'Ativo',

      'Número de Observações'
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
  select(IDADE, SEXO, GRAU_INS, CD_ATIVI, ln_renda_f, TIPVG) |>
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

od_tratado <- od_tratado |>
  simplificar_categorias()

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
  filter(!is.na(ln_renda_f)) |>
  summarise(
    Valor = mean(ln_renda_f, na.rm = TRUE),
    Variavel = 'ln da Renda Familiar per capita (R$)'
  ) |>
  ungroup() |>
  mutate(Valor = round(Valor, 2)) |>
  select(Variavel, Valor)

n_obs_tratado <- data.frame(
  Valor = nrow(od_tratado),
  Variavel = 'Número de Observações'
)

tabela_descrit_tratado <- bind_rows(
  n_obs_tratado,
  idade_anual_tratado,
  renda_anual_tratado,
  categorias_anual_tratado
) |>
  arrange(factor(
    Variavel,
    levels = c(
      'Idade Média (Anos)',
      'ln da Renda Familiar per capita (R$)',
      'Masculino',
      'Feminino',

      'Médio incompleto',
      'Médio completo',
      'Superior completo',

      'Ocupado',
      'Sem Ocupação',
      'Estudante',

      'Coletivo',
      'Particular',
      'Ativo',

      'Número de Observações'
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

##### Fazendo as diferenças padronizadas entre cada grupo de controle e o tratamento #####

od_estat_tratado <- od_tratado |>
  mutate(
    SEXO = as.character(SEXO),
    GRAU_INS = as.character(GRAU_INS),
    CD_ATIVI = as.character(CD_ATIVI),
    TIPVG = as.character(TIPVG)
  ) |>
  select(IDADE, SEXO, GRAU_INS, CD_ATIVI, ln_renda_f, TIPVG)

vars_smd <- c("IDADE", "SEXO", "GRAU_INS", "CD_ATIVI", "ln_renda_f", "TIPVG")

### Tratado x Pareado ###

od_estat_pareado <- od_pareada |>
  mutate(across(c(SEXO, GRAU_INS, CD_ATIVI, TIPVG), as.character)) |>
  select(IDADE, SEXO, GRAU_INS, CD_ATIVI, ln_renda_f, TIPVG)

comp_pareado <- bind_rows(
  od_estat_tratado |> mutate(trat = 1),
  od_estat_pareado |> mutate(trat = 0)
)

smd_calc_pareado <- bal.tab(
  comp_pareado[, vars_smd],
  treat = comp_pareado$trat,
  binary = "std",
  continuous = "std",
  s.d.denom = "pooled"
)

### Tratado x CPTM ###

comp_cptm <- bind_rows(
  od_estat_tratado |> mutate(trat = 1),
  od_estat_cptm |> mutate(trat = 0)
)

smd_calc_cptm <- bal.tab(
  comp_cptm[, vars_smd],
  treat = comp_cptm$trat,
  binary = "std",
  continuous = "std",
  s.d.denom = "pooled"
)

### Tratado x Linhas Futuras ###

comp_linhas <- bind_rows(
  od_estat_tratado |> mutate(trat = 1),
  od_estat_linhas |> mutate(trat = 0)
)

smd_calc_linhas <- bal.tab(
  comp_linhas[, vars_smd],
  treat = comp_linhas$trat,
  binary = "std",
  continuous = "std",
  s.d.denom = "pooled"
)

#### Extraindo os dados ####

extrair_smd <- function(bal_obj, nome_coluna) {
  bal_obj$Balance |>
    rownames_to_column("Variavel") |>
    select(Variavel, Diff.Un) |>
    rename(!!nome_coluna := Diff.Un) |>
    mutate(across(where(is.numeric), ~ round(., 3)))
}

resumo_smd <- extrair_smd(smd_calc_pareado, "SMD_Pareado") |>
  left_join(extrair_smd(smd_calc_cptm, "SMD_CPTM"), by = "Variavel") |>
  left_join(extrair_smd(smd_calc_linhas, "SMD_Linhas"), by = "Variavel")

print(resumo_smd)
