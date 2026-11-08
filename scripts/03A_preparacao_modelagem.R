# ==============================================================================
# PROJETO: Predição do Abandono do Tratamento da Tuberculose em Minas Gerais
# Arquivo: scripts/03A_preparacao_modelagem.R
# Objetivo: Filtrar a amostra, harmonizar os preditores e dividir treino/teste
# ==============================================================================

library(tidyverse)
library(here)
library(rsample)

source(here("scripts", "funcoes_auxiliares.R"))

#Leitura da base ----
tb_dados <- readRDS(here("dados", "intermediarios", "tb_dados_com_encerramento.rds"))

# Filtragem dos casos elegíveis ----
dados_elegiveis <- tb_dados |>
  filter(
    nu_idade_n >= 18,
    forma == "Pulmonar",
    is.na(pop_liber) | pop_liber != "Sim",
    situa_ence %in% c("Cura", "Abandono"),
    tratamento %in% c("Caso Novo", "Recidiva", "Reingresso após Abandono"),
    is.na(test_sensi) | !(test_sensi %in% c(
      "Resistente a outras drogas de 1ª linha",
      "Resistente somente à Isoniazida",
      "Resistente somente à Rifampicina",
      "Resistente à Isoniazida e Rifampicina"
    )),
    !(cs_gestant %in% c(
      "1° Trimestre",
      "2° Trimestre",
      "3° Trimestre",
      "Idade gestacional ignorada"
    ))
  ) |>
  mutate(
    desfecho_abandono = factor(situa_ence, levels = c("Cura", "Abandono")),
    tb_previa = if_else(tratamento == "Caso Novo", "Não", "Sim")
  )

cat("Total antes dos filtros:", nrow(tb_dados), "\n")
cat("Total após os filtros (amostra final):", nrow(dados_elegiveis), "\n")

padroes_ausentes <- c("Ignorado", "Não se aplica", "Não sabe", "9", "")

base_modelagem <- dados_elegiveis |>
  select(
    desfecho_abandono,
    nu_idade_n,
    cs_sexo,
    cs_raca,
    cs_escol_n,
    cs_zona,
    agravalcoo,
    agravtabac,
    agravdroga,
    agravaids,
    agravdiabe,
    agravdoenc,
    tb_previa
  ) |>
  mutate(
    across(
      where(is.character),
      ~ if_else(.x %in% padroes_ausentes, NA_character_, .x)
    )
  )

colSums(is.na(base_modelagem))
glimpse(base_modelagem)

# Partição 70/30 estratificada pelo desfecho para preservar a taxa basal nos dois conjuntos
set.seed(42)

split_dados <- initial_split(
  base_modelagem,
  prop = 0.70,
  strata = desfecho_abandono
)

tb_treino <- training(split_dados)
tb_teste  <- testing(split_dados)

stopifnot(identical(levels(base_modelagem$desfecho_abandono), c("Cura", "Abandono")))

# Salvamento das bases ----
saveRDS(base_modelagem, here("dados", "processados", "tb_base_modelagem.rds"))
saveRDS(tb_treino,    here("dados", "processados", "tb_treino.rds"))
saveRDS(tb_teste,     here("dados", "processados", "tb_teste.rds"))

#Tabela de verificação da partição Treino/Teste
resumo_particao <- bind_rows(
  tb_treino |> count(desfecho_abandono) |> mutate(conjunto = "Treino"),
  tb_teste  |> count(desfecho_abandono) |> mutate(conjunto = "Teste")
) |>
  group_by(conjunto) |>
  mutate(percentual = round(100 * n / sum(n), 2)) |>
  ungroup() |>
  relocate(conjunto)

print(resumo_particao)

exportar_tabelas(
  list(particao_treino_teste = resumo_particao),
  nome_planilha = "03A_amostra",
  salvar_csv = FALSE
)