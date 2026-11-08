# ==============================================================================
# PROJETO: Predicao do Abandono do Tratamento da Tuberculose em Minas Gerais
#
# Arquivo: scripts/01_leitura_dados.R
#
# Objetivo:
# - Ler a base bruta de tuberculose
# - Auditar a consistencia de encerramento dos registros
# - Salvar a base intermediaria para as proximas etapas
# ==============================================================================

library(tidyverse)
library(here)

# Leitura dos dados ----

tb_bruta <- read_csv2(
  file = here("dados","brutos","dados_tuberculose.csv"),
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

# Carrega as funções auxiliares 
source(here("scripts", "funcoes_auxiliares.R"))

glimpse(tb_bruta)

# Resumo de qualidade por ano ----
resumo_encerramento_por_ano <- tb_bruta |>
  group_by(nu_ano) |>
  summarise(
    total_casos = n(),
    sem_encerramento = sum(is.na(situa_ence)),
    pct_sem_encerramento = round(100 * sem_encerramento / total_casos, 2),
    .groups = "drop"
  ) |>
  arrange(nu_ano)

# Bases intermediarias ----
# Base destinada a apenas registro com situação de encerramento preenchida
tb_dados_com_encerramento <- tb_bruta |>
  filter(!is.na(situa_ence),
         nu_ano <= 2024)

saveRDS(tb_dados_com_encerramento,file = here("dados","intermediarios","tb_dados_com_encerramento.rds"))

# Salvamento da auditoria em Excel ----
exportar_tabelas(
  lista_tabelas = list("Encerramento por Ano" = resumo_encerramento_por_ano),
  nome_planilha = "01_auditoria_encerramento",
  salvar_csv = FALSE
)