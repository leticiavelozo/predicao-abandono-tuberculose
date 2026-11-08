# ==============================================================================
# PROJETO: Predicao do Abandono do Tratamento da Tuberculose em Minas Gerais
# Arquivo: scripts/00_configuracao_projeto.R
# Objetivo: criar a estrutura de diretorios e definir configuracoes gerais
# ==============================================================================

library(here)

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE
)

pastas_projeto <- c(
  here("dados", "brutos"),
  here("dados", "intermediarios"),
  here("dados", "processados"),
  here("resultados", "figuras"),
  here("resultados", "tabelas"),
  here("resultados", "metricas"),
  here("resultados", "modelos"),
  here("scripts")
)

message("✔ Ambiente e diretórios configurados com sucesso.")
