# ==============================================================================
# PROJETO: Predição do Abandono do Tratamento da Tuberculose em Minas Gerais
# Arquivo: scripts/03B_analise_descritiva.R
# Objetivo: Fazer uma análise descritiva dos dados (Cura vs Abandono)
# ==============================================================================

library(tidyverse)
library(gtsummary)
library(here)

source(here("scripts", "funcoes_auxiliares.R"))
theme_gtsummary_language(language = "pt")

# Leitura da base filtrada e com as 13 colunas selecionadas ----
base_modelagem <- readRDS(here("dados", "processados", "tb_base_modelagem.rds"))

#Tabela de Perfil sociodemográfico e clínico
tabela_descritiva <- base_modelagem |>
  tbl_summary(
    by = desfecho_abandono,
    missing = "ifany",
    missing_text = "Dados ausentes",
    # Mostra apenas a categoria "Sim" na linha principal das binárias
    type = list(
      c(agravalcoo, agravtabac, agravdroga, agravaids, agravdiabe, agravdoenc, tb_previa) ~ "dichotomous"
    ),
    value = list(
      agravalcoo ~ "Sim",
      agravtabac ~ "Sim",
      agravdroga ~ "Sim",
      agravaids  ~ "Sim",
      agravdiabe ~ "Sim",
      agravdoenc ~ "Sim",
      tb_previa  ~ "Sim"
    ),
    label = list(
      nu_idade_n ~ "Idade (anos)",
      cs_sexo    ~ "Sexo",
      cs_raca    ~ "Raça/Cor",
      cs_escol_n ~ "Escolaridade",
      cs_zona    ~ "Zona de residência",
      agravalcoo ~ "Uso de álcool",
      agravtabac ~ "Tabagismo",
      agravdroga ~ "Uso de drogas ilícitas",
      agravaids  ~ "Co-infecção por HIV/aids",
      agravdiabe ~ "Diabetes",
      agravdoenc ~ "Doença mental",
      tb_previa  ~ "Histórico de TB prévia"
    )
  ) |>
  add_overall(last = FALSE, col_label = "**Total**  \nN = {N}") |>
  add_p() |>
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Desfecho do Tratamento**")

# Exibe no Viewer do RStudio
print(tabela_descritiva)

# Exportação para Excel ----
exportar_tabelas(
  lista_tabelas = list(
    tabela_descritiva = as_tibble(tabela_descritiva, col_labels = TRUE)
  ),
  nome_planilha = "03B_descritiva",
  salvar_csv = FALSE
)