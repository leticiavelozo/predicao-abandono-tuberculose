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

nota_descritiva <- paste(
  "Idade apresentada como mediana (1º quartil, 3º quartil).",
  "Os percentuais usam os registros com informação disponível para cada variável em cada coluna.",
  "Para as variáveis binárias, apresenta-se a frequência da resposta Sim."
)

#Tabela de Perfil sociodemográfico e clínico
tabela_descritiva <- base_modelagem |>
  tbl_summary(
    by = desfecho_abandono,
    missing = "ifany",
    missing_text = "Dados ausentes",
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    percent = "column",
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
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Desfecho do Tratamento**") |>
  modify_footnote_header(footnote = nota_descritiva, columns = all_stat_cols())

print(tabela_descritiva)

# Exportação para Excel ----
exportar_tabelas(
  lista_tabelas = list(
    tabela_descritiva = as_tibble(tabela_descritiva, col_labels = TRUE) |>
      rename_with(~ str_squish(str_remove_all(.x, stringr::fixed("**")))),
    notas = tibble(Nota = nota_descritiva)
  ),
  nome_planilha = "03B_descritiva",
  salvar_csv = FALSE
)