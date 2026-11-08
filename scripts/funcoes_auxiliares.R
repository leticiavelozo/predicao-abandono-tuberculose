# ==============================================================================
# PROJETO: Predição do Abandono do Tratamento da Tuberculose em Minas Gerais
# Arquivo: scripts/funcoes_auxiliares.R
# Objetivo: funções compartilhadas pelos scripts do pipeline
# ==============================================================================

# Localidade e nomes de colunas dummy ----
# O LightGBM pode alterar temporariamente a localidade da sessão R para "C" no Windows,
# o que afeta o tratamento de acentos e nomes de colunas no step_dummy()
localidade_inicial <- Sys.getlocale("LC_CTYPE")

restaurar_localidade <- function() {
  invisible(Sys.setlocale("LC_CTYPE", localidade_inicial))
}

nomes_dummy <- function(var, lvl, ordinal = FALSE, sep = "_") {
  paste(var, lvl, sep = sep)
}

# Identidade visual das figuras ----
cor_principal <- "#2a78d6"
cor_neutra    <- "#8c8c8c"

nomes_modelos <- c(
  lr   = "Regressão Logística",
  rf   = "Random Forest",
  lgbm = "LightGBM"
)

cores_modelos <- c(
  "LightGBM"            = "#2a78d6",
  "Random Forest"       = "#eb6834",
  "Regressão Logística" = "#1baf7a"
)

linhas_modelos <- c(
  "LightGBM"            = "solid",
  "Random Forest"       = "longdash",
  "Regressão Logística" = "dotted"
)

# Rótulos numéricos com vírgula decimal e ponto de milhar
rotulo_numero <- function(accuracy = NULL, ...) {
  scales::label_number(accuracy = accuracy, decimal.mark = ",", big.mark = ".", ...)
}

formatar_numero <- function(x, accuracy = NULL, ...) {
  scales::number(x, accuracy = accuracy, decimal.mark = ",", big.mark = ".", ...)
}

tema <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#e6e6e6", linewidth = 0.3),
      axis.text = ggplot2::element_text(color = "#333333"),
      axis.title = ggplot2::element_text(color = "#111111"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = ggplot2::rel(1)),
      plot.margin = ggplot2::margin(4, 6, 4, 4)
    )
}

salvar_figura <- function(grafico, nome_arquivo, altura_cm, largura_cm = 16) {
  ggplot2::ggsave(
    filename = here::here("resultados", "figuras", nome_arquivo),
    plot = grafico,
    width = largura_cm,
    height = altura_cm,
    units = "cm",
    dpi = 300,
    bg = "white"
  )
}

# Cálculo Padronizado de métricas por fold
calcular_metricas_fold <- function(predicoes) {
  desfecho <- predicoes$desfecho_abandono
  prob     <- predicoes$.pred_Abandono
  classe   <- predicoes$.pred_class
  
  tibble::tibble(
    roc_auc  = yardstick::roc_auc_vec(desfecho, prob, event_level = "second"),
    pr_auc   = yardstick::pr_auc_vec(desfecho, prob, event_level = "second"),
    sens     = yardstick::sens_vec(desfecho, classe, event_level = "second"),
    spec     = yardstick::spec_vec(desfecho, classe, event_level = "second"),
    f_meas   = yardstick::f_meas_vec(desfecho, classe, event_level = "second"),
    accuracy = yardstick::accuracy_vec(desfecho, classe),
    brier    = yardstick::brier_class_vec(desfecho, prob, event_level = "second")
  )
}

# Exportação de tabelas ----
exportar_tabelas <- function(lista_tabelas, nome_planilha, casas_decimais = 4, salvar_csv = TRUE) {
  pasta <- here::here("resultados", "tabelas")
  
  # O Excel limita o nome de cada aba a 31 caracteres
  names(lista_tabelas) <- substr(names(lista_tabelas), 1, 31)
  
  tabelas_formatadas <- list()
  
  # Itera sobre cada tabela da lista de forma direta
  for (nome in names(lista_tabelas)) {
    tabela <- as.data.frame(lista_tabelas[[nome]])
    
    # Formata cada coluna sem funções aninhadas ou tidyeval complexo
    for (coluna in names(tabela)) {
      valores <- tabela[[coluna]]
      if (is.list(valores)) {
        tabela[[coluna]] <- sapply(valores, function(v) paste(v, collapse = ", "))
      } else if (is.numeric(valores)) {
        tabela[[coluna]] <- round(valores, casas_decimais)
      }
    }
    
    tabelas_formatadas[[nome]] <- tabela
    if (salvar_csv) {
      caminho_csv <- file.path(pasta, paste0(nome_planilha, "_", nome, ".csv"))
      readr::write_excel_csv2(tabela, caminho_csv, na = "")
    }
  }
  
  openxlsx::write.xlsx(
    tabelas_formatadas,
    file = file.path(pasta, paste0(nome_planilha, ".xlsx")),
    overwrite = TRUE,
    colWidths = "auto",
    firstRow = TRUE
  )
  
  invisible(tabelas_formatadas)
}
