# ==============================================================================
# PROJETO: Predição do Abandono do Tratamento da Tuberculose em Minas Gerais
# Arquivo: scripts/04_selecao_atributos_rfecv.R
# Objetivo: Seleção recursiva de atributos com validação cruzada (RFECV)
# ==============================================================================

library(tidyverse)
library(tidymodels)
library(themis)
library(here)

source(here("scripts", "funcoes_auxiliares.R"))

# Carregamento da base de treinamento ----
tb_treino <- readRDS(here("dados", "processados", "tb_treino.rds"))

# Validação cruzada estratificada (10 folds) ----
set.seed(2024)

cv_folds <- vfold_cv(
  data = tb_treino,
  v = 10,
  strata = desfecho_abandono
)

# Especificações do Random Forest ----
modelo_rf <- rand_forest(trees = 500, min_n = 2) |>
  set_engine("ranger", seed = 2024) |>
  set_mode("classification")

modelo_rf_importancia <- rand_forest(trees = 500, min_n = 2) |>
  set_engine("ranger", importance = "impurity_corrected", seed = 2024) |>
  set_mode("classification")

# Função auxiliar para criar a receita ----
criar_receita <- function(variaveis, com_downsample = TRUE) {
  formula_modelo <- as.formula(
    paste("desfecho_abandono ~", paste(variaveis, collapse = " + "))
  )
  
  rec <- recipe(formula_modelo, data = tb_treino) |>
    step_string2factor(all_nominal_predictors()) |>
    step_unknown(all_nominal_predictors(), new_level = "Missing")
  
  if (com_downsample) {
    rec <- rec |> 
      step_downsample(desfecho_abandono, under_ratio = 1, seed = 2024)
  }
  rec
}

# Configuração inicial do RFECV ----
variaveis_atuais <- setdiff(names(tb_treino), "desfecho_abandono")
minimo_variaveis <- 4

resultados_rfecv <- tibble()
historico_rfecv  <- tibble()

metricas_rfecv <- metric_set(roc_auc, pr_auc, sens, spec, f_meas)

# Loop recursivo do RFECV ----
while (length(variaveis_atuais) >= minimo_variaveis) {
  set.seed(2024)
  
  qtd_atual <- length(variaveis_atuais)
  cat("Rodada com", qtd_atual, "variáveis:\n", paste(variaveis_atuais, collapse = ", "), "\n")
  
  receita_atual <- criar_receita(variaveis_atuais, com_downsample = TRUE)
  
  # 1. Validação cruzada de 10 folds
  fluxo_validacao <- workflow() |>
    add_recipe(receita_atual) |>
    add_model(modelo_rf)
  
  resultado_validacao <- fit_resamples(
    fluxo_validacao,
    resamples = cv_folds,
    metrics = metricas_rfecv,
    control = control_resamples(event_level = "second")
  )
  
  metricas_rodada <- collect_metrics(resultado_validacao) |>
    transmute(
      n_variaveis = qtd_atual,
      metrica     = .metric,
      media       = mean,
      erro_padrao = std_err
    )
  
  resultados_rfecv <- bind_rows(resultados_rfecv, metricas_rodada)
  
  # 2. Cálculo de importância para achar a pior variável
  fluxo_importancia <- workflow() |>
    add_recipe(receita_atual) |>
    add_model(modelo_rf_importancia)
  
  pesos_importancia <- fit(fluxo_importancia, data = tb_treino) |>
    extract_fit_engine() |>
    ranger::importance()
  
  pior_variavel <- names(sort(pesos_importancia))[1]
  
  historico_rfecv <- bind_rows(
    historico_rfecv,
    tibble(
      n_variaveis       = qtd_atual,
      variaveis         = paste(variaveis_atuais, collapse = ", "),
      variavel_removida = pior_variavel
    )
  )
  
  cat("-> Removendo variável menos importante:", pior_variavel, "\n\n")
  
  # Remove a pior variável da lista para a próxima rodada
  variaveis_atuais <- setdiff(variaveis_atuais, pior_variavel)
}

# Tabela consolidada do RFECV ----
tabela_rfecv <- resultados_rfecv |>
  select(n_variaveis, metrica, media, erro_padrao) |>
  filter(metrica %in% c("roc_auc", "pr_auc", "sens", "spec")) |>
  pivot_wider(
    names_from = metrica,
    values_from = c(media, erro_padrao)
  ) |>
  arrange(desc(n_variaveis))

print(tabela_rfecv, n = Inf)

# Seleção do melhor conjunto de variáveis (maior ROC-AUC médio) ----
desempenho_auc <- resultados_rfecv |>
  filter(metrica == "roc_auc")

melhor_ponto <- desempenho_auc |>
  arrange(desc(media), n_variaveis) |>
  slice(1)

melhor_qtd <- melhor_ponto$n_variaveis

variaveis_finais <- historico_rfecv |>
  filter(n_variaveis == melhor_qtd) |>
  pull(variaveis) |>
  str_split_1(",\\s*")

cat("Quantidade ótima selecionada:", melhor_qtd, "variáveis\n")
cat("Variáveis finais:", paste(variaveis_finais, collapse = ", "), "\n")

# Receitas finais (com e sem balanceamento para o Script 05) ----
receita_com_balanceamento <- criar_receita(variaveis_finais, com_downsample = TRUE)
receita_sem_balanceamento <- criar_receita(variaveis_finais, com_downsample = FALSE)

# Gráfico da curva do RFECV ----
grafico_rfecv <- ggplot(desempenho_auc, aes(x = n_variaveis, y = media)) +
  geom_errorbar(
    aes(ymin = media - erro_padrao, ymax = media + erro_padrao),
    width = 0.15,
    linewidth = 0.4,
    color = cor_neutra
  ) +
  geom_line(color = cor_principal, linewidth = 0.6) +
  geom_point(color = cor_principal, size = 1.8) +
  geom_point(data = melhor_ponto, color = "#eb6834", size = 3) +
  geom_text(
    data = melhor_ponto,
    aes(label = paste0(n_variaveis, " variáveis\n(selecionado)")),
    nudge_x = 0.6,
    hjust = 0,
    size = 3,
    lineheight = 0.9,
    color = "#333333"
  ) +
  scale_x_continuous(breaks = unique(desempenho_auc$n_variaveis)) +
  scale_y_continuous(labels = rotulo_numero(0.001)) +
  labs(
    x = "Número de variáveis",
    y = "ROC-AUC médio na validação cruzada"
  ) +
  tema() +
  theme(panel.grid.major.x = element_blank())

salvar_figura(grafico_rfecv, "04_curva_rfecv.png", altura_cm = 8)

# Salvar artefatos ----
saveRDS(resultados_rfecv, here("dados", "intermediarios", "resultados_rfecv.rds"))
saveRDS(historico_rfecv,  here("dados", "intermediarios", "historico_rfecv.rds"))
saveRDS(cv_folds,         here("dados", "processados", "cv_folds.rds"))

saveRDS(receita_com_balanceamento, here("resultados", "modelos", "receita_com_balanceamento.rds"))
saveRDS(receita_sem_balanceamento, here("resultados", "modelos", "receita_sem_balanceamento.rds"))
saveRDS(variaveis_finais,          here("resultados", "modelos", "vars_selecionadas.rds"))

exportar_tabelas(
  list(
    rfecv_metricas         = tabela_rfecv,
    rfecv_eliminacao       = historico_rfecv,
    variaveis_selecionadas = tibble(variavel = variaveis_finais)
  ),
  nome_planilha = "04_rfecv",
  salvar_csv = FALSE
)

readRDS(here("dados", "intermediarios", "historico_rfecv.rds")) |>
  select(n_variaveis, variavel_removida)

message("✔ Script 04 concluído com sucesso!")
