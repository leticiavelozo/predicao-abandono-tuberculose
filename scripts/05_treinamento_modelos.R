# ==============================================================================
# PROJETO: Predição do Abandono do Tratamento da Tuberculose em Minas Gerais
# Arquivo: scripts/05_treinamento_modelos.R
# Objetivo:
# - Treinar Regressão Logística, Random Forest e LightGBM
# - Avaliar os modelos por validação cruzada estratificada de 10 folds
# - Reproduzir os hiperparâmetros reportados por Rodrigues et al. (2024)
# ==============================================================================

library(tidyverse)
library(tidymodels)
library(themis)
library(bonsai)
library(here)

source(here("scripts", "funcoes_auxiliares.R"))

# Carregamento dos dados e artefatos ----
tb_treino                 <- readRDS(here("dados", "processados", "tb_treino.rds"))
cv_folds                  <- readRDS(here("dados", "processados", "cv_folds.rds"))
receita_com_balanceamento <- readRDS(here("resultados", "modelos", "receita_com_balanceamento.rds"))
receita_sem_balanceamento <- readRDS(here("resultados", "modelos", "receita_sem_balanceamento.rds"))

# Função para calcular métricas por fold ----
metricas_validacao <- metric_set(
  roc_auc, pr_auc, sens, spec, f_meas, accuracy, brier_class
)

controle_cv <- control_resamples(save_pred = TRUE, event_level = "second")

avaliar_modelo <- function(resultado_cv, nome_modelo) {
  collect_metrics(resultado_cv) |>
    transmute(
      modelo      = nome_modelo,
      metrica     = if_else(.metric == "brier_class", "brier", .metric),
      media       = mean,
      erro_padrao = std_err
    )
}

# Especificação dos 3 algoritmos (hiperparâmetros de Rodrigues et al., 2024) ----
modelo_lr <- logistic_reg(penalty = tune(), mixture = 1) |>
  set_engine("glmnet") |>
  set_mode("classification")

grade_penalidade <- grid_regular(penalty(range = c(-5, 0)), levels = 10)

modelo_rf <- rand_forest(trees = 500, min_n = 2) |>
  set_engine("ranger", max.depth = 8, seed = 2024) |>
  set_mode("classification")

modelo_lgbm <- boost_tree(trees = 500, tree_depth = 4, learn_rate = 0.01) |>
  set_engine(
    "lightgbm",
    seed = 2024,
    deterministic = TRUE,
    force_row_wise = TRUE,
    num_threads = 1
  ) |>
  set_mode("classification")

# CENÁRIO PRINCIPAL — MODELOS SEM BALANCEAMENTO ----

receita_lr_principal <- receita_sem_balanceamento |>
  step_dummy(all_nominal_predictors(), one_hot = FALSE, naming = nomes_dummy)

fluxo_lr <- workflow() |>
  add_recipe(receita_lr_principal) |>
  add_model(modelo_lr)

set.seed(2024)
ajuste_penalidade_lr <- tune_grid(
  fluxo_lr,
  resamples = cv_folds,
  grid = grade_penalidade,
  metrics = metric_set(roc_auc),
  control = control_grid(event_level = "second")
)

roc_auc_penalidade_principal <- collect_metrics(ajuste_penalidade_lr) |>
  select(penalty, roc_auc_medio = mean, erro_padrao = std_err)

melhor_penalidade_principal <- roc_auc_penalidade_principal |>
  arrange(desc(roc_auc_medio), penalty) |>
  slice(1) |>
  select(penalty)

fluxo_lr <- finalize_workflow(fluxo_lr, melhor_penalidade_principal)

fluxo_rf <- workflow() |>
  add_recipe(receita_sem_balanceamento) |>
  add_model(modelo_rf)

fluxo_lgbm <- workflow() |>
  add_recipe(receita_sem_balanceamento) |>
  add_model(modelo_lgbm)

cat("Avaliando modelos principais (sem balanceamento) na validação cruzada...\n")
res_cv_lr   <- fit_resamples(fluxo_lr,   resamples = cv_folds, metrics = metricas_validacao, control = controle_cv)
res_cv_rf   <- fit_resamples(fluxo_rf,   resamples = cv_folds, metrics = metricas_validacao, control = controle_cv)
res_cv_lgbm <- fit_resamples(fluxo_lgbm, resamples = cv_folds, metrics = metricas_validacao, control = controle_cv)
restaurar_localidade()

tabela_cv_principal <- bind_rows(
  avaliar_modelo(res_cv_lr,   "Regressão Logística"),
  avaliar_modelo(res_cv_rf,   "Random Forest"),
  avaliar_modelo(res_cv_lgbm, "LightGBM")
) |>
  pivot_wider(names_from = metrica, values_from = c(media, erro_padrao)) |>
  arrange(desc(media_roc_auc))

print(tabela_cv_principal, n = Inf)

cat("Ajustando modelos principais no treino completo...\n")
modelos_principais <- list(
  lr   = fit(fluxo_lr,   data = tb_treino),
  rf   = fit(fluxo_rf,   data = tb_treino),
  lgbm = fit(fluxo_lgbm, data = tb_treino)
)
restaurar_localidade()

# COMPARAÇÃO COM O ARTIGO — MODELOS COM DOWNSAMPLING
receita_lr_artigo <- receita_com_balanceamento |>
  step_dummy(all_nominal_predictors(), one_hot = FALSE, naming = nomes_dummy)

fluxo_lr_artigo <- workflow() |>
  add_recipe(receita_lr_artigo) |>
  add_model(modelo_lr)

set.seed(2024)
ajuste_penalidade_artigo <- tune_grid(
  fluxo_lr_artigo,
  resamples = cv_folds,
  grid = grade_penalidade,
  metrics = metric_set(roc_auc),
  control = control_grid(event_level = "second")
)

melhor_penalidade_artigo <- collect_metrics(ajuste_penalidade_artigo) |>
  arrange(desc(mean), penalty) |>
  slice(1) |>
  select(penalty)

fluxo_lr_artigo <- finalize_workflow(fluxo_lr_artigo, melhor_penalidade_artigo)

fluxo_rf_artigo <- workflow() |>
  add_recipe(receita_com_balanceamento) |>
  add_model(modelo_rf)

fluxo_lgbm_artigo <- workflow() |>
  add_recipe(receita_com_balanceamento) |>
  add_model(modelo_lgbm)

cat("Avaliando modelos com downsampling (condição do artigo) na validação cruzada...\n")
res_cv_lr_artigo   <- fit_resamples(fluxo_lr_artigo,   resamples = cv_folds, metrics = metricas_validacao, control = controle_cv)
res_cv_rf_artigo   <- fit_resamples(fluxo_rf_artigo,   resamples = cv_folds, metrics = metricas_validacao, control = controle_cv)
res_cv_lgbm_artigo <- fit_resamples(fluxo_lgbm_artigo, resamples = cv_folds, metrics = metricas_validacao, control = controle_cv)
restaurar_localidade()

tabela_cv_artigo <- bind_rows(
  avaliar_modelo(res_cv_lr_artigo,   "Regressão Logística"),
  avaliar_modelo(res_cv_rf_artigo,   "Random Forest"),
  avaliar_modelo(res_cv_lgbm_artigo, "LightGBM")
) |>
  pivot_wider(names_from = metrica, values_from = c(media, erro_padrao)) |>
  arrange(desc(media_roc_auc))

print(tabela_cv_artigo, n = Inf)

cat("Ajustando modelos com downsampling no treino completo...\n")
modelos_com_downsampling <- list(
  lr   = fit(fluxo_lr_artigo,   data = tb_treino),
  rf   = fit(fluxo_rf_artigo,   data = tb_treino),
  lgbm = fit(fluxo_lgbm_artigo, data = tb_treino)
)
restaurar_localidade()

# LIMIAR ÓTIMO DE YOUDEN NA VALIDAÇÃO CRUZADA ----
extrair_predicoes <- function(resultado_cv, nome_modelo, nome_estrategia) {
  collect_predictions(resultado_cv) |>
    mutate(modelo = nome_modelo, estrategia = nome_estrategia)
}

predicoes_cv_todas <- bind_rows(
  extrair_predicoes(res_cv_lr,          "Regressão Logística", "Sem balanceamento"),
  extrair_predicoes(res_cv_rf,          "Random Forest",       "Sem balanceamento"),
  extrair_predicoes(res_cv_lgbm,        "LightGBM",            "Sem balanceamento"),
  extrair_predicoes(res_cv_lr_artigo,   "Regressão Logística", "Com downsampling"),
  extrair_predicoes(res_cv_rf_artigo,   "Random Forest",       "Com downsampling"),
  extrair_predicoes(res_cv_lgbm_artigo, "LightGBM",            "Com downsampling")
)

calcular_limiar_youden <- function(predicoes) {
  predicoes |>
    roc_curve(truth = desfecho_abandono, .pred_Abandono, event_level = "second") |>
    filter(is.finite(.threshold)) |>
    mutate(indice_youden = sensitivity + specificity - 1) |>
    slice_max(indice_youden, n = 1, with_ties = FALSE) |>
    transmute(
      limiar_youden     = .threshold,
      sensibilidade_cv  = sensitivity,
      especificidade_cv = specificity,
      indice_youden_cv  = indice_youden
    )
}

limiares_youden_cv <- predicoes_cv_todas |>
  group_by(modelo, estrategia) |>
  group_modify(function(dados, chave) calcular_limiar_youden(dados)) |>
  ungroup()

print(limiares_youden_cv)

# SALVAMENTO DOS MODELOS E TABELAS
saveRDS(predicoes_cv_todas,       here("dados", "intermediarios", "predicoes_cv_sensibilidade.rds"))
saveRDS(modelos_principais,       here("resultados", "modelos", "modelos_sem_balanceamento.rds"))
saveRDS(modelos_com_downsampling, here("resultados", "modelos", "modelos_finais_treinados.rds"))
saveRDS(limiares_youden_cv,       here("resultados", "modelos", "limiares_youden_cv.rds"))

exportar_tabelas(
  list(
    cv_principal_sem_bal      = tabela_cv_principal,
    cv_artigo_com_downsampling = tabela_cv_artigo,
    limiares_youden_cv        = limiares_youden_cv,
    penalidade_lr_principal   = melhor_penalidade_principal,
    penalidade_lr_artigo      = melhor_penalidade_artigo
  ),
  nome_planilha = "05_validacao_cruzada",
  salvar_csv = FALSE
)

message("✔ Script 05 concluído com sucesso!")