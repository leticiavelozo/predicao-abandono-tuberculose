# ==============================================================================
# PROJETO: Predição do Abandono do Tratamento da Tuberculose em Minas Gerais
# Arquivo: scripts/06_avaliacao_teste.R
# Objetivo:
# - Avaliar o desempenho final no conjunto de teste (30% retido)
# - Comparar o Cenário Principal (Sem balanceamento + Youden) com a abordagem
#   de Rodrigues et al., 2024 (Com downsampling + limiar 0,50)
# - Gerar curvas ROC, Precision-Recall e matrizes de confusão
# ==============================================================================

library(tidyverse)
library(tidymodels)
library(here)
library(bonsai)

source(here("scripts", "funcoes_auxiliares.R"))

# Carregamento dos dados de teste e modelos finais ----
tb_teste                 <- readRDS(here("dados", "processados", "tb_teste.rds"))
modelos_sem_downsampling <- readRDS(here("resultados", "modelos", "modelos_sem_balanceamento.rds"))
modelos_com_downsampling <- readRDS(here("resultados", "modelos", "modelos_finais_treinados.rds"))
limiares_youden_cv       <- readRDS(here("resultados", "modelos", "limiares_youden_cv.rds"))

#Gerar predições no conjunto de teste ----
prever_no_teste <- function(lista_modelos, nome_estrategia) {
  imap(lista_modelos, function(modelo, sigla) {
    probs <- predict(modelo, new_data = tb_teste, type = "prob")
    
    tibble(
      modelo            = nomes_modelos[[sigla]],
      estrategia        = nome_estrategia,
      desfecho_abandono = tb_teste$desfecho_abandono,
      .pred_Cura        = probs$.pred_Cura,
      .pred_Abandono    = probs$.pred_Abandono
    )
  }) |>
    bind_rows()
}

predicoes_principais <- prever_no_teste(modelos_sem_downsampling,       "Sem balanceamento")
predicoes_artigo     <- prever_no_teste(modelos_com_downsampling, "Com downsampling")
restaurar_localidade()

predicoes_teste <- bind_rows(predicoes_principais, predicoes_artigo) |>
  left_join(
    limiares_youden_cv |> select(modelo, estrategia, limiar_youden),
    by = c("modelo", "estrategia")
  ) |>
  mutate(
    classe_050 = factor(
      if_else(.pred_Abandono >= 0.50, "Abandono", "Cura"),
      levels = c("Cura", "Abandono")
    ),
    classe_youden = factor(
      if_else(.pred_Abandono >= limiar_youden, "Abandono", "Cura"),
      levels = c("Cura", "Abandono")
    )
  )

metricas_prob   <- metric_set(roc_auc, pr_auc, brier_class)
metricas_classe <- metric_set(sens, spec, ppv, npv, f_meas, accuracy)

avaliar_no_teste <- function(df, coluna_classe, nome_regra) {
  # Métricas baseadas em probabilidade (ROC-AUC, PR-AUC, Brier)
  res_prob <- df |>
    group_by(modelo, estrategia, limiar_youden) |>
    metricas_prob(truth = desfecho_abandono, .pred_Abandono, event_level = "second")
  
  # Métricas baseadas na classe predita (Sens, Spec, VPP, VPN, F1, Acurácia)
  res_classe <- df |>
    group_by(modelo, estrategia, limiar_youden) |>
    metricas_classe(truth = desfecho_abandono, estimate = {{ coluna_classe }}, event_level = "second")
  
  bind_rows(res_prob, res_classe) |>
    select(modelo, estrategia, limiar_youden, .metric, .estimate) |>
    pivot_wider(names_from = .metric, values_from = .estimate) |>
    mutate(regra_decisao = nome_regra, .after = estrategia)
}

tabela_desempenho_teste <- bind_rows(
  avaliar_no_teste(predicoes_teste, classe_youden, "Limiar de Youden (CV)"),
  avaliar_no_teste(predicoes_teste, classe_050,    "Limiar padrão (0,50)")
) |>
  arrange(estrategia, regra_decisao, desc(roc_auc))

cenarios_destaque <- tribble(
  ~estrategia,          ~regra_decisao,
  "Sem balanceamento",  "Limiar de Youden (CV)",
  "Com downsampling",   "Limiar padrão (0,50)"
)

rotulos_estrategia <- c(
  "Sem balanceamento" = "Sem subamostragem",
  "Com downsampling" = "Com subamostragem"
)

desempenho_exportacao <- tabela_desempenho_teste |>
  mutate(
    limiar_aplicado = if_else(
      regra_decisao == "Limiar de Youden (CV)",
      limiar_youden,
      0.50
    ),
    .before = limiar_youden
  )

desempenho_resumido <- desempenho_exportacao |>
  semi_join(cenarios_destaque, by = c("estrategia", "regra_decisao")) |>
  arrange(desc(estrategia), desc(roc_auc)) |>
  transmute(
    `Cenário` = unname(rotulos_estrategia[estrategia]),
    Modelo = modelo,
    `Limiar aplicado` = limiar_aplicado,
    `ROC-AUC` = roc_auc,
    `PR-AUC` = pr_auc,
    Brier = brier_class,
    Sensibilidade = sens,
    Especificidade = spec
  )

desempenho_exportacao <- desempenho_exportacao |>
  mutate(estrategia = unname(rotulos_estrategia[estrategia]))

print(tabela_desempenho_teste, n = Inf, width = Inf)

# 4. Matrizes de confusão ----
matrizes_confusao <- bind_rows(
  predicoes_teste |>
    count(modelo, estrategia, Verdadeiro = desfecho_abandono, Predito = classe_youden) |>
    mutate(regra_decisao = "Limiar de Youden (CV)", .before = 1),
  predicoes_teste |>
    count(modelo, estrategia, Verdadeiro = desfecho_abandono, Predito = classe_050) |>
    mutate(regra_decisao = "Limiar padrão (0,50)", .before = 1)
)

# 5.1 Curvas ROC por estratégia de treino ----

predicoes_curvas <- predicoes_teste |>
  mutate(
    estrategia = factor(
      estrategia,
      levels = c("Sem balanceamento", "Com downsampling"),
      labels = c("Sem subamostragem", "Com subamostragem")
    )
  )

dados_roc <- predicoes_curvas |>
  group_by(estrategia, modelo) |>
  roc_curve(
    truth = desfecho_abandono,
    .pred_Abandono,
    event_level = "second"
  )

grafico_roc <- ggplot(
  dados_roc,
  aes(x = 1 - specificity, y = sensitivity, color = modelo, linetype = modelo)
) +
  geom_abline(
    intercept = 0, slope = 1,
    linetype = "dashed", color = cor_neutra, linewidth = 0.4
  ) +
  geom_path(linewidth = 0.7) +
  facet_wrap(vars(estrategia), nrow = 1) +
  scale_color_manual(values = cores_modelos) +
  scale_linetype_manual(values = linhas_modelos) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = rotulo_numero(0.01)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = rotulo_numero(0.01)
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    x = "1 − Especificidade (taxa de falsos positivos)",
    y = "Sensibilidade",
    caption = paste(
      "Ambos os painéis usam o mesmo conjunto de teste.",
      "A subamostragem foi aplicada apenas no treino."
    )
  ) +
  tema() +
  theme(plot.caption = element_text(hjust = 0))

salvar_figura(
  grafico_roc, "06_curva_roc_teste.png",
  altura_cm = 10, largura_cm = 20
)

# 5.2 Curvas de precisão–revocação por estratégia de treino ----

prevalencia_teste <- mean(tb_teste$desfecho_abandono == "Abandono")

dados_pr <- predicoes_curvas |>
  group_by(estrategia, modelo) |>
  pr_curve(
    truth = desfecho_abandono,
    .pred_Abandono,
    event_level = "second"
  )

grafico_pr <- ggplot(
  dados_pr,
  aes(x = recall, y = precision, color = modelo, linetype = modelo)
) +
  geom_hline(
    yintercept = prevalencia_teste,
    linetype = "dashed", color = cor_neutra, linewidth = 0.4
  ) +
  geom_path(linewidth = 0.7) +
  facet_wrap(vars(estrategia), nrow = 1) +
  scale_color_manual(values = cores_modelos) +
  scale_linetype_manual(values = linhas_modelos) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = rotulo_numero(0.01)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = rotulo_numero(0.01)
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    x = "Sensibilidade (revocação)",
    y = "Precisão (valor preditivo positivo)",
    caption = paste0(
      "Ambos os painéis usam o mesmo conjunto de teste. ",
      "A subamostragem foi aplicada apenas no treino.\n",
      "A linha tracejada indica a proporção de abandono no teste (",
      formatar_numero(100 * prevalencia_teste, accuracy = 0.01),
      "%)."
    )
  ) +
  tema() +
  theme(plot.caption = element_text(hjust = 0))

salvar_figura(
  grafico_pr, "06_curva_pr_teste.png",
  altura_cm = 10, largura_cm = 20
)

# 5.3 Figura Comparativa das Matrizes de Confusão (Sem balanceamento vs. Downsampling)
dados_grafico_matriz <- matrizes_confusao |>
  semi_join(cenarios_destaque, by = c("estrategia", "regra_decisao")) |>
  mutate(
    cenario = factor(
      if_else(
        estrategia == "Sem balanceamento",
        "Sem subamostragem\n(Limiar de Youden)",
        "Com subamostragem\n(Limiar 0,50)"
      ),
      levels = c(
        "Sem subamostragem\n(Limiar de Youden)",
        "Com subamostragem\n(Limiar 0,50)"
      )
    )
  ) |>
  group_by(cenario, modelo, Verdadeiro) |>
  mutate(
    percentual = 100 * n / sum(n),
    rotulo = paste0(
      formatar_numero(n, accuracy = 1), "\n(",
      formatar_numero(percentual, accuracy = 0.1), "%)"
    )
  ) |>
  ungroup()

grafico_matriz <- ggplot(dados_grafico_matriz, aes(x = Predito, y = fct_rev(Verdadeiro), fill = percentual)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = rotulo), size = 3, lineheight = 0.95, color = "#111111") +
  facet_grid(cenario ~ modelo) +
  scale_fill_gradient(
    low = "#f0f4f8",
    high = cor_principal,
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    x = "Classe predita pelo modelo",
    y = "Desfecho verdadeiro",
    caption = paste(
      "Contagens e percentuais por linha (classe real), em cada painel.",
      "Os cenários diferem no balanceamento e no limiar.",
      sep = "\n"
    )
  ) +
  tema() +
  theme(panel.grid = element_blank(), legend.position = "none")

salvar_figura(grafico_matriz, "06_matriz_confusao_teste.png", altura_cm = 11)

# 5.4 Distribuição das probabilidades preditas (mostra o deslocamento pelo downsampling)
grafico_probabilidades <- ggplot(predicoes_teste, aes(x = .pred_Abandono, fill = desfecho_abandono)) +
  geom_density(alpha = 0.55, color = NA, adjust = 1.5) +
  geom_vline(
    aes(xintercept = limiar_youden),
    linetype = "dashed",
    color = "#333333",
    linewidth = 0.5
  ) +
  facet_grid(
    estrategia ~ modelo,
    scales = "free_y",
    labeller = labeller(
      estrategia = c(
        "Com downsampling" = "Com\nsubamostragem",
        "Sem balanceamento" = "Sem\nsubamostragem"
      )
    )
  ) +
  scale_fill_manual(values = c("Cura" = cor_principal, "Abandono" = "#eb6834")) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = rotulo_numero(0.01)
  ) +
  scale_y_continuous(labels = rotulo_numero(0.1)) +
  labs(
    x = "Probabilidade predita de abandono",
    y = "Densidade",
    caption = paste(
      "A subamostragem da classe majoritária foi aplicada apenas no treino.",
      "As linhas tracejadas indicam o limiar de Youden de cada modelo nos dois cenários.",
      sep = "\n"
    )
  ) +
  tema() +
  theme(panel.spacing.x = unit(0.6, "cm"))

salvar_figura(grafico_probabilidades, "06_distribuicao_probabilidades.png", altura_cm = 10.5)

# 6. Salvamento dos resultados e tabelas ----
saveRDS(predicoes_teste,         here("dados", "intermediarios", "predicoes_teste.rds"))
saveRDS(tabela_desempenho_teste, here("resultados", "modelos", "tabela_desempenho_teste.rds"))

exportar_tabelas(
  list(
    desempenho_resumido = desempenho_resumido,
    desempenho_teste = desempenho_exportacao,
    matrizes_confusao = matrizes_confusao |>
      mutate(estrategia = unname(rotulos_estrategia[estrategia]))
  ),
  nome_planilha = "06_avaliacao_teste",
  salvar_csv = FALSE
)

message("✔ Script 06 concluído com sucesso!")
