# ==============================================================================
# PROJETO: Predição do Abandono do Tratamento da Tuberculose em Minas Gerais
# Arquivo: scripts/07_interpretabilidade.R
# Objetivo:
# - Estimar Razões de Chances (Odds Ratios) via Regressão Logística (GLM)
# - Calcular valores SHAP do LightGBM agrupados pelas 8 variáveis originais
# - Gerar gráficos de interpretabilidade (Forest Plot e gráficos SHAP)
# ==============================================================================

library(tidyverse)
library(tidymodels)
library(bonsai)
library(lightgbm)
library(shapviz)
library(here)

source(here("scripts", "funcoes_auxiliares.R"))

# Carregamento dos dados e variáveis selecionadas ----
tb_treino        <- readRDS(here("dados", "processados", "tb_treino.rds"))
tb_teste         <- readRDS(here("dados", "processados", "tb_teste.rds"))
variaveis_finais <- readRDS(here("resultados", "modelos", "vars_selecionadas.rds"))

if (length(variaveis_finais) == 1 && str_detect(variaveis_finais, ",")) {
  variaveis_finais <- str_split_1(variaveis_finais, ",\\s*")
}

# Dicionário de nomes legíveis para as figuras e tabelas
rotulos_variaveis <- c(
  nu_idade_n = "Idade (anos)",
  cs_sexo    = "Sexo",
  cs_raca    = "Raça/Cor",
  cs_zona    = "Zona de residência",
  agravalcoo = "Uso de álcool",
  agravdroga = "Uso de drogas ilícitas",
  agravaids  = "Co-infecção por HIV/aids",
  tb_previa  = "Histórico de TB prévia"
)

# RAZÕES DE CHANCES (ODDS RATIOS) — REGRESSÃO LOGÍSTICA ----
# Prepara a base definindo as categorias de referência (Feminino, Branca, Urbana, Não)
preparar_base_or <- function(df) {
  base_prep <- df |>
    select(desfecho_abandono, all_of(variaveis_finais)) |>
    mutate(
      across(where(is.character), ~ replace_na(.x, "Sem informação")),
      cs_sexo    = fct_relevel(cs_sexo, "Feminino"),
      cs_raca    = fct_relevel(cs_raca, "Branca"),
      agravalcoo = fct_relevel(agravalcoo, "Não"),
      agravdroga = fct_relevel(agravdroga, "Não"),
      agravaids  = fct_relevel(agravaids, "Não"),
      tb_previa  = fct_relevel(tb_previa, "Não")
    )
  
  if ("cs_zona" %in% names(base_prep)) {
    base_prep <- base_prep |> mutate(cs_zona = fct_relevel(cs_zona, "Urbana"))
  }
  base_prep
}

treino_or <- preparar_base_or(tb_treino)

modelo_glm <- glm(
  desfecho_abandono ~ .,
  data = treino_or,
  family = binomial(link = "logit")
)

# Extrai Odds Ratios e IC 95% diretamente com broom::tidy
tabela_or <- tidy(modelo_glm, exponentiate = TRUE, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  rename(
    termo      = term,
    odds_ratio = estimate,
    erro_pad   = std.error,
    estat_z    = statistic,
    p_valor    = p.value,
    ic95_inf   = conf.low,
    ic95_sup   = conf.high
  ) |>
  arrange(desc(odds_ratio))

print(tabela_or, n = Inf)

# Figura 1: Forest Plot das Razões de Chances (excluindo categorias "Sem informação" para limpeza visual, ou mantendo todas)
nota_or <- if ("cs_sexoSem informação" %in% tabela_or$termo) {
  "Sexo sem informação foi omitido do gráfico por instabilidade da estimativa.\nO termo consta na tabela completa."
} else {
  NULL
}

dados_forest <- tabela_or |>
  filter(termo != "cs_sexoSem informação") |>
  mutate(
    termo_limpo = termo |>
      str_replace("^nu_idade_n$", "Idade (por ano adicional)") |>
      str_replace("^cs_sexo", "Sexo: ") |>
      str_replace("^cs_raca", "Raça/Cor: ") |>
      str_replace("^cs_zona", "Zona: ") |>
      str_replace("^agravalcoo", "Uso de álcool: ") |>
      str_replace("^agravdroga", "Uso de drogas: ") |>
      str_replace("^agravaids", "HIV/aids: ") |>
      str_replace("^tb_previa", "TB prévia: "),
    termo_limpo = fct_reorder(termo_limpo, odds_ratio)
  )

grafico_or <- ggplot(dados_forest, aes(x = odds_ratio, y = termo_limpo)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = cor_neutra, linewidth = 0.5) +
  geom_errorbar(aes(xmin = ic95_inf, xmax = ic95_sup), height = 0.2, color = "#555555", linewidth = 0.5) +
  geom_point(color = cor_principal, size = 2.2) +
  scale_x_log10(labels = rotulo_numero(0.1)) +
  labs(
    x = "Razão de Chances ajustada (escala logarítmica, IC 95%)",
    y = NULL,
    caption = nota_or
  ) +
  tema()

salvar_figura(grafico_or, "07_odds_ratios_logistica.png", altura_cm = 12)

#VALORES SHAP (LIGHTGBM) ----

# Receita com codificação one-hot para extrair a matriz numérica exigida pelo lightgbm/shapviz
receita_shap <- recipe(
  as.formula(paste("desfecho_abandono ~", paste(variaveis_finais, collapse = " + "))),
  data = tb_treino
) |>
  step_string2factor(all_nominal_predictors()) |>
  step_unknown(all_nominal_predictors(), new_level = "Missing") |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE, naming = nomes_dummy) |>
  prep(training = tb_treino)

matriz_treino <- bake(receita_shap, new_data = tb_treino, all_predictors(), composition = "matrix")
matriz_teste  <- bake(receita_shap, new_data = tb_teste,  all_predictors(), composition = "matrix")
y_treino      <- as.integer(tb_treino$desfecho_abandono == "Abandono")

# Treina o LightGBM nativo com os mesmos hiperparâmetros do Script 05
set.seed(2024)
dtrain <- lgb.Dataset(data = matriz_treino, label = y_treino)

modelo_lgb_shap <- lgb.train(
  params = list(
    objective        = "binary",
    max_depth        = 4,
    learning_rate    = 0.01,
    num_leaves       = 15,
    seed             = 2024,
    deterministic    = TRUE,
    force_row_wise   = TRUE,
    num_threads      = 1,
    verbose          = -1
  ),
  data = dtrain,
  nrounds = 500
)
restaurar_localidade()

# Calcula os valores SHAP no conjunto de teste e agrupa pelas 8 variáveis originais
shap_bruto <- shapviz(modelo_lgb_shap, X_pred = matriz_teste)

# Mapeia cada coluna dummy de volta para sua variável original
mapa_colunas <- map(variaveis_finais, function(var) {
  colnames(matriz_teste)[str_starts(colnames(matriz_teste), var)]
})
names(mapa_colunas) <- unname(rotulos_variaveis[variaveis_finais])

# Colapsa (soma) os valores SHAP das dummies dentro de cada variável original
Dados_teste_originais <- tb_teste |>
  select(all_of(variaveis_finais)) |>
  mutate(across(where(is.character), ~ replace_na(.x, "Sem informação"))) |>
  rename_with(~ unname(rotulos_variaveis[.x]))

shap_agrupado <- collapse_shap(shap_bruto$S, collapse = mapa_colunas)
objeto_shap   <- shapviz(shap_agrupado, X = Dados_teste_originais)

# Tabela de importância global SHAP (média do valor absoluto de SHAP por variável)
tabela_importancia_shap <- enframe(
  colMeans(abs(objeto_shap$S)),
  name  = "variavel",
  value = "shap_medio_absoluto"
) |>
  arrange(desc(shap_medio_absoluto))

print(tabela_importancia_shap)

# Figura 2: Importância Global SHAP (Gráfico de Barras)
grafico_shap_barras <- sv_importance(objeto_shap, kind = "bar", fill = cor_principal) +
  scale_x_continuous(labels = rotulo_numero(0.01)) +
  labs(x = "Importância média (|SHAP| médio na escala log-odds)", y = NULL) +
  tema()

salvar_figura(grafico_shap_barras, "07_shap_importancia_global.png", altura_cm = 8)

# Rótulos para o gráfico SHAP
rotulos_shap <- c(
  nu_idade_n = "Idade (anos)",
  tb_previa  = "TB prévia (Sim vs. Não)",
  agravalcoo = "Uso de álcool (Sim vs. Não)",
  agravdroga = "Uso de drogas (Sim vs. Não)",
  cs_sexo    = "Sexo (Masculino vs. Feminino)",
  agravaids  = "HIV/aids (Sim vs. Não)",
  cs_raca    = "Raça/Cor",
  cs_zona    = "Zona de residência"
)

mapa_colunas <- map(variaveis_finais, function(var) {
  colnames(matriz_teste)[str_starts(colnames(matriz_teste), var)]
})
names(mapa_colunas) <- unname(rotulos_shap[variaveis_finais])

# Garante ordem lógica das cores: Roxo (Baixo) = Referência/Não | Laranja = Sem inf. | Amarelo (Alto) = Sim
Dados_teste_originais <- tb_teste |>
  select(all_of(variaveis_finais)) |>
  mutate(
    across(where(is.character), ~ replace_na(.x, "Sem informação")),
    cs_sexo    = factor(cs_sexo, levels = c("Feminino", "Sem informação", "Masculino")),
    cs_raca    = factor(cs_raca, levels = c("Branca", "Sem informação", "Amarela", "Parda", "Preta", "Indígena")),
    agravalcoo = factor(agravalcoo, levels = c("Não", "Sem informação", "Sim")),
    agravdroga = factor(agravdroga, levels = c("Não", "Sem informação", "Sim")),
    agravaids  = factor(agravaids,  levels = c("Não", "Sem informação", "Sim")),
    tb_previa  = factor(tb_previa,  levels = c("Não", "Sem informação", "Sim"))
  ) |>
  rename_with(~ unname(rotulos_shap[.x]))

shap_agrupado <- collapse_shap(shap_bruto$S, collapse = mapa_colunas)
objeto_shap   <- shapviz(shap_agrupado, X = Dados_teste_originais)

# Figura 3: Resumo de Impacto SHAP (Beeswarm limpo e sem sobreposições)
grafico_shap_beeswarm <- sv_importance(objeto_shap, kind = "beeswarm", size = 1.2) +
  scale_x_continuous(labels = rotulo_numero(0.1)) +
  labs(
    x = "Valor SHAP (impacto no log-odds de Abandono)",
    y = NULL,
    color = "Valor da variável",
    caption = paste(
      "Valores SHAP negativos reduzem e valores positivos aumentam o log-odds predito de abandono.",
      "Nas variáveis categóricas, a cor representa a ordem dos níveis.",
      sep = "\n"
    )
  ) +
  tema() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8, hjust = 0.5),
    legend.text  = element_text(size = 7.5),
    # Centraliza a nota da figura.
    plot.caption = element_text(size = 8, color = "#555555", hjust = 0.5, margin = margin(t = 15))
  ) +
  guides(
    color = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(0.35, "cm"),
      barheight = unit(4, "cm"),
      ticks = FALSE
    )
  )

salvar_figura(grafico_shap_beeswarm, "07_shap_beeswarm.png", altura_cm = 10, largura_cm = 20)

# TABELAS

exportar_tabelas(
  list(
    odds_ratios_logistica = tabela_or |>
      mutate(p_valor = if_else(
        p_valor < 0.0001,
        "< 0,0001",
        formatar_numero(p_valor, accuracy = 0.0001)
      )),
    importancia_shap = tabela_importancia_shap,
    notas = tibble(Nota = c(
      "Limites do IC 95% indisponíveis são apresentados em branco.",
      str_replace_all(nota_or %||% character(), stringr::fixed("\n"), " ")
    ))
  ),
  nome_planilha = "07_interpretabilidade",
  salvar_csv = FALSE
)

message("✔ Script 07 concluído com sucesso! Todo o pipeline foi finalizado!")