# ==============================================================================
# PROJETO: Predição do Abandono do Tratamento da Tuberculose em Minas Gerais
# Arquivo: scripts/02_analise_exploratoria.R
# Objetivo: Panorama geral dos encerramentos e série temporal (2010–2024)
# ==============================================================================

library(tidyverse)
library(here)

# Tema, cores e salvamento das figuras definidos em funcoes_auxiliares.R
source(here("scripts", "funcoes_auxiliares.R"))

# Leitura e preparação da base exploratória ----
df_tb <- readRDS(here("dados", "intermediarios", "tb_dados_com_encerramento.rds")) 

# Distribuição geral das situações de encerramento ----
resumo_desfechos <- df_tb |>
  count(situa_ence, sort = TRUE, name = "n_casos") |>
  mutate(
    percentual = round(100 * n_casos / sum(n_casos), 2),
    rotulo_eixo = paste0(situa_ence, " (n = ", formatar_numero(n_casos, 1), ")")
  )

print(resumo_desfechos)

p_desfechos <- ggplot(resumo_desfechos, aes(x = reorder(rotulo_eixo, percentual), y = percentual)) +
  geom_col(fill = cor_principal, width = 0.65) +
  geom_text(
    aes(label = paste0(formatar_numero(percentual, 0.1), "%")),
    hjust = -0.15,
    size = 3,
    color = "#222222"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = rotulo_numero(1, suffix = "%"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    x = NULL,
    y = "Proporção dos casos encerrados (%)"
  ) +
  tema() +
  theme(legend.position = "none", panel.grid.major.y = element_blank())

salvar_figura(p_desfechos, "01_distribuicao_encerramentos.png", altura_cm = 10)

# 2. Evolução anual da taxa de abandono (2010 a 2024) ----
df_abandono_ano <- df_tb |>
  group_by(nu_ano) |>
  summarise(
    total_casos = n(),
    qtd_abandono = sum(situa_ence == "Abandono", na.rm = TRUE),
    taxa_abandono = round(100 * qtd_abandono / total_casos, 2),
    .groups = "drop"
  )

print(df_abandono_ano, n = Inf)

p_temporal <- ggplot(df_abandono_ano, aes(x = nu_ano, y = taxa_abandono)) +
  geom_line(color = cor_principal, linewidth = 0.7) +
  geom_point(color = cor_principal, size = 1.8) +
  geom_text(
    aes(label = formatar_numero(taxa_abandono, 0.1)),
    vjust = -1.1,
    size = 2.6,
    color = "#222222"
  ) +
  scale_y_continuous(
    labels = rotulo_numero(1, suffix = "%"),
    limits = c(0, max(df_abandono_ano$taxa_abandono) * 1.25),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(breaks = seq(min(df_abandono_ano$nu_ano), max(df_abandono_ano$nu_ano), by = 1)) +
  labs(
    x = "Ano de notificação",
    y = "Taxa de abandono regular (%)"
  ) +
  tema() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

salvar_figura(p_temporal, "02_evolucao_abandono_temporal.png", altura_cm = 8)

# 3. Exportação das tabelas de apoio ----
exportar_tabelas(
  lista_tabelas = list(
    "Desfechos Geral" = select(resumo_desfechos, situa_ence, n_casos, percentual),
    "Evolucao Anual Abandono" = df_abandono_ano
  ),
  nome_planilha = "02_analise_exploratoria",
  salvar_csv = FALSE
)

message("✔ Script 02 concluído com sucesso!")