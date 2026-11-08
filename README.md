# Desenvolvimento e Avaliação de um Pipeline Computacional para Predição do Abandono do Tratamento da Tuberculose em Minas Gerais
Este repositório contém os scripts, resultados e documentação de um projeto de Ciência de Dados aplicado à Saúde Pública.

## Objetivo

Replicar de forma independente o pipeline de aprendizado de máquina proposto por Rodrigues et al. (2024) para predição do abandono do tratamento da tuberculose, desenvolvendo uma implementação própria em R e avaliando seu desempenho, interpretabilidade e limitações em uma população de estudo composta por casos notificados em Minas Gerais.

## Questão de pesquisa

A abordagem metodológica de Rodrigues et al. (2024), replicada de forma independente, produz desempenho preditivo consistente para o abandono do tratamento da tuberculose em uma população de casos notificados em Minas Gerais?

Perguntas secundárias:

- Quais etapas da metodologia original podem ser replicadas diretamente?
- Quais etapas precisam ser adaptadas?
- Quais características dos dados influenciam os resultados?
- O desempenho do método se mantém no novo conjunto de dados?
- Quais são as limitações da replicação?
- O código desenvolvido permite reproduzir os resultados obtidos?

## Metodologia

Este projeto consiste em uma replicação independente do pipeline de Rodrigues et al. (2024): a implementação foi feita a partir da descrição publicada no artigo, sem uso do código original, e aplicada a dados novos. Segue-se a definição das National Academies (NASEM, 2019), em que replicabilidade é obter resultados consistentes em estudos que respondem à mesma pergunta com dados próprios, e reprodutibilidade é obter os mesmos resultados com os mesmos dados e o mesmo código .

O trabalho tem duas partes: a replicação, que avalia se a estratégia metodológica original produz resultados comparáveis quando reaplicada, com implementação independente, a dados de Minas Gerais; e a avaliação, que analisa criticamente o método, em particular a comparação entre a estratégia sem balanceamento com limiar de Youden e a abordagem com subamostragem (*downsampling*) .

A implementação foi desenvolvida em linguagem R (`tidymodels`) e aplicada a uma população de estudo composta por casos de tuberculose notificados em Minas Gerais.

### Etapas analíticas dos scripts
1. **Configuração e auditoria dos dados** (`00`, `01`, `02`)
   - Configuração de ambiente controlada por `renv` e caminhos relativos via `here`.
   - Leitura segura da base bruta e padronização de tipos e datas.
   - Análise exploratória univariada e bivariada do perfil sociodemográfico e clínico.

2. **Construção da base de dados de modelagem** (`03A`, `03B`)
   - Notificações encerradas de 2010 a 2024 (exclusão de 2025 por encerramento incompleto).
   - Critérios de elegibilidade: idade $\ge$ 18 anos, tuberculose pulmonar, e desfecho de encerramento como cura ou abandono .
   - Exclusão de população privada de liberdade, notificação pós-óbito, resistência a fármacos (TB-DR) e gestantes.
   - Partição estratificada 70% treino / 30% teste (semente fixa `42`).
   - Geração da tabela descritiva comparativa (Cura vs. Abandono) via `gtsummary`.

3. **Seleção de atributos** (`04`)
   - Avaliação de 12 preditores candidatos disponíveis na notificação.
   - RFECV com Random Forest via validação cruzada estratificada de 10 folds para identificar o subconjunto ideal de **7 variáveis** (`nu_idade_n`, `cs_sexo`, `cs_raca`, `agravalcoo`, `agravdroga`, `agravaids`, `tb_previa`), mitigando o impacto de variáveis com alta taxa de dados ausentes como escolaridade e tabagismo.

4. **Treinamento e cenários experimentais** (`05`)
   - Ajuste de três algoritmos via `tidymodels`: Regressão Logística LASSO (`glmnet`), Random Forest (`ranger`) e LightGBM (`bonsai`).
   - Comparação metodológica central:
     - **Cenário Principal:** Treinamento sem balanceamento e definição do limiar de decisão otimizado pelo **Índice de Youden** na validação cruzada .
     - **Cenário Comparativo (Replicação do Artigo):** Subamostragem 1:1 (*downsampling*) restrita aos dados de treino de cada fold, avaliada com o limiar padrão de 0,50 .

5. **Avaliação no conjunto de teste** (`06`)
   - Computação de métricas de discriminação (ROC-AUC e PR-AUC) e calibração (**Escore de Brier**).
   - Geração de curvas ROC e Precision-Recall .
   - Construção da figura comparativa das Matrizes de Confusão em painel (exibindo contagens absolutas e percentuais por classe real) e do gráfico de densidade de probabilidades preditas .

6. **Interpretabilidade** (`07`)
   - Extração de Razões de Chances (*Odds Ratios*) com IC 95% via Regressão Logística não penalizada e o pacote `broom` .
   - Cálculo de valores SHAP no LightGBM utilizando `shapviz`, com agrupamento das variáveis *dummy* de volta às 8 variáveis originais para gerar gráficos limpos de importância global .

7. **Reprodutibilidade computacional**
   - Sementes fixas em todas as etapas estocásticas para garantir total determinismo e reprodutibilidade dos resultados.

## Estrutura

- `dados/brutos/`: base original, sem alterações
- `dados/intermediarios/`: bases padronizadas, população de estudo, predições do teste e artefatos intermediários
- `dados/processados/`: bases finais de modelagem (`tb_analitica.rds`, `tb_treino.rds`, `tb_teste.rds`) e folds de validação cruzada (`cv_folds.rds`)
- `resultados/tabelas/`: tabelas em `.xlsx` (exportadas sem arquivos CSV redundantes)
- `resultados/figuras/`: figuras em `.png` (300 dpi, padronizadas com os temas visuais do projeto)
- `resultados/modelos/`: receita final, variáveis selecionadas e modelos treinados
- `resultados/sessionInfo.txt`: versões do R e dos pacotes usados na última execução
- `scripts/`: scripts R organizados na sequência do pipeline

## Ordem de execução

```text
scripts/
├── 00_configuracao_projeto.R      # estrutura de pastas e opções gerais
├── funcoes_auxiliares.R            # funções compartilhadas (temas, formatação e salvamento)
├── 01_leitura_dados.R              # leitura, padronização e auditoria de qualidade
├── 02_analise_exploratoria.R       # análise exploratória univariada e bivariada
├── 03A_preparacao_modelagem.R      # população de estudo, desfecho, fluxograma e partição 70/30
├── 03B_analise_descritiva.R        # tabela descritiva Cura vs. Abandono (gtsummary)
├── 04_selecao_atributos_rfecv.R    # RFECV com Random Forest
├── 05_treinamento_modelos.R        # regressão logística LASSO, Random Forest e LightGBM
├── 06_avaliacao_teste.R            # métricas, curvas ROC/PR, Brier e matrizes comparativas
└── 07_interpretabilidade.R         # odds ratios (broom) e valores SHAP agrupados
```

Cada script foi projetado para rodar de forma independente em uma sessão limpa do R, lendo os artefatos .rds produzidos pela etapa imediatamente anterior. Caso utilize gerenciamento de ambiente com renv, execute renv::restore() antes da primeira execução.

## Dados

Os dados utilizados neste projeto são públicos e anonimizados, provenientes do Portal de Dados Abertos do Estado de Minas Gerais:

- Conjunto de dados: [Dados de Tuberculose — Portal de Dados Abertos de Minas Gerais](https://dados.mg.gov.br/dataset/dados-tuberculose)
- Instituição responsável: Secretaria de Estado de Saúde de Minas Gerais (SES-MG)
- Unidade técnica: Diretoria de Vigilância de Condições Crônicas
- Formato: CSV com separador ponto e vírgula (;) e metadados em datapackage.json
- Data de download do CSV utilizado na análise: 25/08/2026
- SHA-256 do CSV utilizado: 8D6FD5547642DDEEFE76D86631518EBBF5D2B732854B6E2A1700D037BF60861E

A base bruta não é versionada neste repositório. Para executar o pipeline com os dados atualmente disponíveis, baixe o CSV na fonte oficial e salve-o no caminho abaixo, relativo à raiz do projeto.

```text
dados/brutos/dados_tuberculose.csv
```
Reprodução dos resultados

A reprodução exata dos resultados apresentados no TCC depende da cópia do CSV baixada em 25/08/2026. O SHA-256 permite verificar se uma cópia corresponde ao arquivo utilizado nas análises.

Na página pública do conjunto, não foi localizada uma opção para baixar versões anteriores do CSV. Como o portal atualiza a base, uma nova extração pode apresentar diferenças no número de notificações e no preenchimento dos encerramentos, inclusive para os mesmos anos de notificação. Essas diferenças podem alterar a composição da amostra, as estatísticas descritivas e as métricas de desempenho dos modelos.

## Referência metodológica

Rodrigues, M. M. S. et al. Machine learning algorithms using national registry data to predict loss to follow-up during tuberculosis treatment. BMC Public Health, 2024. DOI: 10.1186/s12889-024-18815-0.
