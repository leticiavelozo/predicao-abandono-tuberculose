# Desenvolvimento e Avaliação de um Pipeline Computacional para Predição do Abandono do Tratamento da Tuberculose em Minas Gerais
Este repositório contém os scripts, resultados e documentação de um projeto de Ciência de Dados aplicado à Saúde Pública.

## Objetivo

Avaliar, adaptar e desenvolver um pipeline computacional modular em linguagem R para a predição do abandono do tratamento da tuberculose em Minas Gerais, a partir da replicação independente e da extensão do protocolo proposto por Rodrigues et al. (2024). O trabalho busca mostrar que é possível, a partir de um artigo de referência, replicar e adaptar uma abordagem a uma população diferente, dentro do mesmo contexto (dados de rotina do SINAN sobre desfecho do tratamento da tuberculose), documentando o que foi mantido, o que precisou ser adaptado e o que foi estendido. O objetivo não é validar o artigo de referência.

## Questão de pesquisa

É possível replicar e estender, a partir dos dados abertos de Minas Gerais, a abordagem computacional proposta por Rodrigues et al. (2024) para a predição do abandono do tratamento da tuberculose, considerando as particularidades da base estadual e as adaptações metodológicas necessárias?

Questões secundárias:

- Quais etapas da metodologia de Rodrigues et al. (2024) puderam ser replicadas diretamente e quais exigiram adaptações, em função da estrutura, da disponibilidade e do preenchimento das informações na base de Minas Gerais?
- De que maneira o treinamento com e sem subamostragem da classe majoritária e o uso de dois limiares de classificação (0,50 e Youden) se refletiram no desempenho dos modelos no conjunto de teste?
- Quais semelhanças e diferenças foram observadas em relação aos resultados do estudo de referência (seleção de atributos, importância dos preditores e desempenho), e quais diferenças entre as bases e os procedimentos limitam essa comparação?
- Em que medida a disponibilização do código e da documentação favorece a rastreabilidade e a reprodutibilidade dos resultados, permitindo aplicar a abordagem a dados de outros períodos ou localidades?

## Metodologia

Este projeto consiste em uma replicação independente, adaptada e estendida do pipeline de Rodrigues et al. (2024). A implementação foi feita em R a partir da descrição publicada no artigo, sem uso do código original, que não foi localizado no repositório indicado pelos autores, e aplicada a dados de Minas Gerais. Por isso, o trabalho é uma reconstrução e adaptação da abordagem publicada, e não uma reprodução computacional integral da análise original.

Adota-se a distinção das National Academies (NASEM, 2019): replicabilidade é obter resultados consistentes em estudos que respondem à mesma pergunta com outros dados; reprodutibilidade é obter os mesmos resultados com os mesmos dados, o mesmo código e as mesmas condições de análise.

O trabalho tem três componentes:

- **Replicação e adaptação:** reconstrução das etapas de ingestão, filtragem, seleção de atributos e modelagem para a base estadual, com registro das diferenças em relação ao artigo.
- **Extensão:** comparação entre o treinamento sem subamostragem e com subamostragem (*downsampling*) restrita ao treino, e entre o limiar fixo de 0,50 e o limiar de Youden, no mesmo conjunto de teste sem balanceamento. No artigo, a subamostragem foi aplicada antes da divisão entre treino e teste.
- **Comparação descritiva com o artigo:** serve para mostrar a consistência da adaptação. Populações, períodos, campos disponíveis, codificação dos preditores e protocolo de avaliação diferem, então a comparação não constitui validação externa nem comprova superioridade em relação ao estudo nacional.

A implementação foi desenvolvida em linguagem R (`tidymodels`) e aplicada a uma população de estudo composta por notificações de tuberculose de Minas Gerais encerradas por cura ou abandono.

### Etapas analíticas dos scripts
1. **Configuração e auditoria dos dados** (`00`, `01`, `02`)
   - Configuração de ambiente controlada por `renv` e caminhos relativos via `here`.
   - Leitura segura da base bruta e padronização de tipos e datas.
   - Análise exploratória univariada e bivariada do perfil sociodemográfico e clínico.

2. **Construção da base de dados de modelagem** (`03A`, `03B`)
   - Notificações encerradas de 2010 a 2024 (exclusão de 2025 e 2026 por maior incompletude da situação de encerramento).
   - Critérios de elegibilidade: idade >= 18 anos, tuberculose pulmonar, tipo de entrada caso novo, recidiva ou reingresso após abandono, e desfecho de encerramento como cura ou abandono.
   - Exclusão apenas de registros com indicação das condições abaixo. Registros com o campo ausente ou com outra resposta foram mantidos, e a ausência não confirma que a condição não existe:
     - privação de liberdade (resposta "Sim");
     - gestação no 1º, 2º ou 3º trimestre ou com idade gestacional ignorada;
     - resistência a fármacos de primeira linha (isoniazida, rifampicina, ambas ou outras) no teste de sensibilidade.
   - Os filtros de situação de rua, imigrante e profissional de saúde, usados no artigo, não puderam ser aplicados por ausência de campo correspondente na base estadual.
   - Partição estratificada 70% treino / 30% teste (semente fixa `42`).
   - Geração da tabela descritiva comparativa (Cura vs. Abandono) via `gtsummary`.

3. **Seleção de atributos** (`04`)
   - Avaliação de 12 preditores candidatos disponíveis na notificação.
   - RFECV com Random Forest via validação cruzada estratificada de 10 folds, com subconjunto de **7 variáveis** escolhido pela maior ROC-AUC média (`nu_idade_n`, `cs_sexo`, `cs_raca`, `agravalcoo`, `agravdroga`, `agravaids`, `tb_previa`). Escolaridade e tabagismo, de alta incompletude, não foram mantidas nessa trajetória de seleção. Isso não demonstra que sejam irrelevantes nem que a incompletude seja a causa da exclusão.

4. **Treinamento e cenários experimentais** (`05`)
   - Ajuste de três algoritmos via `tidymodels`: Regressão Logística LASSO (`glmnet`), Random Forest (`ranger`) e LightGBM (`bonsai`).
   - Dois cenários de treinamento para cada algoritmo:
     - **Sem subamostragem:** preserva a distribuição das classes; avaliado com o limiar fixo de 0,50 e com o limiar de Youden estimado na validação cruzada.
     - **Com subamostragem 1:1 (*downsampling*):** aplicada apenas à porção de treino de cada fold e ao treino final; avaliado com os mesmos dois limiares.

5. **Avaliação no conjunto de teste** (`06`)
   - Métricas de discriminação (ROC-AUC e PR-AUC) e erro probabilístico (escore de Brier). O Brier não substitui uma avaliação específica de calibração.
   - Geração de curvas ROC e Precision-Recall.
   - Figura comparativa das matrizes de confusão em painel (contagens absolutas e percentuais por classe real) e gráfico de densidade das probabilidades preditas.

6. **Interpretabilidade** (`07`)
   - Razões de chances (*odds ratios*) com IC 95% via regressão logística não penalizada e o pacote `broom`.
   - Valores SHAP em um LightGBM ajustado especificamente para interpretabilidade (`shapviz`), com agrupamento das variáveis *dummy* de volta às 7 variáveis originais. As explicações referem-se a esse modelo de interpretabilidade, e não diretamente ao modelo usado na avaliação preditiva.

7. **Reprodutibilidade computacional**
   - Sementes fixas nas etapas estocásticas e dependências registradas em `renv.lock`. Esses registros fornecem as condições para a reprodução, que depende do mesmo arquivo de entrada, do ambiente restaurado e da execução dos scripts na ordem prevista.

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

Os dados utilizados neste projeto são públicos, provenientes do Portal de Dados Abertos do Estado de Minas Gerais e não contêm nomes nem números de documentos das pessoas notificadas:

- Conjunto de dados: [Dados de Tuberculose — Portal de Dados Abertos de Minas Gerais](https://dados.mg.gov.br/dataset/dados-tuberculose)
- Instituição responsável: Secretaria de Estado de Saúde de Minas Gerais (SES-MG)
- Unidade técnica: Diretoria de Vigilância de Condições Crônicas
- Formato: CSV com separador ponto e vírgula (;) e metadados em datapackage.json
- Data de download do CSV utilizado na análise: 25/08/2026
- SHA-256 do CSV utilizado: 8D6FD5547642DDEEFE76D86631518EBBF5D2B732854B6E2A1700D037BF60861E

Os registros combinam município, unidade de saúde, datas, idade e sexo, o que pode, em casos raros, permitir a reidentificação por quem conheça o paciente. Os dados devem ser usados apenas para fins de pesquisa, e não se deve tentar reidentificar pessoas.

A base bruta não é versionada neste repositório. Para executar o pipeline, baixe o CSV na fonte oficial e salve-o no caminho abaixo, relativo à raiz do projeto.

```text
dados/brutos/dados_tuberculose.csv
```

## Reprodução dos resultados

A reprodução exata dos resultados apresentados no TCC depende da cópia do CSV baixada em 25/08/2026. O SHA-256 permite verificar se uma cópia corresponde ao arquivo utilizado nas análises.

Na página pública do conjunto, não foi localizada uma opção para baixar versões anteriores do CSV. Como o portal atualiza a base, uma nova extração pode apresentar diferenças no número de notificações e no preenchimento dos encerramentos, inclusive para os mesmos anos de notificação. Essas diferenças podem alterar a composição da amostra, as estatísticas descritivas e as métricas de desempenho dos modelos.

Disponibilizar o código e o registro do ambiente não garante, por si só, resultados idênticos: a reprodução depende do mesmo arquivo de entrada, da restauração consistente do ambiente e da execução na ordem prevista.

## Limitações

- Os resultados descrevem desempenho em uma divisão aleatória da própria base estadual, sem validação temporal, geográfica ou externa, e não indicam prontidão para uso assistencial.
- As métricas são estimativas pontuais, sem intervalos de confiança nem testes entre modelos.
- A seleção de atributos e a escolha da penalidade do LASSO usaram as mesmas dobras de validação; as métricas de validação cruzada são, portanto, internas ao desenvolvimento.
- O Brier não comprova calibração em todos os níveis de risco; não foi feita avaliação específica de calibração.
- A unidade de análise é a notificação; não houve vinculação de episódios da mesma pessoa.

## Referência metodológica

Rodrigues, M. M. S. et al. Machine learning algorithms using national registry data to predict loss to follow-up during tuberculosis treatment. BMC Public Health, 2024. DOI: 10.1186/s12889-024-18815-0.
