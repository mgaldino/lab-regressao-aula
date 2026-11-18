# Aula 11 — Termos de interação

[Slides em PDF](../../projeto_agna/output/aula_11/previa_2026-11-18/slides_aula_11_interacoes.pdf) · [Lista de exercícios](../../output/pdf/exercicios_interacao_2026.pdf) · [Gráfico das inclinações com IC de 95%](../../output/pdf/inclinacoes_RJ_SP_HC3_aula11.pdf)

## Prática em R

- [Com internet: script com os dados observados](../../projeto_agna/scripts/32_laboratorio_aula11_interacoes.R). O primeiro bloco importa do GitHub um [CSV com as 447 zonas de RJ e SP](../../projeto_agna/data/aula_11/zonas_observadas_2022.csv).
- [Sem internet: script com dados sintéticos](../../projeto_agna/scripts/33_laboratorio_aula11_offline.R). Copie o primeiro bloco para uma sessão de R e execute até a criação de `pct_lula_t2`. Ele cria a base `zonas` com R básico, inclusive em um computador sem os pacotes da prática. O restante do script usa `ggplot2`, `sandwich`, `lmtest` e `marginaleffects`. A [cópia da base sintética em CSV](../../projeto_agna/data/aula_11/zonas_sinteticas.csv) foi gerada com a mesma semente do script.

Cada linha representa uma zona eleitoral. As duas colunas de votação são percentuais dos votos válidos de cada turno. O recorte observado vem da amostra de treino da Aula 9 nas eleições presidenciais de 2022, com 132 zonas no RJ e 315 em SP. O CSV conserva as quatro colunas usadas na prática. A fonte é o TSE, consultado em 13/10/2026, conforme o [registro da base](../../projeto_agna/data/eleicoes_2022_2026/docs/SOURCES.yaml).

Na simulação, os números de zonas, a distribuição da votação no primeiro turno e as retas dos dois estados foram calibrados nesse recorte. Os identificadores de zona são fictícios e os resultados da atividade variam em relação aos dos slides.

[Roteiro da aula](../../projeto_agna/roteiros/aula_11/2026-11-09/roteiro_v1_revisado.pdf) · [Nota para o quadro sobre interação causal](../../projeto_agna/roteiros/aula_11/2026-11-18_complemento_causal/nota_interacao_causal.pdf)
