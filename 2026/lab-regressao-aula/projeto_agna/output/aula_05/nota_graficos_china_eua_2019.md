# Série China–EUA e pontos ideais dos países

Gráficos gerados em 23 de setembro de 2026 por
`projeto_agna/scripts/08_serie_china_eua_pontos_ideais_2019.R`. Para reproduzir,
execute a partir da raiz de `lab-regressao-aula`:

```bash
Rscript --vanilla projeto_agna/scripts/08_serie_china_eua_pontos_ideais_2019.R
```

## Série de votos China–EUA, 1990–2019

Fonte: a coluna `ChinaAgree` na linha dos EUA do mesmo arquivo de dados usado
no paper, descrito abaixo. Cada observação é uma sessão da Assembleia Geral,
identificada pelo ano de início (`session + 1945`). São 30 sessões, de 1990 a
2019. O gráfico apresenta diretamente a proporção de concordância registrada
no arquivo, sem recalcular votos a partir de outra base.

Arquivos: `serie_convergencia_china_eua_1990_2019.csv` e
`figura_convergencia_china_eua_1990_2019.{png,pdf}`.

## Pontos ideais dos países, sessão iniciada em 2019

Fonte: `projeto_agna/data/raw/ideal_points/IdealpointestimatesAll_Jun2024.csv`,
cópia idêntica do arquivo usado no pipeline do paper em
`/Users/manoelgaldino/Documents/DCP/Papers/RDD Trade/red_trade/raw data/dataverse_files-2/IdealpointestimatesAll_Jun2024.csv`
(SHA-256 `94ce7440bdba9252b2f4294333291585748dfe84dbaf56fe9f26e1af38f66198`).
O pipeline do paper define o ano de referência como `session + 1945`; por isso,
2019 corresponde à sessão 74. Os dois gráficos seguem essa convenção.

O gráfico usa diretamente a mediana posterior `Q50%All` já existente no
arquivo, para os 193 países da sessão 74. Os traços dos quatro países
identificados vão de `Q5%All` a `Q95%All` (intervalo posterior central de 90%).
Não há reestimação de pontos ideais. Os valores dos países destacados são:

| País | Ponto ideal (`Q50%All`) |
| --- | ---: |
| China | -0,3713594 |
| Rússia | 0,09219355 |
| Brasil | 0,2885661 |
| EUA | 2,606005 |

Arquivos: `pontos_ideais_2019.csv` e `figura_pontos_ideais_2019.{png,pdf}`.
