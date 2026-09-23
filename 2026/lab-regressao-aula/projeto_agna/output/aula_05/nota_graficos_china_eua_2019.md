# Série China–EUA e pontos ideais em 2019

Preparado em 23 de setembro de 2026 com a fonte local
`projeto_agna/data/raw/unvotes/unvotes_0.3.0.tar.gz` (SHA-256:
`5c826e9ab6d6aa6bb52fe936050783e2ce5ba879166ba0444fe2f6e530dfdeed`).
O script reproduzível é `projeto_agna/scripts/08_serie_china_eua_pontos_ideais_2019.R`.
Execute-o a partir da raiz de `lab-regressao-aula` com:

```bash
Rscript --vanilla projeto_agna/scripts/08_serie_china_eua_pontos_ideais_2019.R
```

## Série China–EUA

Cada ano mostra a fração de votações nominais em que China e EUA registraram
o mesmo voto (`yes`, `no` ou `abstain`). O denominador contém apenas votações
com votos observados de ambos. Há 30 anos completos, de 1990 a 2019,
2.567 pares válidos e 417 votos iguais (16,2% no período). Os denominadores
anuais variam de 61 a 127. Os anos de 1997 a 2016 foram conferidos contra
o painel anual já processado no projeto: contagens e taxas coincidem.

Arquivos: `serie_convergencia_china_eua_1990_2019.csv` e
`figura_convergencia_china_eua_1990_2019.{png,pdf}`.

## Pontos ideais de 2019

O arquivo local contém 90 votações nominais e 193 entradas de países em 2019.
Foram usadas 46 votações com pelo menos cinco votos `yes` e cinco votos `no`.
As abstenções são tratadas como ausentes no modelo. Cada país tem ao menos dois
votos binários nas votações usadas; Dominica tem dois e Guiné Equatorial, sete.

As posições são médias posteriores de um modelo espacial bayesiano
unidimensional (IRT 2PL), estimado por `pscl::ideal` em três cadeias de
60.000 iterações (15.000 iniciais descartadas, uma amostra a cada 30).
O sinal do eixo foi orientado para colocar os EUA à esquerda da China.
O maior R-hat entre os 193 países foi 1,0996. Os traços dos quatro países
identificados na figura são intervalos de credibilidade de 95%.

Os pontos representam posições relativas em **2019**. Seu valor numérico
depende da normalização, dos votos incluídos e do tratamento das abstenções;
este ajuste não produz uma série temporal de pontos ideais. Uma dimensão
resume apenas parte das diferenças entre países.

Arquivos: `pontos_ideais_2019.csv` e `figura_pontos_ideais_2019.{png,pdf}`.
O CSV preserva os códigos da fonte. Ao lê-lo com R, use
`read.csv("pontos_ideais_2019.csv", na.strings = "")`: o código `NA` é o da
Namíbia, e a leitura padrão o confundiria com um valor ausente. A fonte usa
o rótulo histórico `YU`/`Yugoslavia` em 2019; ele foi preservado no CSV.
