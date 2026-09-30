# Covariáveis país-ano em escala original

Arquivo: `covariaveis_pais_ano_1997_2016.csv` (1.920 país-anos, 96 países, 1997–2016).
Gerado por `projeto_agna/scripts/11_extrair_covariaveis_originais.R` em 29/09/2026.

## Origem

O `synth_data.rds` do curso traz as covariáveis contínuas padronizadas por `arm::rescale()`, que subtrai a média e divide por dois desvios-padrão calculados nos 1.920 país-anos. As variáveis originais vêm do objeto `final_df` do pipeline `{targets}` do projeto de pesquisa "RDD Trade" (função `clean_synth_data()`). O script confere que a padronização dessas variáveis reproduz as colunas do painel do curso com erro menor que 10⁻⁸.

## Variáveis

| Variável | Definição | Fonte |
|---|---|---|
| `pais_iso3`, `ano` | chave país-ano | painel do curso |
| `continente` | África, Américas, Ásia e Oceania (Oceania: FJI e PNG), Europa | `countrycode` (continente) |
| `exportacoes_china_pct` | exportações do país para a China / exportações totais do país × 100 | ITPD-E, release 3 (USITC) |
| `exportacoes_eua_pct` | exportações do país para os EUA / exportações totais do país × 100 | ITPD-E, release 3 (USITC) |
| `hiato_poder_eua` | \|GPI dos EUA − GPI do país\|; vale 0 para os EUA | Global Power Index |
| `pib_per_capita_mil_usd` | PIB per capita, milhares de dólares correntes | Dynamic Gravity (USITC), via RDD Trade |
| `conta_corrente_pct_pib` | saldo em conta corrente, % do PIB | via RDD Trade |

## Amostra

Brasil e 95 países em que a China não foi o principal destino das exportações de bens em nenhum ano de 1997–2016 e com série completa. É o grupo de comparação de um desenho de controle sintético. Os EUA fazem parte da amostra.
