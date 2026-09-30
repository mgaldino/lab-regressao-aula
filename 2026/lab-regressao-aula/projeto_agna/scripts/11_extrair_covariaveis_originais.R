# Covariáveis país-ano em escala original, para a Aula 6 em diante.
#
# Uso docente: roda uma vez e grava
# projeto_agna/data/processed/covariaveis_pais_ano_1997_2016.csv.
# Execute na pasta 2026/lab-regressao-aula:
#   LC_ALL=pt_BR.UTF-8 Rscript --vanilla \
#     projeto_agna/scripts/11_extrair_covariaveis_originais.R
#
# O synth_data.rds do curso guarda as covariáveis contínuas padronizadas por
# arm::rescale(): (x - média) / (2 desvios-padrão), calculadas nos 1.920
# país-anos. As variáveis originais estão no objeto `final_df` do pipeline
# {targets} do projeto de pesquisa "RDD Trade" (função clean_synth_data() em
# scripts/functions.R daquele projeto):
# - exportações do país para a China e para os EUA divididas pelas exportações
#   totais do país (ITPD-E, release 3, USITC);
# - hiato de poder: |GPI dos EUA - GPI do país|, com o Global Power Index;
# - PIB per capita em dólares correntes; conta corrente em % do PIB.
# O script confere que a padronização das variáveis originais reproduz, sem
# erro numérico, as colunas do painel do curso.

library(data.table)
library(dplyr)
library(here)

pasta_rdd_trade <- Sys.getenv(
  "RDD_TRADE_DIR",
  file.path("~", "Documents", "DCP", "Papers", "RDD Trade", "red_trade")
)
arquivo_final_df <- file.path(pasta_rdd_trade, "_targets", "objects", "final_df")
stopifnot(file.exists(arquivo_final_df))

final_df <- readRDS(arquivo_final_df)

painel <- data.table::fread(
  here::here("projeto_agna", "data", "processed", "painel_pais_ano_1997_2016.csv")
)

originais <- final_df |>
  dplyr::transmute(
    pais_iso3 = iso3c,
    ano = as.integer(year),
    exportacoes_china_pct = 100 * trade_with_china / total_trade,
    exportacoes_eua_pct = 100 * trade_with_us / total_trade,
    hiato_poder_eua = us_power_gap,
    pib_per_capita_mil_usd = gdp_cur / pop / 1000,
    conta_corrente_pct_pib = CA_GDP
  ) |>
  dplyr::distinct(pais_iso3, ano, .keep_all = TRUE)

covariaveis <- painel |>
  dplyr::select(
    pais_iso3, ano, perc_trade_with_china, perc_trade_with_us,
    us_power_gap, pci_cur, CA_GDP
  ) |>
  dplyr::inner_join(originais, by = c("pais_iso3", "ano"))

stopifnot(nrow(covariaveis) == nrow(painel), nrow(covariaveis) == 1920L)

# arm::rescale() aplicado às variáveis originais deve reproduzir o painel.
reescalar <- function(x) (x - mean(x)) / (2 * sd(x))
pares <- list(
  c("perc_trade_with_china", "exportacoes_china_pct"),
  c("perc_trade_with_us", "exportacoes_eua_pct"),
  c("us_power_gap", "hiato_poder_eua"),
  c("pci_cur", "pib_per_capita_mil_usd"),
  c("CA_GDP", "conta_corrente_pct_pib")
)
for (par in pares) {
  diferenca <- reescalar(covariaveis[[par[2]]]) - covariaveis[[par[1]]]
  stopifnot(max(abs(diferenca)) < 1e-8)
}

# Continente, com Ásia e Oceania juntas (Oceania tem dois países: FJI e PNG).
covariaveis$continente <- countrycode::countrycode(
  covariaveis$pais_iso3, "iso3c", "continent"
)
covariaveis$continente <- dplyr::recode(
  covariaveis$continente,
  "Africa" = "África",
  "Americas" = "Américas",
  "Asia" = "Ásia e Oceania",
  "Oceania" = "Ásia e Oceania",
  "Europe" = "Europa"
)
stopifnot(!anyNA(covariaveis$continente))

saida <- covariaveis |>
  dplyr::select(
    pais_iso3, ano, continente, exportacoes_china_pct, exportacoes_eua_pct,
    hiato_poder_eua, pib_per_capita_mil_usd, conta_corrente_pct_pib
  ) |>
  dplyr::arrange(pais_iso3, ano)

data.table::fwrite(
  saida,
  here::here("projeto_agna", "data", "processed", "covariaveis_pais_ano_1997_2016.csv")
)

cat("COVARIAVEIS_ORIGINAIS_OK:", nrow(saida), "país-anos\n")
