# Verificação técnica da Aula 6 (uso docente).
# Execute na pasta 2026/lab-regressao-aula:
#   LC_ALL=pt_BR.UTF-8 Rscript --vanilla \
#     projeto_agna/scripts/10_validar_aula6_regressao_multipla.R
# O roteiro dos alunos é 09_laboratorio_aula6_regressao_multipla.R;
# source() não imprime os gráficos, então nenhum Rplots.pdf é criado.

source(here::here(
  "projeto_agna", "scripts",
  "09_laboratorio_aula6_regressao_multipla.R"
))

# Base de 2016 --------------------------------------------------------------

stopifnot(
  nrow(covariaveis) == 1920L,
  data.table::uniqueN(covariaveis, by = c("pais_iso3", "ano")) == 1920L,
  nrow(paises_2016) == 96L,
  !anyDuplicated(paises_2016$pais_iso3),
  !anyNA(paises_2016$continente),
  identical(as.integer(table(paises_2016$continente)), c(24L, 22L, 20L, 30L)),
  all(paises_2016$convergencia >= 0 & paises_2016$convergencia <= 100),
  all(paises_2016$exportacoes_china_pct >= 0 & paises_2016$exportacoes_china_pct <= 100),
  paises_2016$pais_iso3[which.max(paises_2016$exportacoes_china_pct)] == "BRA",
  paises_2016$hiato_poder_eua[paises_2016$pais_iso3 == "USA"] == 0
)

# As covariáveis originais, padronizadas por arm::rescale(), reproduzem o
# painel do curso.
reescalar <- function(x) (x - mean(x)) / (2 * sd(x))
conferencia <- dplyr::inner_join(
  painel_votos, covariaveis, by = c("pais_iso3", "ano")
)
stopifnot(
  nrow(conferencia) == 1920L,
  max(abs(reescalar(conferencia$exportacoes_china_pct) -
            conferencia$perc_trade_with_china)) < 1e-8,
  max(abs(reescalar(conferencia$exportacoes_eua_pct) -
            conferencia$perc_trade_with_us)) < 1e-8,
  max(abs(reescalar(conferencia$hiato_poder_eua) -
            conferencia$us_power_gap)) < 1e-8,
  max(abs(reescalar(conferencia$pib_per_capita_mil_usd) -
            conferencia$pci_cur)) < 1e-8,
  max(abs(reescalar(conferencia$conta_corrente_pct_pib) -
            conferencia$CA_GDP)) < 1e-8
)

# Continente ------------------------------------------------------------------

coef_continente <- unname(coef(modelo_continente))
stopifnot(
  isTRUE(all.equal(coef_continente[1], tabela_1$convergencia_media[1])),
  isTRUE(all.equal(
    coef_continente[2:4],
    tabela_1$convergencia_media[2:4] - tabela_1$convergencia_media[1]
  )),
  isTRUE(all.equal(fitted(modelo_continente), fitted(modelo_americas))),
  sum(is.na(coef(modelo_armadilha))) == 1L
)

# Regressões curta, longa e auxiliar ------------------------------------------

stopifnot(
  abs(coef_curto - (coef_longo + coef_europa * inclinacao_auxiliar)) < 1e-10,
  coef_curto > 0,
  coef_longo < 0,
  coef_europa < 0,
  inclinacao_auxiliar < 0
)

# Decomposição do coeficiente --------------------------------------------------

stopifnot(
  abs(coef(modelo_residuo)[["residuo_exportacoes"]] -
        coef(modelo_m2)[["exportacoes_china_pct"]]) < 1e-10
)

# Solução matricial e equações normais nos quatro modelos ---------------------

for (modelo in list(modelo_m1, modelo_m2, modelo_m3, modelo_m4)) {
  X <- model.matrix(modelo)
  y <- model.response(model.frame(modelo))
  stopifnot(
    nobs(modelo) == 96L,
    qr(X)$rank == ncol(X),
    max(abs(solve(crossprod(X), crossprod(X, y)) - coef(modelo))) < 1e-8,
    max(abs(crossprod(X, residuals(modelo)))) < 1e-8
  )
}
stopifnot(identical(tabela_3$modelo, c("M1", "M2", "M3", "M4")))

# Números dos slides -----------------------------------------------------------

# Exemplo de quatro unidades.
exemplo <- data.frame(x1 = c(0, 0, 1, 1), x2 = c(0, 1, 1, 2))
exemplo$y <- 1 + 2 * exemplo$x1 + 3 * exemplo$x2
residuo_exemplo <- as.numeric(residuals(lm(x1 ~ x2, data = exemplo)))
stopifnot(
  isTRUE(all.equal(residuo_exemplo, c(0, -0.5, 0.5, 0))),
  isTRUE(all.equal(sum(residuo_exemplo * exemplo$y) / sum(residuo_exemplo^2), 2)),
  isTRUE(all.equal(unname(coef(lm(y ~ x1, data = exemplo))[2]), 5)),
  isTRUE(all.equal(unname(coef(lm(x2 ~ x1, data = exemplo))[2]), 1))
)

# Brasil e China por resolução: ano e conflito palestino.
resolucoes <- data.table::fread(here::here(
  "projeto_agna", "data", "processed", "brasil_convergencia_china_1997_2016.csv"
))
resolucoes$convergencia <- 100 * resolucoes$convergente
resolucoes$anos_desde_1997 <- resolucoes$ano - 1997
resolucoes$palestina <- as.integer(grepl("Palestinian conflict", resolucoes$tema))
gamma_1 <- coef(lm(convergencia ~ anos_desde_1997, data = resolucoes))[[2]]
longa <- coef(lm(convergencia ~ anos_desde_1997 + palestina, data = resolucoes))
delta <- coef(lm(palestina ~ anos_desde_1997, data = resolucoes))[[2]]
stopifnot(
  nrow(resolucoes) == 1762L,
  abs(gamma_1 - (longa[[2]] + longa[[3]] * delta)) < 1e-10,
  round(gamma_1, 2) == 0.31,
  round(longa[[2]], 2) == 0.45,
  round(longa[[3]], 1) == 23.2,
  round(delta, 4) == -0.0060
)

cat("VALIDACAO_AULA6_OK\n")
