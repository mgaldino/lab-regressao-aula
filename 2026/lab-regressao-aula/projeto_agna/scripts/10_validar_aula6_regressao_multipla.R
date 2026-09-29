# Verificação técnica da Aula 6. Execute na pasta 2026/lab-regressao-aula.
# O roteiro projetável é 09_laboratorio_aula6_regressao_multipla.R.

source(here::here(
  "projeto_agna", "scripts",
  "09_laboratorio_aula6_regressao_multipla.R"
))

# Arquivo, chaves, tipos, valores faltantes e aritmética da taxa.
campos_modelo <- c(
  "taxa_convergencia_china", "n_pares_validos",
  "perc_trade_with_china", "perc_trade_with_us", "us_power_gap",
  "latin_america", "pci_cur", "CA_GDP"
)
stopifnot(
  nrow(painel) == 1920L,
  data.table::uniqueN(painel, by = c("pais_iso3", "ano")) == 1920L,
  data.table::uniqueN(painel$pais_iso3) == 96L,
  identical(sort(unique(painel$ano)), 1997:2016),
  is.character(painel$pais_iso3),
  is.integer(painel$ano),
  is.integer(painel$n_pares_validos),
  is.numeric(painel$taxa_convergencia_china),
  is.logical(painel$latin_america),
  nrow(dados_2016) == 96L,
  data.table::uniqueN(dados_2016$pais_iso3) == 96L,
  sum(dados_2016$latin_america) == 20L,
  all(dados_2016$n_votacoes == 114L),
  all(ausentes_2016[campos_modelo] == 0L),
  all(dados_2016$n_convergentes_china >= 0L),
  all(dados_2016$n_divergentes_china >= 0L),
  all(dados_2016$taxa_convergencia_china >= 0 &
        dados_2016$taxa_convergencia_china <= 1),
  max(abs(
    dados_2016$taxa_convergencia_china -
      dados_2016$n_convergentes_china / dados_2016$n_pares_validos
  )) < 1e-12
)
numericas_modelo <- setdiff(campos_modelo, "latin_america")
stopifnot(all(vapply(
  dados_2016[, ..numericas_modelo], function(x) all(is.finite(x)), logical(1)
)))

# A padronização deve ter sido feita no recorte de 2016.
padronizadas <- c(
  "z_trade_china", "z_trade_eua", "z_pares_validos",
  "z_hiato_poder", "z_pci", "z_ca"
)
stopifnot(
  max(abs(vapply(dados_2016[, ..padronizadas], mean, numeric(1)))) < 1e-12,
  max(abs(vapply(dados_2016[, ..padronizadas], sd, numeric(1)) - 1)) < 1e-12
)

# Exemplo do slide de variável omitida: mesma amostra de quatro unidades.
exemplo <- data.frame(x = c(0, 0, 1, 1), z = c(0, 1, 1, 2))
exemplo$y <- 1 + 2 * exemplo$x + 3 * exemplo$z
curto <- lm(y ~ x, data = exemplo)
completo <- lm(y ~ x + z, data = exemplo)
associacao_xz <- lm(z ~ x, data = exemplo)
stopifnot(
  isTRUE(all.equal(unname(coef(curto)[["x"]]), 5)),
  isTRUE(all.equal(unname(coef(completo)[["x"]]), 2)),
  isTRUE(all.equal(unname(coef(completo)[["z"]]), 3)),
  isTRUE(all.equal(unname(coef(associacao_xz)[["x"]]), 1)),
  isTRUE(all.equal(
    unname(coef(curto)[["x"]]),
    unname(coef(completo)[["x"]] +
             coef(completo)[["z"]] * coef(associacao_xz)[["x"]])
  ))
)

# Mesma amostra, solução matricial, equações normais e R².
modelos <- list(modelo_1, modelo_2, modelo_3, modelo_4)
stopifnot(all(vapply(modelos, nobs, integer(1)) == 96L))
for (modelo in modelos) {
  X <- model.matrix(modelo)
  y <- model.response(model.frame(modelo))
  b_matriz <- solve(crossprod(X), crossprod(X, y))
  r2_manual <- 1 - sum(residuals(modelo)^2) / sum((y - mean(y))^2)
  stopifnot(
    qr(X)$rank == ncol(X),
    max(abs(as.numeric(b_matriz) - unname(coef(modelo)))) < 1e-10,
    max(abs(crossprod(X, residuals(modelo)))) < 1e-10,
    isTRUE(all.equal(r2_manual, summary(modelo)$r.squared, tolerance = 1e-12))
  )
}
stopifnot(
  isTRUE(all.equal(
    unname(round(tabela_modelos$china_trade_pp_por_dp, 2)),
    c(0.80, 0.52, 1.29, 1.69)
  )),
  isTRUE(all.equal(
    unname(round(tabela_modelos$r2_amostral, 3)),
    c(0.004, 0.104, 0.250, 0.398)
  )),
  isTRUE(all.equal(
    unname(round(perfis$taxa_ajustada_percentual, 2)),
    c(60.45, 67.15)
  )),
  isTRUE(all.equal(
    diff(perfis$taxa_ajustada_percentual),
    100 * unname(coef(modelo_4)[["regiaoAmérica Latina"]]),
    tolerance = 1e-12
  )),
  length(unique(ggplot2::ggplot_build(figura_1)$data[[2]]$group)) == 1L
)

cat("VALIDACAO_AULA6_OK\n")
