# Conferência docente da Aula 7: porcentagens por zona e previsão eleitoral.
# Usa identidades de MQO e cálculo por médias dentro de UF, independente de
# lm()/predict(), para conferir o ajuste e todas as 500 reamostras de pares.

library(here)
here::i_am("projeto_agna/scripts/19_validar_laboratorio_eleitoral.R")
pasta <- here::here("projeto_agna", "output", "aula_07", "laboratorio_eleitoral")
data_dir <- here::here("projeto_agna", "data", "eleicoes_2022_2026")
r <- readRDS(file.path(pasta, "resultados_laboratorio_eleitoral.rds"))
t <- r$treino
a <- r$aplicacao
stopifnot(nrow(t) == 2636L, nrow(a) == 2640L, nlevels(t$uf) == 27L,
          !anyNA(t), !anyNA(a), !anyNA(r$bootstrap),
          !anyDuplicated(t[c("uf", "zona")]), !anyDuplicated(a[c("uf", "zona")]),
          all(t$validos_t1 > 0), all(t$validos_t2 > 0), all(a$validos_t1 > 0),
          all(abs(t$pct_lula_t2 + t$pct_bolsonaro_t2 - 100) < 1e-10),
          all(abs(a$pct_lula_t2_prevista + a$pct_flavio_t2_prevista - 100) < 1e-10))

# Denominadores e agregação dos dois turnos de 2022, a partir das seções.
z <- read.csv(file.path(data_dir, "processed", "presidente_2022_zonas.csv"))
for (turno in c("t1", "t2")) {
  d <- arrow::read_parquet(file.path(data_dir, "raw", paste0(
    "section_votes_2022_", toupper(turno), ".parquet")))
  d <- d[d$uf != "ZZ", ]
  somas <- aggregate(d[c("c13", "c22", "validos")], d[c("uf", "zona")], sum)
  chave <- paste(z$uf, z$zona)
  idx <- match(chave, paste(somas$uf, somas$zona))
  stopifnot(!anyNA(idx),
    all(z[[paste0("lula_", turno)]] == somas$c13[idx]),
    all(z[[paste0("bolsonaro_", turno)]] == somas$c22[idx]),
    all(z[[paste0("validos_", turno)]] == somas$validos[idx]),
    max(abs(z[[paste0("pct_lula_", turno)]] -
      100 * somas$c13[idx] / somas$validos[idx])) < 1e-10)
}

# Totais de zona de 2026: BUs mais os complementos documentados de duas zonas.
z26 <- read.csv(file.path(data_dir, "processed", "presidente_2026_t1_zonas.csv"))
d26 <- arrow::read_parquet(file.path(data_dir, "raw", "section_votes_2026_T1_bu.parquet"))
d26 <- d26[d26$uf != "ZZ", ]
somas26 <- aggregate(d26[c("c13", "c22", "validos")], d26[c("uf", "zona")], sum)
complementos <- read.csv(file.path(data_dir, "docs", "complementos_totais_de_zona_2026.csv"))
for (i in seq_len(nrow(complementos))) {
  idx <- which(somas26$uf == complementos$uf[i] & somas26$zona == complementos$zona[i])
  for (coluna in c("c13", "c22", "validos")) {
    somas26[[coluna]][idx] <- somas26[[coluna]][idx] + complementos[[coluna]][i]
  }
}
idx <- match(paste(z26$uf, z26$zona), paste(somas26$uf, somas26$zona))
stopifnot(!anyNA(idx), all(z26$lula_t1 == somas26$c13[idx]),
          all(z26$flavio_t1 == somas26$c22[idx]), all(z26$validos_t1 == somas26$validos[idx]),
          sum(z26$secoes_completadas_por_total_municipal_t1) == 15L,
          sum(complementos$validos) == 3935,
          max(abs(a$pct_lula_t1 - 100 * z26$lula_t1 / z26$validos_t1)) < 1e-10)

# MQO com indicadoras de UF: beta por X e Y centrados dentro das UFs.
ajuste_por_medias <- function(d, futuro) {
  x <- d$pct_lula_t1
  y <- d$pct_lula_t2
  xc <- x - ave(x, d$uf, FUN = mean)
  yc <- y - ave(y, d$uf, FUN = mean)
  beta <- sum(xc * yc) / sum(xc^2)
  interceptos <- tapply(y - beta * x, d$uf, mean)
  pred <- unname(interceptos[as.character(futuro$uf)]) + beta * futuro$pct_lula_t1
  list(beta = beta, pred = pred)
}
indep <- ajuste_por_medias(t, a)
stopifnot(abs(indep$beta - coef(r$modelo_uf)[["pct_lula_t1"]]) < 1e-10,
          max(abs(indep$pred - a$pct_lula_t2_prevista)) < 1e-10)
X <- model.matrix(r$modelo_uf)
stopifnot(max(abs(crossprod(X, t$e_hat))) < 1e-6)

# Nacional como média ponderada das zonas e como média ponderada das UFs.
nacional <- sum(indep$pred * a$validos_t1) / sum(a$validos_t1)
u <- r$tabelas[[3]]
stopifnot(abs(nacional - r$tabelas[[4]]$pct_lula_t2_prevista[2]) < 1e-10,
          abs(nacional - sum(u$pct_lula_t2_prevista * u$total_validos_t1) /
                           sum(u$total_validos_t1)) < 1e-10,
          sum(u$zonas) == nrow(a), sum(u$total_validos_t1) == sum(a$validos_t1),
          abs(weighted.mean(c(10, 90), c(100, 900)) - 82) < 1e-10)

# Todos os sorteios preservam os números de zonas dentro de cada UF.
stopifnot(r$B_boot == 500L, ncol(r$indices_boot) == r$B_boot,
          nrow(r$indices_boot) == nrow(t),
          all(r$indices_boot >= 1L & r$indices_boot <= nrow(t)))
dif_beta <- dif_previsao <- numeric(r$B_boot)
for (b in seq_len(r$B_boot)) {
  idx <- r$indices_boot[, b]
  stopifnot(identical(table(t$uf[idx]), table(t$uf)))
  boot <- ajuste_por_medias(t[idx, ], a)
  dif_beta[b] <- abs(boot$beta - r$bootstrap$inclinacao[b])
  dif_previsao[b] <- abs(weighted.mean(boot$pred, a$validos_t1) -
                          r$bootstrap$pct_lula_t2_prevista[b])
}
stopifnot(max(dif_beta) < 1e-10, max(dif_previsao) < 1e-10,
          all(abs(r$bootstrap$pct_lula_t2_prevista +
                  r$bootstrap$pct_flavio_t2_prevista - 100) < 1e-10))
prev_fora <- a$pct_lula_t2_prevista < 0 | a$pct_lula_t2_prevista > 100
stopifnot(identical(prev_fora, a$fora_limites))
for (nome in c("figura_01_relacao_2022.pdf", "figura_02_bootstrap_previsoes.pdf",
               "previsoes_2026_zonas.csv", "bootstrap_previsoes.csv")) {
  stopifnot(file.exists(file.path(pasta, nome)), file.info(file.path(pasta, nome))$size > 0)
}
relatorio <- c("VALIDACAO_LAB_ELEITORAL_OK",
  "Unidades, ausências, duplicidades e denominadores conferidos.",
  "CSV de zonas igual à soma das seções, com os complementos oficiais documentados de 2026.",
  "Inclinação e previsões conferidas por centralização dentro de UF, sem lm()/predict().",
  "Equações normais e agregações ponderadas de zonas e UFs conferidas.",
  "500 sorteios: números de zonas por UF preservados, coeficientes e previsões conferidos.",
  paste("Maior diferença de inclinação bootstrap:", format(max(dif_beta), scientific = TRUE)),
  paste("Maior diferença de previsão bootstrap:", format(max(dif_previsao), scientific = TRUE)),
  paste("Previsões de zona fora de 0-100:", sum(prev_fora)),
  paste("Zonas 2026 fora da faixa de X de sua UF em 2022:", sum(a$fora_faixa_treino_uf)),
  paste("Previsão didática de Lula, 27 UFs, pesos 2026 T1:", format(nacional, digits = 10)),
  "Estes checks validam dados e cálculos. Não testam a estabilidade da relação entre eleições.")
writeLines(relatorio, file.path(pasta, "validacao_laboratorio_eleitoral.txt"))
writeLines(capture.output(sessionInfo()), file.path(pasta, "sessionInfo.txt"))
cat(paste(relatorio, collapse = "\n"), "\n")
