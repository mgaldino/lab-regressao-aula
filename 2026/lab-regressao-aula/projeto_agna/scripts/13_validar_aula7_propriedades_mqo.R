# Validação analítica dos resultados usados na Aula 7.
# Executar da raiz 2026/lab-regressao-aula após o script 12.
options(encoding = "UTF-8")
saida <- file.path("projeto_agna", "output", "aula_07")
a <- readRDS(file.path(saida, "resultados_aula_07.rds"))
p <- a$parametros
stopifnot(nrow(a$simulacoes) == nrow(a$cenarios) * p$repeticoes,
          !anyNA(a$simulacoes), !anyDuplicated(a$simulacoes[c("cenario", "repeticao")]),
          all(vapply(a$simulacoes[c("alpha", "beta")], is.numeric, logical(1))),
          all(is.finite(as.matrix(a$simulacoes[c("alpha", "beta")]))))

# As duas amostras: fórmula de MQO, equações normais, erro versus resíduo.
for (i in seq_along(a$amostras)) {
  d <- a$amostras[[i]]
  m <- lm(y ~ x, data = d)
  stopifnot(nrow(d) == 40, !anyNA(d), !anyDuplicated(d$id),
            isTRUE(all.equal(unname(coef(m)), unname(a$coef_amostras[i, ]))),
            max(abs(crossprod(model.matrix(m), resid(m)))) < 1e-8,
            max(abs(d$y - (p$alpha + p$beta * d$x + d$u))) < 1e-10)
}

# Para X normal e erros homocedásticos independentes:
# Var(beta_hat) = sigma^2 / [dp_X^2 * (n - 3)], pois Sxx/dp_X^2 ~ chi^2_(n-1).
# A validação considera o erro de Monte Carlo da média e do segundo momento.
checagens_mc <- lapply(seq_len(nrow(a$cenarios)), function(i) {
  c <- a$cenarios[i, ]
  d <- a$simulacoes[a$simulacoes$cenario == c$cenario, ]
  stopifnot(nrow(d) == p$repeticoes, all(d$n == c$n), all(d$alvo == c$alvo_blp))
  ep_media <- sd(d$beta) / sqrt(nrow(d))
  stopifnot(abs(mean(d$beta) - c$alvo_blp) < 5 * ep_media)
  segundo_momento <- mean((d$beta - c$alvo_blp)^2)
  variancia_teorica <- if (c$hetero) NA_real_ else
    c$sigma^2 / (p$dp_x^2 * (c$n - 3))
  if (!c$hetero) {
    ep_segundo <- sd((d$beta - c$alvo_blp)^2) / sqrt(nrow(d))
    stopifnot(abs(segundo_momento - variancia_teorica) < 5 * ep_segundo)
  }
  r <- a$resumo[a$resumo$cenario == c$cenario, ]
  stopifnot(abs(r$eqm_mc - (r$variancia_mc + r$vies_mc^2)) < 1e-12)
  data.frame(cenario = c$cenario, media_mc = mean(d$beta), alvo_blp = c$alvo_blp,
             ep_media_mc = ep_media, segundo_momento_mc = segundo_momento,
             variancia_teorica = variancia_teorica)
})
checagens_mc <- do.call(rbind, checagens_mc)

# Esperança e matriz de covariância condicionais, mantendo X fixa.
X <- model.matrix(~x, a$amostras[[1]])
cov_teorica <- p$sigma^2 * solve(crossprod(X))
medias <- rowMeans(a$condicionais)
ep_medias <- apply(a$condicionais, 1, sd) / sqrt(ncol(a$condicionais))
stopifnot(all(abs(medias - c(p$alpha, p$beta)) < 5 * ep_medias),
          max(abs(cov(t(a$condicionais)) - cov_teorica) / abs(cov_teorica)) < 0.1)

# Bootstrap: cada coluna contém n índices válidos da mesma amostra.
stopifnot(all(dim(a$indices_boot) == c(40L, p$bootstrap)),
          all(a$indices_boot >= 1L & a$indices_boot <= 40L),
          all(apply(a$indices_boot, 2, function(x) length(unique(x))) >= 2L),
          anyDuplicated(a$indices_boot[, 1]) > 0L,
          nrow(a$bootstrap) == p$bootstrap, !anyNA(a$bootstrap),
          !anyDuplicated(a$bootstrap$repeticao),
          abs(a$resumo_boot$ep_boot - sd(a$bootstrap$beta)) < 1e-12)
for (j in c(1L, 17L, p$bootstrap)) {
  d <- a$amostras[[1]][a$indices_boot[, j], ]
  stopifnot(isTRUE(all.equal(unname(coef(lm(y ~ x, data = d))),
                            unname(unlist(a$bootstrap[j, c("alpha", "beta")])))))
}

# Gauss--Markov: alternativa linear não viesada para uma matriz X fixa.
# C X = 0 e A = A_mqo + C. A diferença das covariâncias deve ser sigma^2 C C'.
A_mqo <- solve(crossprod(X), t(X))
M <- diag(nrow(X)) - X %*% A_mqo
set.seed(618311)
C <- matrix(rnorm(2 * nrow(X)), nrow = 2) %*% M / nrow(X)
A <- A_mqo + C
diferenca <- p$sigma^2 * (tcrossprod(A) - tcrossprod(A_mqo))
stopifnot(max(abs(A %*% X - diag(2))) < 1e-10,
          max(abs(C %*% X)) < 1e-10,
          max(abs(diferenca - p$sigma^2 * tcrossprod(C))) < 1e-10,
          min(eigen(diferenca, symmetric = TRUE)$values) > -1e-10)

# O estimador com shrinkage da média: viés não zero, porém tende a zero.
# Exemplo dos slides com Y_i iid, E[Y_i]=mu e Var[Y_i]=tau^2.
n <- c(5, 40, 640)
mu <- 5
tau2 <- 4
vies_shrinkage <- -mu / (n + 1)
eqm_shrinkage <- (n / (n + 1))^2 * tau2 / n + vies_shrinkage^2
stopifnot(all(diff(eqm_shrinkage) < 0))

figuras <- c("01_duas_amostras", "02_distribuicao_amostral", "03_tamanho_amostra",
             "04_ruido_heterocedasticidade", "05_parametro_e_blp", "06_bootstrap",
             "07_amostras_pequenas_n10_n40")
for (nome in figuras) {
  for (ext in c("pdf", "png")) {
    f <- file.path(saida, "figuras", paste0(nome, ".", ext))
    stopifnot(file.exists(f), file.info(f)$size > 1000)
  }
}
write.csv(checagens_mc, file.path(saida, "validacao_monte_carlo.csv"), row.names = FALSE)
capture.output({
  cat("VALIDACAO_AULA7_OK\n")
  print(checagens_mc)
  cat("\nCovariância condicional teórica:\n")
  print(cov_teorica)
  cat("\nCovariância condicional simulada:\n")
  print(cov(t(a$condicionais)))
  cat("\nGauss--Markov: autovalores da diferença de covariâncias:\n")
  print(eigen(diferenca, symmetric = TRUE)$values)
  cat("\nSessão R:\n")
  print(sessionInfo())
}, file = file.path(saida, "validacao.txt"))
cat("VALIDACAO_AULA7_OK\n")
