# Conferência docente do laboratório da Aula 7, após executar o script 14.
# Rodar a partir de 2026/lab-regressao-aula. Não é parte da atividade discente.
options(encoding = "UTF-8")
saida <- file.path("projeto_agna", "output", "aula_07", "laboratorio")
a <- readRDS(file.path(saida, "resultados_laboratorio_aula_07.rds"))
p <- a$parametros
d <- a$dados

stopifnot(nrow(d) == p$n, !anyNA(d), !anyDuplicated(d$id),
          all(vapply(d, is.numeric, logical(1))),
          max(abs(d$y - (p$alpha + p$beta * d$x + d$epsilon))) < 1e-10)
X <- cbind(1, d$x)
coef_matriz <- as.numeric(solve(crossprod(X), crossprod(X, d$y)))
stopifnot(abs(a$beta_observado - coef_matriz[2]) < 1e-10,
          max(abs(crossprod(X, d$e_hat))) < 1e-8,
          abs(mean(d$e_hat)) < 1e-10)

# Mesma amostra inicial do slide 3, quando os resultados dos slides existem.
arquivo_slides <- file.path("projeto_agna", "output", "aula_07", "resultados_aula_07.rds")
if (file.exists(arquivo_slides)) {
  s <- readRDS(arquivo_slides)$amostras[[1]]
  stopifnot(isTRUE(all.equal(d$x, s$x)), isTRUE(all.equal(d$y, s$y)))
}

# Checagens de Monte Carlo contra propriedades exatas do caso normal:
# E[beta_hat] = beta; Var(beta_hat) = sigma^2/[dp_X^2 * (n - 3)].
mc <- a$mc
stopifnot(nrow(mc) == 4 * p$R_mc, !anyNA(mc),
          !anyDuplicated(mc[c("cenario", "repeticao")]),
          all(is.finite(mc$beta_estimado)), all(is.finite(mc$alpha_estimado)))
checagens <- lapply(split(mc, mc$cenario), function(cenario) {
  stopifnot(nrow(cenario) == p$R_mc,
            length(unique(cenario$n)) == 1, length(unique(cenario$sigma)) == 1)
  n <- unique(cenario$n)
  sigma <- unique(cenario$sigma)
  beta <- cenario$beta_estimado
  ep_media_mc <- sd(beta) / sqrt(length(beta))
  variancia_teorica <- sigma^2 / (p$dp_x^2 * (n - 3))
  segundo_momento_mc <- mean((beta - p$beta)^2)
  ep_segundo_momento <- sd((beta - p$beta)^2) / sqrt(length(beta))
  stopifnot(abs(mean(beta) - p$beta) < 5 * ep_media_mc,
            abs(segundo_momento_mc - variancia_teorica) < 5 * ep_segundo_momento)
  data.frame(cenario = as.character(cenario$cenario[1]), n = n, sigma = sigma,
             media_mc = mean(beta), ep_media_mc = ep_media_mc,
             segundo_momento_mc = segundo_momento_mc,
             variancia_teorica = variancia_teorica)
})
checagens <- do.call(rbind, checagens)
row.names(checagens) <- NULL

# Confere as medidas da Tabela 1 e o denominador da variância empírica.
base <- mc[mc$n == p$n & mc$sigma == p$sigma, ]
valores <- setNames(a$tabelas$tabela_1$valor, a$tabelas$tabela_1$medida)
stopifnot(abs(valores[["Média das estimativas"]] - mean(base$beta_estimado)) < 1e-12,
          abs(valores[["Erro-padrão aproximado"]] - sd(base$beta_estimado)) < 1e-12,
          abs(valores[["EQM aproximado"]] -
              valores[["Variância aproximada"]] - valores[["Viés aproximado"]]^2) < 1e-12)

# Confere TODOS os sorteios bootstrap pela solução matricial, a partir dos
# pares originais selecionados pelos índices guardados. Detecta separação
# indevida de X e Y ou uma estimativa calculada em outra reamostra.
stopifnot(nrow(a$bootstrap) == p$B_boot, !anyNA(a$bootstrap),
          !anyDuplicated(a$bootstrap$reamostra), length(a$indices_boot) == p$B_boot)
erro_max_boot <- 0
for (b in seq_len(p$B_boot)) {
  indices <- a$indices_boot[[b]]
  stopifnot(length(indices) == p$n, all(indices %in% seq_len(p$n)))
  reamostra <- d[indices, ]
  Xb <- cbind(1, reamostra$x)
  beta_matriz <- as.numeric(solve(crossprod(Xb), crossprod(Xb, reamostra$y)))[2]
  erro_max_boot <- max(erro_max_boot, abs(beta_matriz - a$bootstrap$beta_estimado[b]))
}
stopifnot(erro_max_boot < 1e-10,
          anyDuplicated(a$indices_boot[[1]]) > 0,
          abs(a$tabelas$tabela_3$erro_padrao_aproximado[2] -
              sd(a$bootstrap$beta_estimado)) < 1e-12)

for (f in c("tabela_01_monte_carlo.csv", "tabela_02_cenarios.csv",
            "tabela_03_monte_carlo_bootstrap.csv", "figura_03_cenarios.pdf",
            "figura_04_bootstrap.pdf", "figuras_laboratorio_aula_07.rds")) {
  caminho <- file.path(saida, f)
  stopifnot(file.exists(caminho), file.info(caminho)$size > 0)
}
write.csv(checagens, file.path(saida, "validacao_monte_carlo.csv"), row.names = FALSE)
capture.output({
  cat("VALIDACAO_LAB_AULA7_OK\n")
  print(checagens)
  cat("\nMaior erro entre bootstrap e solução matricial:", erro_max_boot, "\n")
  print(a$tabelas$tabela_3)
  cat("\nSessão R:\n")
  print(sessionInfo())
}, file = file.path(saida, "validacao_laboratorio.txt"))
cat("VALIDACAO_LAB_AULA7_OK\n")
