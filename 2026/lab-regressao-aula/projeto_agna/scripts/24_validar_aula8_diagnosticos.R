# Conferências analíticas da Aula 8; executar após o script 23.
options(encoding = "UTF-8")
saida <- file.path("projeto_agna", "output", "aula_08")
a <- readRDS(file.path(saida, "resultados_aula_08.rds"))
stopifnot(file.exists(a$entrada), unname(tools::md5sum(a$entrada)) == a$entrada_md5)

# Tipos, ausências, chaves, limites e reconstrução das regras geradoras.
for (nome in c("curva", "hetero", "binario")) {
  d <- a[[nome]]
  stopifnot(!anyNA(d), !anyDuplicated(d$id),
            all(vapply(d, is.numeric, logical(1))), all(is.finite(as.matrix(d))))
}
stopifnot(max(abs(a$curva$y - a$curva$cef - a$curva$u)) < 1e-12,
          max(abs(a$curva$cef - (2 + 1.5*a$curva$x - 0.8*(a$curva$x^2 - 4/3)))) < 1e-12,
          max(abs(a$hetero$y - 1 - 1.5*a$hetero$x - a$hetero$dp*a$hetero$z)) < 1e-12)

# MQO escalar e ortogonalidade; a relação com X^2 permanece nos resíduos lineares.
x <- a$curva$x
y <- a$curva$y
beta <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
stopifnot(abs(beta - a$coef_curva[2]) < 1e-12,
          abs(mean(a$curva$e_hat)) < 1e-12,
          abs(sum(x * a$curva$e_hat)) < 1e-9,
          abs(cor(x^2, a$curva$e_hat)) > 0.6)

# Sensibilidade dos três exemplos sintéticos contra reajustes independentes.
for (j in seq_len(nrow(a$influencia))) {
  nome <- a$influencia$cenario[j]
  d <- subset(a$dados_influencia, cenario == nome)
  completo <- lm(y ~ x, d)
  excluido <- lm(y ~ x, d[!d$destaque, ])
  stopifnot(abs(coef(completo)[2] - coef(excluido)[2] - a$influencia$delta_beta[j]) < 1e-12)
}
stopifnot(abs(a$influencia$delta_beta[1]) < 1e-10,
          abs(a$influencia$delta_beta[2]) < 1e-10,
          abs(a$influencia$delta_beta[3]) > 0.5,
          a$influencia$h[2] > 0.4)

# A ordem das linhas não altera MQO, nem elimina a dependência na ordem original.
t <- a$tempo
stopifnot(identical(sort(t$permutacao), seq_len(nrow(t))),
          isTRUE(all.equal(unname(coef(lm(y ~ x, t))),
                           unname(coef(lm(y ~ x, t[t$permutacao, ]))), tolerance = 1e-12)),
          cor(t$e_hat[-1], t$e_hat[-nrow(t)]) > 0.5)

# Resposta binária: suporte, probabilidades, tratamento e diferença de médias.
b <- a$binario
stopifnot(all(b$d %in% 0:1), all(b$y %in% 0:1), all(b$y0 %in% 0:1),
          all(b$y1 %in% 0:1), all(b$p0 > 0), all(b$p1 < 1),
          max(abs(b$p1 - b$p0 - a$parametros$tau)) < 1e-14,
          all(b$y == ifelse(b$d == 1, b$y1, b$y0)),
          max(abs(b$erro_cef - ifelse(b$y == 1, 1-b$p, -b$p))) < 1e-14,
          abs(a$coef_binario[2] - (mean(b$y[b$d == 1]) - mean(b$y[b$d == 0]))) < 1e-12)
# A diferença observada é uma realização, sem exigir igualdade ao ATE populacional.
ep_diferenca <- sqrt(var(b$y[b$d == 1])/sum(b$d == 1) + var(b$y[b$d == 0])/sum(b$d == 0))
stopifnot(abs(a$coef_binario[2] - a$parametros$tau) < 5*ep_diferenca)

# A média da distribuição amostral aproxima o parâmetro; tolerância pelo erro MC.
stopifnot(nrow(a$mc) == 4000L, !anyNA(a$mc), !anyDuplicated(a$mc$repeticao),
          abs(mean(a$mc$beta) - 1.5) < 5 * sd(a$mc$beta)/sqrt(nrow(a$mc)))
v <- a$variancia_residuos
# Erros normais com sigma^2=1: erro-padrão MC da variância é v*sqrt(2/(B-1)).
stopifnot(all(abs(v$variancia_mc - v$variancia_teorica) <
                5*v$variancia_teorica*sqrt(2/19999)),
          v$h[30] > 0.7, v$variancia_mc[30] < 0.3)

# Dados observados: chaves, denominadores, cobertura, modelos comparáveis.
d <- a$brasil
stopifnot(nrow(a$eleicao) == 2637L, nrow(d) == 2636L, nlevels(d$uf) == 27L,
          !anyNA(d), !anyDuplicated(d[c("uf", "zona")]), !any(d$uf == "ZZ"),
          all(d$validos_t1 > 0), all(d$validos_t2 > 0),
          max(abs(100*d$lula_t1/d$validos_t1 - d$pct_lula_t1)) < 1e-9,
          max(abs(100*d$lula_t2/d$validos_t2 - d$pct_lula_t2)) < 1e-9,
          all(d$pct_lula_t1 >= 0 & d$pct_lula_t1 <= 100),
          all(d$pct_lula_t2 >= 0 & d$pct_lula_t2 <= 100),
          abs(a$exterior$h - 1) < 1e-12, abs(a$exterior$residuo) < 1e-10)

# Inclinação comum por centralização dentro de UF, sem lm() ou predict().
xc <- d$pct_lula_t1 - ave(d$pct_lula_t1, d$uf)
yc <- d$pct_lula_t2 - ave(d$pct_lula_t2, d$uf)
beta_uf <- sum(xc*yc)/sum(xc^2)
ajustado_uf <- ave(d$pct_lula_t2, d$uf) + beta_uf*xc
diag_uf <- a$diagnosticos[as.character(a$diagnosticos$modelo) == "Primeiro turno + UF", ]
stopifnot(abs(beta_uf - a$coef_uf["pct_lula_t1"]) < 1e-12,
          max(abs(ajustado_uf - diag_uf$ajustado)) < 1e-9,
          all(diag_uf$h >= 0 & diag_uf$h < 1))

# Fórmula de exclusão de uma observação contra reajustes reais.
s <- a$sensibilidade
for (j in unique(c(1:5, 100L, nrow(s)))) {
  linha <- which(d$uf == s$uf[j] & d$zona == s$zona[j])
  m_sem <- lm(pct_lula_t2 ~ pct_lula_t1 + uf, d[-linha, ])
  stopifnot(abs(unname(coef(m_sem)["pct_lula_t1"]) - s$beta_sem[j]) < 1e-10)
}

# Treino/teste disjuntos, níveis geográficos cobertos e previsão por álgebra matricial.
idx <- a$indices_teste
stopifnot(!anyDuplicated(idx), all(idx %in% seq_len(nrow(d))),
          all(table(d$uf[idx]) > 0), all(table(d$uf[-idx]) > 0))
treino <- d[-idx, ]
teste <- d[idx, ]
formulas <- list(~pct_lula_t1, ~pct_lula_t1 + uf, ~pct_lula_t1 + I(pct_lula_t1^2) + uf)
for (j in seq_along(formulas)) {
  X <- model.matrix(formulas[[j]], treino)
  Xnovo <- model.matrix(formulas[[j]], teste)
  coef_matriz <- solve(crossprod(X), crossprod(X, treino$pct_lula_t2))
  pred <- as.numeric(Xnovo %*% coef_matriz)
  erro <- teste$pct_lula_t2 - pred
  stopifnot(abs(sqrt(mean(erro^2)) - a$avaliacao$rmse_teste[j]) < 1e-8,
            abs(mean(abs(erro)) - a$avaliacao$mae_teste[j]) < 1e-8,
            a$avaliacao$n_treino[j] + a$avaliacao$n_teste[j] == nrow(d))
}

figuras <- c("01_curvatura", "02_residuos_curvatura", "03_heterocedasticidade",
             "04_alavancagem_influencia", "05_dependencia", "06_resposta_binaria",
             "07_erros_e_estimador", "08_eleitoral_residuos", "09_eleitoral_dispersao")
for (nome in figuras) for (ext in c("png", "pdf")) {
  f <- file.path(saida, "figuras", paste0(nome, ".", ext))
  stopifnot(file.exists(f), file.info(f)$size > 1000)
}
capture.output({
  cat("VALIDACAO_AULA8_OK\n")
  cat("Inclinação eleitoral, cálculo independente dentro de UF:", beta_uf, "\n")
  cat("Diferença binária observada:", a$coef_binario[2], "; ATE populacional:", a$parametros$tau, "\n")
  cat("Média MC da inclinação:", mean(a$mc$beta), "; parâmetro: 1.5\n")
  print(a$influencia)
  print(a$avaliacao)
  print(a$exterior)
  print(sessionInfo())
}, file = file.path(saida, "validacao.txt"))
cat("VALIDACAO_AULA8_OK\n")
