# População hipotética: uma linha por combinação das três variáveis binárias.
celulas <- expand.grid(T = 0:1, M = 0:1, C = 0:1)
celulas$prob <- with(celulas, ifelse(T == C, 3/16, 1/16))
celulas$Y <- with(celulas, 50 + 8*C + 8*M*C)

ajustar <- function(formula) lm(formula, data = celulas, weights = prob)
modelos <- list(aditivo = ajustar(Y ~ T*M + C),
                com_TC = ajustar(Y ~ T*M + C + T:C),
                com_MC = ajustar(Y ~ T*M + C + M:C))
interacoes <- vapply(modelos, function(m) unname(coef(m)["T:M"]), numeric(1))
r <- resid(ajustar(I(T*M) ~ T + M + C))
pi_MC <- with(celulas, sum(prob * r * M*C) / sum(prob * r^2))

stopifnot(sum(celulas$prob) == 1, max(abs(interacoes - c(4, 4, 0))) < 1e-10,
          abs(pi_MC - 0.5) < 1e-10)
