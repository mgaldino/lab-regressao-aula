# Aula 8: dados, cálculos e figuras dos slides. Executar da raiz do projeto R.
# LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/23_preparar_aula8_diagnosticos.R
# Fontes conceituais: Hansen, Econometrics, 3.19--3.21, 4.10--4.16;
# Shalizi, The Truth about Linear Regression, 6.1 e 6.5;
# Galdino, Regressão, capítulo Checagem. Consulta: 20/10/2026.
# Dados observados: snapshot TSE preparado e documentado para a Aula 7.
# Os demais exemplos são sintéticos, com processos e sementes abaixo.

options(encoding = "UTF-8", scipen = 999)
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(patchwork)
})
stopifnot(file.exists("lab-regressao-aula-2026.Rproj"))
saida <- file.path("projeto_agna", "output", "aula_08")
figuras <- file.path(saida, "figuras")
dir.create(figuras, recursive = TRUE, showWarnings = FALSE)

azul <- "#1D4ED8"
verde <- "#047857"
laranja <- "#B45309"
cinza <- "#334155"
tema <- theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 13, color = "#0F172A"),
        axis.title = element_text(size = 13), plot.title = element_text(size = 14),
        legend.position = "bottom", legend.title = element_blank(),
        plot.margin = margin(5, 10, 5, 5))
exportar <- function(g, nome, largura = 9, altura = 3.25) {
  ggsave(file.path(figuras, paste0(nome, ".pdf")), g, width = largura,
         height = altura, units = "in", device = grDevices::pdf,
         useDingbats = FALSE, bg = "white")
  ggsave(file.path(figuras, paste0(nome, ".png")), g, width = largura,
         height = altura, units = "in", dpi = 200, bg = "white")
}

# 1. CEF quadrática; a centralização e a simetria de X preservam o BLP 2 + 1,5 X.
# O modelo linear simples é uma aproximação; adicionar X^2 recupera a CEF.
set.seed(618320)
curva <- data.frame(id = 1:450, x = runif(450, -2, 2), u = rnorm(450, sd = 0.6))
curva$cef <- 2 + 1.5 * curva$x - 0.8 * (curva$x^2 - 4/3)
curva$y <- curva$cef + curva$u
m_curva <- lm(y ~ x, curva)
m_quadratico <- lm(y ~ x + I(x^2), curva)
curva$e_hat <- resid(m_curva)
curva$ajustado <- fitted(m_curva)
grade <- data.frame(x = seq(-2, 2, length.out = 250))
grade$cef <- 2 + 1.5 * grade$x - 0.8 * (grade$x^2 - 4/3)
grade$linear <- predict(m_curva, grade)
grade$quadratico <- predict(m_quadratico, grade)
g_curva <- ggplot(curva, aes(x, y)) +
  geom_point(alpha = 0.25, size = 1.2, color = cinza) +
  geom_line(data = grade, aes(y = cef, color = "CEF conhecida"), linewidth = 1.1) +
  geom_line(data = grade, aes(y = linear, color = "MQO: Y em X"), linewidth = 1) +
  scale_color_manual(values = c("CEF conhecida" = verde, "MQO: Y em X" = azul)) +
  labs(x = "X (unidades)", y = "Y (unidades)") + tema
exportar(g_curva, "01_curvatura")
g_res_curva <- ggplot(curva, aes(x, e_hat)) +
  geom_hline(yintercept = 0, color = "grey60", linetype = "dotted") +
  geom_point(alpha = 0.3, size = 1.2, color = cinza) +
  geom_smooth(aes(color = "Reta nos resíduos"), method = "lm", formula = y ~ x,
              se = FALSE, linewidth = 1) +
  geom_smooth(aes(color = "Suavização flexível"), method = "loess", formula = y ~ x,
              span = 0.8, se = FALSE, linewidth = 1.1) +
  scale_color_manual(values = c("Reta nos resíduos" = azul,
                               "Suavização flexível" = verde)) +
  labs(x = "X (unidades)", y = "Resíduo (unidades de Y)") + tema
exportar(g_res_curva, "02_residuos_curvatura")

# 2. Média linear correta, variância dependente de X. Não confundir com curvatura.
set.seed(618321)
hetero <- data.frame(id = 1:600, x = runif(600, 0, 4), z = rnorm(600))
hetero$dp <- 0.25 + 0.5 * hetero$x
hetero$u <- hetero$dp * hetero$z
hetero$y <- 1 + 1.5 * hetero$x + hetero$u
m_hetero <- lm(y ~ x, hetero)
hetero$e_hat <- resid(m_hetero)
g_hetero <- ggplot(hetero, aes(x, e_hat)) +
  geom_hline(yintercept = 0, color = azul, linewidth = 0.7) +
  geom_point(alpha = 0.35, size = 1.3, color = cinza) +
  geom_smooth(method = "loess", formula = y ~ x, span = 0.9,
              se = FALSE, color = verde, linewidth = 1) +
  labs(x = "X (unidades)", y = "Resíduo (unidades de Y)") + tema
exportar(g_hetero, "03_heterocedasticidade")

# 3. Mesmos 35 pontos de base, mais uma observação de cada tipo.
# A sensibilidade usa mudança no coeficiente, sem limiar universal de exclusão.
set.seed(618322)
base_influencia <- data.frame(x = seq(-1.5, 1.5, length.out = 35),
                             u = rnorm(35, sd = 0.35))
base_influencia$y <- 1 + 1.5 * base_influencia$x + base_influencia$u
m_base <- lm(y ~ x, base_influencia)
tipos <- data.frame(cenario = c("Resíduo grande", "Alavancagem alta", "Ambos"),
                   x = c(0, 5, 5), y = c(7, predict(m_base, data.frame(x = 5)), 1))
dados_influencia <- list()
retas_influencia <- list()
influencia <- list()
for (j in seq_len(nrow(tipos))) {
  novo <- tipos[j, ]
  d <- rbind(base_influencia[c("x", "y")], novo[c("x", "y")])
  m <- lm(y ~ x, d)
  dados_influencia[[j]] <- data.frame(d, cenario = novo$cenario,
                                    destaque = seq_len(nrow(d)) == nrow(d))
  gx <- data.frame(x = seq(-1.6, 5.2, length.out = 100))
  retas_influencia[[j]] <- rbind(
    data.frame(gx, y = predict(m_base, gx), ajuste = "Sem o ponto", cenario = novo$cenario),
    data.frame(gx, y = predict(m, gx), ajuste = "Com o ponto", cenario = novo$cenario))
  influencia[[j]] <- data.frame(cenario = novo$cenario, h = tail(hatvalues(m), 1),
    residuo = tail(resid(m), 1), beta_sem = unname(coef(m_base)[2]),
    beta_com = unname(coef(m)[2]),
    delta_beta = unname(coef(m)[2] - coef(m_base)[2]))
}
influencia <- bind_rows(influencia)
dados_influencia <- bind_rows(dados_influencia)
retas_influencia <- bind_rows(retas_influencia)
ordem <- tipos$cenario
dados_influencia$cenario <- factor(dados_influencia$cenario, levels = ordem)
retas_influencia$cenario <- factor(retas_influencia$cenario, levels = ordem)
g_influencia <- ggplot(dados_influencia, aes(x, y)) +
  geom_point(data = subset(dados_influencia, !destaque), color = cinza, size = 1.5) +
  geom_point(data = subset(dados_influencia, destaque), color = laranja,
             shape = 17, size = 3.2) +
  geom_line(data = retas_influencia, aes(color = ajuste, linetype = ajuste), linewidth = 0.9) +
  facet_wrap(~cenario, nrow = 1) +
  scale_color_manual(values = c("Sem o ponto" = verde, "Com o ponto" = azul)) +
  scale_linetype_manual(values = c("Sem o ponto" = "dashed", "Com o ponto" = "solid")) +
  labs(x = "X (unidades)", y = "Y (unidades)") + tema
exportar(g_influencia, "04_alavancagem_influencia", altura = 3.4)

# 4. Erros serialmente correlacionados; reordenar linhas não altera o ajuste.
set.seed(618323)
tempo <- data.frame(t = 1:220, x = rnorm(220))
tempo$u <- as.numeric(arima.sim(model = list(ar = 0.8), n = 220))
tempo$y <- 2 + 1.5 * tempo$x + tempo$u
m_tempo <- lm(y ~ x, tempo)
tempo$e_hat <- resid(m_tempo)
tempo$permutacao <- sample.int(nrow(tempo))
tempo_fig <- bind_rows(
  data.frame(ordem = tempo$t, residuo = tempo$e_hat, painel = "Ordem temporal"),
  data.frame(ordem = tempo$t, residuo = tempo$e_hat[tempo$permutacao],
             painel = "Mesmos resíduos em ordem aleatória"))
tempo_fig$painel <- factor(tempo_fig$painel,
  levels = c("Ordem temporal", "Mesmos resíduos em ordem aleatória"))
g_tempo <- ggplot(tempo_fig, aes(ordem, residuo)) +
  geom_hline(yintercept = 0, color = "grey65") +
  geom_line(color = azul, linewidth = 0.6) + facet_wrap(~painel, nrow = 1) +
  labs(x = "Posição na sequência", y = "Resíduo (unidades de Y)") + tema
exportar(g_tempo, "05_dependencia")

# 5. Experimento sintético com CEF não linear em X e efeito constante de 0,12.
# D é independente de X e dos dois resultados potenciais. Um U comum constrói
# Y(0), Y(1); isso implica monotonicidade neste exemplo, desnecessária ao ATE.
set.seed(618324)
binario <- data.frame(id = 1:3000, x = runif(3000, -2, 2),
                     d = rbinom(3000, 1, 0.5), u = runif(3000))
tau <- 0.12
binario$p0 <- 0.15 + 0.6 * plogis(1.5 * binario$x)
binario$p1 <- binario$p0 + tau
binario$y0 <- as.integer(binario$u < binario$p0)
binario$y1 <- as.integer(binario$u < binario$p1)
binario$y <- ifelse(binario$d == 1, binario$y1, binario$y0)
binario$p <- ifelse(binario$d == 1, binario$p1, binario$p0)
binario$erro_cef <- binario$y - binario$p
m_binario <- lm(y ~ d, binario)
medias_binarias <- binario |>
  group_by(d) |> summarise(n = n(), media = mean(y), .groups = "drop")
g_binario <- ggplot(binario, aes(p, erro_cef)) +
  geom_hline(yintercept = 0, color = "grey65") +
  geom_point(alpha = 0.25, size = 1.1, color = azul) +
  labs(x = "Probabilidade verdadeira p(D,X)", y = "Erro da CEF: Y - p(D,X)") + tema
exportar(g_binario, "06_resposta_binaria")

# 6. Erros assimétricos com média zero, independentes de X: dois objetos distintos.
# A distribuição amostral é aproximada por 4.000 novas amostras de n=160.
set.seed(618325)
erros_nao_normais <- rexp(3000) - 1
repeticoes <- 4000L
mc <- data.frame(repeticao = seq_len(repeticoes), beta = NA_real_)
for (b in seq_len(repeticoes)) {
  x <- rnorm(160)
  u <- rexp(160) - 1
  y <- 2 + 1.5 * x + u
  mc$beta[b] <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
}
g_erros <- ggplot(data.frame(u = erros_nao_normais), aes(u)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = azul, color = "white") +
  geom_vline(xintercept = 0, color = verde, linetype = "dashed", linewidth = 0.9) +
  labs(title = "Erros: exponencial menos 1", x = "Erro (unidades de Y)", y = "Densidade") + tema
g_mc <- ggplot(mc, aes(beta)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = azul, color = "white") +
  geom_vline(xintercept = 1.5, color = verde, linetype = "dashed", linewidth = 0.9) +
  labs(title = "Inclinação: 4.000 amostras, n = 160", x = "Inclinação estimada", y = "Densidade") + tema
exportar(g_erros + g_mc, "07_erros_e_estimador", altura = 3.5)

# 7. Aplicação observada: porcentagem de Lula por zona nos dois turnos de 2022.
entrada <- file.path("projeto_agna", "data", "eleicoes_2022_2026", "processed",
                     "presidente_2022_zonas_com_exterior.csv")
eleicao <- read.csv(entrada, encoding = "UTF-8")
necessarias <- c("uf", "zona", "lula_t1", "lula_t2", "validos_t1", "validos_t2",
                 "pct_lula_t1", "pct_lula_t2")
stopifnot(all(necessarias %in% names(eleicao)), !anyNA(eleicao[necessarias]),
          !anyDuplicated(eleicao[c("uf", "zona")]),
          all(eleicao$validos_t1 > 0), all(eleicao$validos_t2 > 0),
          all(eleicao$lula_t1 >= 0 & eleicao$lula_t1 <= eleicao$validos_t1),
          all(eleicao$lula_t2 >= 0 & eleicao$lula_t2 <= eleicao$validos_t2),
          max(abs(eleicao$pct_lula_t1 - 100 * eleicao$lula_t1 / eleicao$validos_t1)) < 1e-9,
          max(abs(eleicao$pct_lula_t2 - 100 * eleicao$lula_t2 / eleicao$validos_t2)) < 1e-9,
          sum(eleicao$uf == "ZZ") == 1L)
eleicao$uf <- factor(eleicao$uf)
modelo_com_zz <- lm(pct_lula_t2 ~ pct_lula_t1 + uf, eleicao)
idx_zz <- which(eleicao$uf == "ZZ")
exterior <- data.frame(n = 1L, h = hatvalues(modelo_com_zz)[idx_zz],
                       residuo = resid(modelo_com_zz)[idx_zz],
                       observado = eleicao$pct_lula_t2[idx_zz],
                       ajustado = fitted(modelo_com_zz)[idx_zz])

# Os gráficos comparáveis usam somente as 27 UFs: ZZ é discutida separadamente.
brasil <- droplevels(eleicao[eleicao$uf != "ZZ", ])
modelos <- list(
  simples = lm(pct_lula_t2 ~ pct_lula_t1, brasil),
  uf = lm(pct_lula_t2 ~ pct_lula_t1 + uf, brasil),
  quadratico = lm(pct_lula_t2 ~ pct_lula_t1 + I(pct_lula_t1^2) + uf, brasil))
nomes_modelos <- c(simples = "Primeiro turno", uf = "Primeiro turno + UF",
                   quadratico = "Primeiro turno + quadrado + UF")
diagnosticos <- bind_rows(lapply(c("simples", "uf"), function(nome) {
  m <- modelos[[nome]]
  data.frame(uf = brasil$uf, zona = brasil$zona, x = brasil$pct_lula_t1,
             observado = brasil$pct_lula_t2, ajustado = fitted(m),
             residuo = resid(m), h = hatvalues(m), modelo = nomes_modelos[nome])
}))
diagnosticos$modelo <- factor(diagnosticos$modelo, levels = nomes_modelos[c("simples", "uf")])
g_eleitoral <- ggplot(diagnosticos, aes(ajustado, residuo)) +
  geom_hline(yintercept = 0, color = azul, linewidth = 0.6) +
  geom_point(alpha = 0.18, size = 0.9, color = cinza) +
  geom_smooth(method = "loess", formula = y ~ x, span = 0.75,
              se = FALSE, color = verde, linewidth = 1) +
  facet_wrap(~modelo, nrow = 1) +
  labs(x = "Lula no segundo turno: valor ajustado (%)", y = "Resíduo (p.p.)") + tema
exportar(g_eleitoral, "08_eleitoral_residuos", altura = 3.4)
g_dispersao <- ggplot(diagnosticos, aes(ajustado, abs(residuo))) +
  geom_point(alpha = 0.18, size = 0.9, color = cinza) +
  geom_smooth(method = "loess", formula = y ~ x, span = 0.75,
              se = FALSE, color = verde, linewidth = 1) +
  facet_wrap(~modelo, nrow = 1) +
  labs(x = "Lula no segundo turno: valor ajustado (%)", y = "Valor absoluto do resíduo (p.p.)") + tema
exportar(g_dispersao, "09_eleitoral_dispersao", altura = 3.4)

# Exclusão de uma zona de cada vez: fórmula analítica, confirmada por reajuste no validador.
m_uf <- modelos$uf
X <- model.matrix(m_uf)
h <- hatvalues(m_uf)
e <- resid(m_uf)
inv <- solve(crossprod(X))
j_beta <- which(colnames(X) == "pct_lula_t1")
delta_beta <- as.numeric(X %*% inv[, j_beta]) * e / (1 - h)
sensibilidade <- data.frame(uf = brasil$uf, zona = brasil$zona, h = h,
  residuo = e, delta_beta = delta_beta,
  beta_sem = unname(coef(m_uf)[j_beta]) - delta_beta)
sensibilidade <- sensibilidade[order(abs(sensibilidade$delta_beta), decreasing = TRUE), ]

# Avaliação preditiva: divisão única, previamente fixada, estratificada por UF.
# O teste é de novas zonas da MESMA eleição; não verifica transporte para 2026.
set.seed(618326)
indices_teste <- sort(unlist(lapply(split(seq_len(nrow(brasil)), brasil$uf), function(ii) {
  sample(ii, max(1L, floor(0.2 * length(ii))))
})))
treino <- brasil[-indices_teste, ]
teste <- brasil[indices_teste, ]
avaliacao <- bind_rows(lapply(names(modelos), function(nome) {
  m <- lm(formula(modelos[[nome]]), treino)
  prev_treino <- predict(m, treino)
  prev_teste <- predict(m, teste)
  data.frame(modelo = nomes_modelos[nome], n_treino = nrow(treino), n_teste = nrow(teste),
    rmse_treino = sqrt(mean((treino$pct_lula_t2 - prev_treino)^2)),
    rmse_teste = sqrt(mean((teste$pct_lula_t2 - prev_teste)^2)),
    mae_teste = mean(abs(teste$pct_lula_t2 - prev_teste)),
    minimo_teste = min(prev_teste), maximo_teste = max(prev_teste))
}))

# Exemplo complementar da expressão Var(e_hat_i|X) = (1-h_ii)*sigma^2.
# 20.000 novas respostas com a mesma X: não supõe que resíduos sejam i.i.d.
set.seed(618327)
fixo_x <- c(seq(-1, 1, length.out = 29), 6)
X_fixo <- cbind(1, fixo_x)
A_fixo <- solve(crossprod(X_fixo), t(X_fixo))
H_fixo <- X_fixo %*% A_fixo
M_fixo <- diag(length(fixo_x)) - H_fixo
eps_fixo <- matrix(rnorm(30 * 20000), nrow = 30)
residuos_fixos <- M_fixo %*% eps_fixo
variancia_residuos <- data.frame(id = 1:30, x = fixo_x, h = diag(H_fixo),
  variancia_teorica = 1 - diag(H_fixo), variancia_mc = apply(residuos_fixos, 1, var))

resultados <- list(
  parametros = list(data = "2026-10-20", sementes = 618320:618327, tau = tau,
                    repeticoes_mc = repeticoes, n_mc = 160L),
  curva = curva, coef_curva = coef(m_curva), coef_quadratico = coef(m_quadratico),
  hetero = hetero, influencia = influencia, dados_influencia = dados_influencia,
  tempo = tempo, binario = binario, medias_binarias = medias_binarias,
  coef_binario = coef(m_binario), erros_nao_normais = erros_nao_normais, mc = mc,
  eleicao = eleicao, brasil = brasil, diagnosticos = diagnosticos,
  coef_uf = coef(m_uf), exterior = exterior, sensibilidade = sensibilidade,
  indices_teste = indices_teste, avaliacao = avaliacao,
  variancia_residuos = variancia_residuos,
  entrada = entrada, entrada_md5 = unname(tools::md5sum(entrada)))
saveRDS(resultados, file.path(saida, "resultados_aula_08.rds"))
for (nome in c("influencia", "medias_binarias", "exterior", "sensibilidade", "avaliacao", "variancia_residuos")) {
  write.csv(resultados[[nome]], file.path(saida, paste0(nome, ".csv")), row.names = FALSE)
}
write.csv(diagnosticos, file.path(saida, "diagnosticos_eleitorais.csv"), row.names = FALSE)
saveRDS(list(curva = g_curva, residuos_curva = g_res_curva, hetero = g_hetero,
             influencia = g_influencia, dependencia = g_tempo, binario = g_binario,
             erros_e_estimador = g_erros + g_mc, eleitoral = g_eleitoral,
             dispersao = g_dispersao), file.path(saida, "graficos_aula_08.rds"))
capture.output(sessionInfo(), file = file.path(saida, "sessao_R.txt"))
cat("AULA8_PREPARADA\n")
print(avaliacao)
print(exterior)
