# Aula 7: cálculos e figuras dos slides, com dados inteiramente sintéticos.
# Executar a partir de 2026/lab-regressao-aula:
# LC_ALL=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/12_simulacoes_aula7_propriedades_mqo.R
# Fontes conceituais: Aronow e Miller (2019), 3.2.2 e 3.4.3;
# Hansen, Econometrics, 4.4--4.8 e 7.2. Consulta: 13/10/2026.

options(encoding = "UTF-8", scipen = 999)
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

raiz <- normalizePath(".", mustWork = TRUE)
stopifnot(file.exists(file.path(raiz, "lab-regressao-aula-2026.Rproj")))
saida <- file.path(raiz, "projeto_agna", "output", "aula_07")
figuras <- file.path(saida, "figuras")
dir.create(figuras, recursive = TRUE, showWarnings = FALSE)

parametros <- list(alpha = 5, beta = 1.5, media_x = 10, dp_x = 3,
                   sigma = 2, repeticoes = 5000L, bootstrap = 5000L,
                   data_criacao = "2026-10-13")

# Fórmula escalar de MQO com intercepto. Conferida contra lm() no validador.
ajustar <- function(dados) {
  xc <- dados$x - mean(dados$x)
  beta <- sum(xc * (dados$y - mean(dados$y))) / sum(xc^2)
  c(alpha = mean(dados$y) - beta * mean(dados$x), beta = beta)
}

gerar <- function(n, sigma = 2, hetero = FALSE, deslocamento = FALSE) {
  x <- rnorm(n, parametros$media_x, parametros$dp_x)
  z <- (x - parametros$media_x) / parametros$dp_x
  # Heterocedasticidade com E[Var(erro|X)] = 4, igual ao caso sigma = 2.
  dp_erro <- if (hetero) sigma * sqrt((1 + z^2) / 2) else rep(sigma, n)
  v <- rnorm(n, sd = dp_erro)
  # u é o erro da regra geradora. No cenário deslocado, o erro do BLP é v.
  u <- if (deslocamento) 0.8 * (x - parametros$media_x) + v else v
  data.frame(id = seq_len(n), x = x, v = v, u = u,
             y = parametros$alpha + parametros$beta * x + u)
}

set.seed(618307)
amostras <- lapply(1:2, function(i) gerar(40))
coef_amostras <- do.call(rbind, lapply(amostras, ajustar))
amostras_fig <- bind_rows(lapply(seq_along(amostras), function(i) {
  mutate(amostras[[i]], amostra = paste0("Amostra ", i))
}))
retas <- expand.grid(x = range(amostras_fig$x),
                     amostra = c("Amostra 1", "Amostra 2"))
retas$indice <- match(retas$amostra, c("Amostra 1", "Amostra 2"))
retas$estimada <- coef_amostras[retas$indice, "alpha"] +
  coef_amostras[retas$indice, "beta"] * retas$x
retas$populacional <- parametros$alpha + parametros$beta * retas$x

cenarios <- data.frame(
  cenario = c("n40", "n160", "n640", "ruido", "hetero", "alvo40", "alvo640"),
  n = c(40L, 160L, 640L, 40L, 40L, 40L, 640L),
  sigma = c(2, 2, 2, 6, 2, 2, 2),
  hetero = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
  deslocamento = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
  alvo_blp = c(1.5, 1.5, 1.5, 1.5, 1.5, 2.3, 2.3)
)

set.seed(618308)
simulacoes <- bind_rows(lapply(seq_len(nrow(cenarios)), function(j) {
  cenario <- cenarios[j, ]
  coeficientes <- replicate(parametros$repeticoes,
    ajustar(gerar(cenario$n, cenario$sigma, cenario$hetero,
                 cenario$deslocamento)))
  data.frame(cenario = cenario$cenario,
             repeticao = seq_len(parametros$repeticoes),
             n = cenario$n, alpha = coeficientes["alpha", ],
             beta = coeficientes["beta", ], alvo = cenario$alvo_blp)
}))
resumo <- simulacoes |>
  group_by(cenario, n, alvo) |>
  summarise(repeticoes = n(), media = mean(beta),
            vies_mc = mean(beta) - first(alvo),
            variancia_mc = mean((beta - mean(beta))^2),
            dp_mc = sd(beta), eqm_mc = mean((beta - alvo)^2),
            ep_media_mc = sd(beta) / sqrt(n()),
            .groups = "drop")

# Bootstrap de pares em UMA amostra observada; mantém X e Y da mesma unidade.
dados_boot <- amostras[[1]]
set.seed(618309)
indices_boot <- replicate(parametros$bootstrap,
                          sample.int(nrow(dados_boot), replace = TRUE))
coef_boot <- apply(indices_boot, 2, function(indices) ajustar(dados_boot[indices, ]))
bootstrap <- data.frame(repeticao = seq_len(parametros$bootstrap),
                        alpha = coef_boot["alpha", ], beta = coef_boot["beta", ])
resumo_boot <- data.frame(n = nrow(dados_boot), repeticoes = nrow(bootstrap),
                          beta_original = coef_amostras[1, "beta"],
                          media_boot = mean(bootstrap$beta),
                          ep_boot = sd(bootstrap$beta))

# Experimento condicional: mantém a matriz X da primeira amostra fixa.
# Permite verificar E[beta_hat|X] e Var(beta_hat|X) diretamente.
set.seed(618310)
fixo_x <- dados_boot$x
condicionais <- replicate(parametros$repeticoes, {
  y <- parametros$alpha + parametros$beta * fixo_x +
    rnorm(length(fixo_x), sd = parametros$sigma)
  ajustar(data.frame(x = fixo_x, y = y))
})

azul <- "#1D4ED8"
verde <- "#047857"
tema <- theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 13, color = "#0F172A"),
        axis.title = element_text(size = 14),
        legend.position = "bottom", legend.title = element_blank(),
        plot.margin = margin(5, 12, 5, 5))
decimais <- function(x) format(x, trim = TRUE, decimal.mark = ",", digits = 3)
exportar <- function(grafico, nome, largura = 8.4, altura = 3.25) {
  ggsave(file.path(figuras, paste0(nome, ".pdf")), grafico,
         width = largura, height = altura, units = "in",
         device = grDevices::pdf, useDingbats = FALSE, bg = "white")
  ggsave(file.path(figuras, paste0(nome, ".png")), grafico,
         width = largura, height = altura, units = "in", dpi = 200, bg = "white")
  stopifnot(all(file.exists(file.path(figuras, paste0(nome, c(".pdf", ".png"))))))
}

g_amostras <- ggplot(amostras_fig, aes(x, y)) +
  geom_point(color = "#334155", size = 1.8, alpha = 0.8) +
  geom_line(data = retas, aes(y = estimada, color = "MQO da amostra",
                            linetype = "MQO da amostra"), linewidth = 1) +
  geom_line(data = retas, aes(y = populacional, color = "BLP da população",
                            linetype = "BLP da população"), linewidth = 1) +
  facet_wrap(~amostra, nrow = 1) +
  scale_color_manual(name = NULL, values = c("MQO da amostra" = azul,
                                "BLP da população" = verde)) +
  scale_linetype_manual(name = NULL, values = c("MQO da amostra" = "solid",
                                   "BLP da população" = "22")) +
  labs(x = "X (unidades)", y = "Y (unidades)") + tema
exportar(g_amostras, "01_duas_amostras")

histograma <- function(dados, largura_classe = 0.025) {
  ggplot(dados, aes(beta)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = largura_classe,
                   boundary = 1.5, fill = azul, color = "white", linewidth = 0.1) +
    geom_vline(xintercept = 1.5, color = verde, linetype = "22", linewidth = 1) +
    scale_x_continuous(labels = decimais) +
    labs(x = "Inclinação estimada (unidades de Y por unidade de X)",
         y = "Densidade") + tema
}
exportar(histograma(filter(simulacoes, cenario == "n40")),
         "02_distribuicao_amostral", largura = 8.0)

g_tamanhos <- simulacoes |>
  filter(cenario %in% c("n40", "n160", "n640")) |>
  mutate(painel = factor(paste0("n = ", n),
                         levels = c("n = 40", "n = 160", "n = 640")))
exportar(histograma(g_tamanhos) + facet_wrap(~painel, nrow = 1),
         "03_tamanho_amostra")

g_erros <- simulacoes |>
  filter(cenario %in% c("n40", "ruido", "hetero")) |>
  mutate(painel = factor(cenario, levels = c("n40", "ruido", "hetero"),
                         labels = c("Homocedástico\nsigma = 2",
                                    "Homocedástico\nsigma = 6",
                                    "Heterocedástico\nvariância média = 4")))
exportar(histograma(g_erros, largura_classe = 0.04) + facet_wrap(~painel, nrow = 1),
         "04_ruido_heterocedasticidade")

g_alvo <- simulacoes |>
  filter(cenario %in% c("alvo40", "alvo640")) |>
  mutate(painel = factor(paste0("n = ", n), levels = c("n = 40", "n = 640")))
g_alvo <- ggplot(g_alvo, aes(beta)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.025,
                   fill = azul, color = "white", linewidth = 0.1) +
  geom_vline(aes(xintercept = 1.5, color = "Parâmetro da regra: 1,5",
                linetype = "Parâmetro da regra: 1,5"), linewidth = 1) +
  geom_vline(aes(xintercept = 2.3, color = "Coeficiente do BLP: 2,3",
                linetype = "Coeficiente do BLP: 2,3"), linewidth = 1) +
  scale_color_manual(name = NULL, values = c("Parâmetro da regra: 1,5" = "#B45309",
                                "Coeficiente do BLP: 2,3" = verde)) +
  scale_linetype_manual(name = NULL, values = c("Parâmetro da regra: 1,5" = "dotted",
                                   "Coeficiente do BLP: 2,3" = "22")) +
  facet_wrap(~painel, nrow = 1) + scale_x_continuous(labels = decimais) +
  labs(x = "Inclinação estimada (unidades de Y por unidade de X)",
         y = "Densidade") + tema
exportar(g_alvo, "05_parametro_e_blp", altura = 2.6)

comparacao_boot <- bind_rows(
  filter(simulacoes, cenario == "n40") |>
    transmute(beta, painel = "Monte Carlo\namostras da população conhecida"),
  bootstrap |>
    transmute(beta, painel = "Bootstrap\nreamostras de uma amostra observada")
)
ordem_boot <- c("Monte Carlo\namostras da população conhecida",
                "Bootstrap\nreamostras de uma amostra observada")
comparacao_boot$painel <- factor(comparacao_boot$painel, levels = ordem_boot)
referencias_boot <- data.frame(
  painel = c("Monte Carlo\namostras da população conhecida",
              "Bootstrap\nreamostras de uma amostra observada"),
  referencia = c(parametros$beta, resumo_boot$beta_original)
)
referencias_boot$painel <- factor(referencias_boot$painel, levels = ordem_boot)
g_boot <- ggplot(comparacao_boot, aes(beta)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.025,
                   fill = azul, color = "white", linewidth = 0.1) +
  geom_vline(data = referencias_boot, aes(xintercept = referencia),
             color = verde, linetype = "22", linewidth = 1) +
  facet_wrap(~painel, nrow = 1) + scale_x_continuous(labels = decimais) +
  labs(x = "Inclinação estimada (unidades de Y por unidade de X)",
         y = "Densidade") + tema
exportar(g_boot, "06_bootstrap")

artefatos <- list(parametros = parametros, cenarios = cenarios,
                  amostras = amostras, coef_amostras = coef_amostras,
                  simulacoes = simulacoes, resumo = resumo,
                  bootstrap = bootstrap, resumo_boot = resumo_boot,
                  indices_boot = indices_boot, condicionais = condicionais)
saveRDS(artefatos, file.path(saida, "resultados_aula_07.rds"))
write.csv(simulacoes, file.path(saida, "estimativas_monte_carlo.csv"), row.names = FALSE)
write.csv(resumo, file.path(saida, "resumo_monte_carlo.csv"), row.names = FALSE)
write.csv(bootstrap, file.path(saida, "estimativas_bootstrap.csv"), row.names = FALSE)
write.csv(amostras_fig, file.path(saida, "duas_amostras_sinteticas.csv"), row.names = FALSE)
write.csv(resumo_boot, file.path(saida, "resumo_bootstrap.csv"), row.names = FALSE)

cat("SIMULACOES_AULA7_OK\n")
print(as.data.frame(resumo))
print(resumo_boot)
