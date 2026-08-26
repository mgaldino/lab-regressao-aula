# Gerar exemplos visuais de momentos para a Aula 2.
#
# As três distribuições são colocadas na mesma escala:
# E[Z] = 0 e Var[Z] = 1. Assim, as diferenças visuais restantes são
# resumidas pelos momentos padronizados de ordens 3 e 4.

options(scipen = 999)

suppressPackageStartupMessages({
  library(ggplot2)
  library(here)
  library(readr)
  library(tibble)
})

diretorio_figuras <- here::here("materials_aula_02", "figures")
dir.create(diretorio_figuras, recursive = TRUE, showWarnings = FALSE)

azul <- "#0072B2"
azul_claro <- "#56B4E9"
laranja <- "#E69F00"
verde <- "#009E73"
cinza <- "#4B5563"

tema_momentos <- ggplot2::theme_minimal(base_size = 15) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(
      color = "#E5E7EB",
      linewidth = 0.35
    ),
    panel.grid.major.x = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(color = "#111111"),
    axis.text = ggplot2::element_text(color = cinza),
    plot.margin = ggplot2::margin(7, 16, 7, 7)
  )

salvar_figura <- function(grafico, nome) {
  ggplot2::ggsave(
    filename = file.path(diretorio_figuras, nome),
    plot = grafico,
    width = 7.2,
    height = 3.35,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

# Uniforme padronizada: U(-sqrt(3), sqrt(3)).
limite_uniforme <- sqrt(3)
dados_uniforme <- tibble::tibble(
  z = seq(-limite_uniforme, limite_uniforme, length.out = 600L),
  densidade = 1 / (2 * limite_uniforme)
)

set.seed(
  6183,
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)
amostra_uniforme <- tibble::tibble(
  z = stats::runif(30000L, -limite_uniforme, limite_uniforme)
)

figura_uniforme <- ggplot2::ggplot(
  amostra_uniforme,
  ggplot2::aes(x = z)
) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = ggplot2::after_stat(density)),
    bins = 28,
    boundary = -limite_uniforme,
    fill = azul_claro,
    color = "white",
    linewidth = 0.25,
    alpha = 0.72
  ) +
  ggplot2::geom_line(
    data = dados_uniforme,
    ggplot2::aes(x = z, y = densidade),
    inherit.aes = FALSE,
    color = azul,
    linewidth = 1.1
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    color = laranja,
    linewidth = 1.0
  ) +
  ggplot2::geom_vline(
    xintercept = c(-1, 1),
    color = verde,
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  ggplot2::annotate(
    "text",
    x = 0.08,
    y = 0.315,
    label = "média = 0",
    hjust = 0,
    color = laranja,
    fontface = "bold",
    size = 4.2
  ) +
  ggplot2::annotate(
    "segment",
    x = -1,
    xend = 1,
    y = 0.055,
    yend = 0.055,
    color = verde,
    linewidth = 0.8
  ) +
  ggplot2::annotate(
    "text",
    x = 0,
    y = 0.025,
    label = "2 desvios-padrão",
    color = verde,
    fontface = "bold",
    size = 4.0
  ) +
  ggplot2::coord_cartesian(xlim = c(-2.15, 2.15), ylim = c(0, 0.35)) +
  ggplot2::scale_x_continuous(
    breaks = c(-sqrt(3), -1, 0, 1, sqrt(3)),
    labels = c("-1,73", "-1", "0", "1", "1,73")
  ) +
  ggplot2::labs(x = "Valor padronizado z", y = "Densidade") +
  tema_momentos

salvar_figura(figura_uniforme, "figura_2_uniforme_momentos.png")

# Normal padrão: a região verde marca um desvio-padrão de cada lado da média;
# as regiões laranjas marcam |Z| > 2.
dados_normal <- tibble::tibble(
  z = seq(-4, 4, length.out = 1201L),
  densidade = stats::dnorm(z)
)

amostra_normal <- tibble::tibble(
  z = stats::rnorm(30000L)
)

figura_normal <- ggplot2::ggplot(
  amostra_normal,
  ggplot2::aes(x = z)
) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = ggplot2::after_stat(density)),
    binwidth = 0.22,
    boundary = 0,
    fill = azul_claro,
    color = "white",
    linewidth = 0.22,
    alpha = 0.68
  ) +
  ggplot2::geom_area(
    data = dados_normal[abs(dados_normal$z) <= 1, ],
    ggplot2::aes(x = z, y = densidade),
    inherit.aes = FALSE,
    fill = verde,
    alpha = 0.22
  ) +
  ggplot2::geom_area(
    data = dados_normal[dados_normal$z <= -2, ],
    ggplot2::aes(x = z, y = densidade),
    inherit.aes = FALSE,
    fill = laranja,
    alpha = 0.45
  ) +
  ggplot2::geom_area(
    data = dados_normal[dados_normal$z >= 2, ],
    ggplot2::aes(x = z, y = densidade),
    inherit.aes = FALSE,
    fill = laranja,
    alpha = 0.45
  ) +
  ggplot2::geom_line(
    data = dados_normal,
    ggplot2::aes(x = z, y = densidade),
    inherit.aes = FALSE,
    color = azul,
    linewidth = 1.1
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    color = laranja,
    linewidth = 1.0
  ) +
  ggplot2::geom_vline(
    xintercept = c(-1, 1),
    color = verde,
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  ggplot2::annotate(
    "text",
    x = 0.12,
    y = 0.415,
    label = "média = 0",
    hjust = 0,
    color = laranja,
    fontface = "bold",
    size = 4.2
  ) +
  ggplot2::annotate(
    "text",
    x = 0,
    y = 0.055,
    label = "média ± 1 DP",
    color = verde,
    fontface = "bold",
    size = 4.0
  ) +
  ggplot2::annotate(
    "text",
    x = 2.75,
    y = 0.035,
    label = "caudas |z| > 2",
    color = laranja,
    fontface = "bold",
    size = 3.8
  ) +
  ggplot2::coord_cartesian(xlim = c(-4, 4), ylim = c(0, 0.44)) +
  ggplot2::scale_x_continuous(breaks = -4:4) +
  ggplot2::labs(x = "Valor padronizado z", y = "Densidade") +
  tema_momentos

salvar_figura(figura_normal, "figura_3_normal_momentos.png")

# Poisson(3) padronizada: Z = (X - 3) / sqrt(3).
lambda_poisson <- 3
dados_poisson <- tibble::tibble(
  x = 0:12,
  z = (x - lambda_poisson) / sqrt(lambda_poisson),
  probabilidade = stats::dpois(x, lambda = lambda_poisson),
  cauda = abs(z) > 2
)

figura_poisson <- ggplot2::ggplot(
  dados_poisson,
  ggplot2::aes(x = z, y = probabilidade, fill = cauda)
) +
  ggplot2::geom_col(width = 0.42, color = "white", linewidth = 0.25) +
  ggplot2::scale_fill_manual(
    values = c(`FALSE` = azul, `TRUE` = laranja),
    guide = "none"
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    color = laranja,
    linewidth = 1.0
  ) +
  ggplot2::geom_vline(
    xintercept = c(-1, 1),
    color = verde,
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  ggplot2::annotate(
    "text",
    x = 0.10,
    y = 0.245,
    label = "média = 0",
    hjust = 0,
    color = laranja,
    fontface = "bold",
    size = 4.2
  ) +
  ggplot2::annotate(
    "text",
    x = 2.3,
    y = 0.105,
    label = "cauda direita",
    color = laranja,
    fontface = "bold",
    size = 3.9
  ) +
  ggplot2::coord_cartesian(xlim = c(-2.0, 5.3), ylim = c(0, 0.27)) +
  ggplot2::scale_x_continuous(breaks = -2:5) +
  ggplot2::labs(x = "Valor padronizado z", y = "Probabilidade") +
  tema_momentos

salvar_figura(figura_poisson, "figura_4_poisson_momentos.png")

momentos_exemplos <- tibble::tibble(
  distribuicao = c(
    "Uniforme padronizada",
    "Normal padrão",
    "Poisson(3) padronizada"
  ),
  media = c(0, 0, 0),
  variancia = c(1, 1, 1),
  assimetria = c(0, 0, 1 / sqrt(3)),
  curtose = c(9 / 5, 3, 3 + 1 / 3),
  leitura_visual = c(
    "simétrica e limitada",
    "simétrica com caudas",
    "assimétrica à direita"
  )
)

stopifnot(
  nrow(momentos_exemplos) == 3L,
  all(momentos_exemplos$media == 0),
  all(momentos_exemplos$variancia == 1),
  isTRUE(all.equal(momentos_exemplos$assimetria, c(0, 0, 1 / sqrt(3)))),
  isTRUE(all.equal(momentos_exemplos$curtose, c(9 / 5, 3, 10 / 3)))
)

readr::write_csv(
  momentos_exemplos,
  file.path(diretorio_figuras, "tabela_1_momentos_distribuicoes.csv"),
  na = ""
)

arquivos_gerados <- file.path(
  diretorio_figuras,
  c(
    "figura_2_uniforme_momentos.png",
    "figura_3_normal_momentos.png",
    "figura_4_poisson_momentos.png",
    "tabela_1_momentos_distribuicoes.csv"
  )
)

stopifnot(
  all(file.exists(arquivos_gerados)),
  all(file.info(arquivos_gerados)$size > 0)
)

message("PASS: figuras e tabela de momentos geradas.")
