# Exemplo ilustrativo para explicação ao docente; não integra os slides.
# Executar a partir de 2026/lab-regressao-aula:
# Rscript --vanilla projeto_agna/scripts/22_exemplo_pesos_mqo_docente.R

here::i_am("projeto_agna/scripts/22_exemplo_pesos_mqo_docente.R")

dados <- data.frame(
  observacao = 1:3,
  x = c(0, 1, 2),
  y = c(1, 4, 3)
)
stopifnot(nrow(dados) == 3L, !anyNA(dados), !anyDuplicated(dados$x))

intercepto_alternativo <- dados$y[1]
inclinacao_alternativa <- dados$y[2] - dados$y[1]
ajuste_mqo <- stats::lm(y ~ x, data = dados)
coeficientes_mqo <- stats::coef(ajuste_mqo)

stopifnot(
  isTRUE(all.equal(inclinacao_alternativa, 3)),
  isTRUE(all.equal(unname(coeficientes_mqo["x"]),
                   (dados$y[3] - dados$y[1]) / 2)),
  isTRUE(all.equal(unname(coeficientes_mqo["(Intercept)"]), 5 / 3))
)

grade_x <- seq(0, 2, length.out = 201)
retas <- rbind(
  data.frame(
    x = grade_x,
    y = intercepto_alternativo + inclinacao_alternativa * grade_x,
    estimador = "Usa observações 1 e 2: y = 1 + 3x"
  ),
  data.frame(
    x = grade_x,
    y = unname(coeficientes_mqo["(Intercept)"]) +
      unname(coeficientes_mqo["x"]) * grade_x,
    estimador = "MQO com três observações: y = 5/3 + x"
  )
)

retas$estimador <- factor(
  retas$estimador,
  levels = c(
    "Usa observações 1 e 2: y = 1 + 3x",
    "MQO com três observações: y = 5/3 + x"
  )
)

grafico <- ggplot2::ggplot(retas, ggplot2::aes(x = x, y = y,
                                               color = estimador,
                                               linetype = estimador)) +
  ggplot2::geom_line(linewidth = 1.15) +
  ggplot2::geom_point(data = dados, ggplot2::aes(x = x, y = y),
                      inherit.aes = FALSE, color = "#172033", size = 3) +
  ggplot2::geom_text(
    data = data.frame(x = c(0.10, 1.07, 2.07),
                      y = c(1.10, 4.18, 3.10),
                      rotulo = c("1", "2", "3")),
    ggplot2::aes(x = x, y = y, label = rotulo),
    inherit.aes = FALSE, color = "#172033", size = 4.2
  ) +
  ggplot2::scale_color_manual(
    values = c("Usa observações 1 e 2: y = 1 + 3x" = "#D55E00",
               "MQO com três observações: y = 5/3 + x" = "#0072B2")
  ) +
  ggplot2::scale_linetype_manual(
    values = c("Usa observações 1 e 2: y = 1 + 3x" = "dashed",
               "MQO com três observações: y = 5/3 + x" = "solid")
  ) +
  ggplot2::scale_x_continuous(breaks = 0:2, limits = c(-0.1, 2.18)) +
  ggplot2::scale_y_continuous(breaks = 0:7, limits = c(0, 7.3)) +
  ggplot2::labs(
    title = "Duas retas possíveis para três observações",
    subtitle = "Os pontos 1, 2 e 3 são (0, 1), (1, 4) e (2, 3)",
    x = "X",
    y = "Y",
    color = NULL,
    linetype = NULL,
    caption = "Exemplo inteiramente ilustrativo. A reta laranja usa os pontos 1 e 2; a azul é o ajuste de MQO dos três pontos."
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    legend.position = "top",
    legend.text = ggplot2::element_text(size = 11),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(hjust = 0, size = 9),
    panel.grid.minor = ggplot2::element_blank()
  )

saida <- here::here("projeto_agna", "output", "aula_07", "explicacoes_docente")
dir.create(saida, recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(file.path(saida, "retas_estimadores_lineares.pdf"),
                plot = grafico, width = 7.5, height = 4.6, units = "in",
                device = grDevices::pdf)
ggplot2::ggsave(file.path(saida, "retas_estimadores_lineares.png"),
                plot = grafico, width = 7.5, height = 4.6, units = "in",
                dpi = 200)

cat("ALT_INTERCEPTO=", intercepto_alternativo, "\n", sep = "")
cat("ALT_INCLINACAO=", inclinacao_alternativa, "\n", sep = "")
cat("MQO_INTERCEPTO=", unname(coeficientes_mqo["(Intercept)"]), "\n", sep = "")
cat("MQO_INCLINACAO=", unname(coeficientes_mqo["x"]), "\n", sep = "")
