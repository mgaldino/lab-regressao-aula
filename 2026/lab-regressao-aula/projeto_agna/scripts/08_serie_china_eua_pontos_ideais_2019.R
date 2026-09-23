# Série de concordância China-EUA (1990-2019) e pontos ideais em 2019.
# Ambos os gráficos usam diretamente o arquivo local do paper.
# Execute da raiz de lab-regressao-aula com:
# Rscript --vanilla projeto_agna/scripts/08_serie_china_eua_pontos_ideais_2019.R

options(scipen = 999)

raiz <- here::here()
arquivo_ideais <- file.path(
  raiz, "projeto_agna", "data", "raw", "ideal_points",
  "IdealpointestimatesAll_Jun2024.csv"
)
diretorio_saida <- file.path(raiz, "projeto_agna", "output", "aula_05")
if (!file.exists(arquivo_ideais)) {
  stop("A tabela local de pontos ideais do paper não foi encontrada.")
}
stopifnot(identical(
  digest::digest(
    arquivo_ideais, algo = "sha256", file = TRUE, serialize = FALSE
  ),
  "94ce7440bdba9252b2f4294333291585748dfe84dbaf56fe9f26e1af38f66198"
))

dados <- utils::read.csv(
  arquivo_ideais, check.names = FALSE, stringsAsFactors = FALSE
)
colunas <- c(
  "session", "iso3c", "Countryname", "NVotesAll", "ChinaAgree",
  "Q50%All", "Q5%All", "Q95%All"
)
stopifnot(all(colunas %in% names(dados)))
# O CSV contém uma primeira coluna sem nome; selecionamos só as colunas usadas.
dados <- dados[, colunas, drop = FALSE]

# 1. Medida de concordância com a China, já presente no arquivo do paper.
# No pipeline do paper, ano de referência = session + 1945.
serie <- dados |>
  dplyr::filter(
    iso3c == "USA", dplyr::between(session, 45L, 74L)
  ) |>
  dplyr::transmute(
    ano = as.integer(session + 1945L),
    sessao = as.integer(session),
    proporcao_iguais = as.numeric(.data[["ChinaAgree"]])
  ) |>
  dplyr::arrange(ano)

stopifnot(
  nrow(serie) == 30L,
  anyDuplicated(serie$ano) == 0L,
  identical(serie$ano, 1990:2019),
  !anyNA(serie),
  all(dplyr::between(serie$proporcao_iguais, 0, 1))
)

grafico_serie <- ggplot2::ggplot(
  serie, ggplot2::aes(x = ano, y = proporcao_iguais)
) +
  ggplot2::geom_line(color = "#166A80", linewidth = 1.1) +
  ggplot2::geom_point(color = "#166A80", size = 2.4) +
  ggplot2::scale_x_continuous(
    breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2019)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 0.2), breaks = seq(0, 0.2, 0.05),
    labels = function(x) paste0(round(100 * x), "%"),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::labs(
    title = "China e EUA: votos iguais na Assembleia Geral da ONU",
    subtitle = "Concordância nas sessões iniciadas entre 1990 e 2019",
    x = "Ano de referência da sessão",
    y = "Votos iguais (%)",
    caption = paste0(
      "Série ChinaAgree para os EUA, como registrada no arquivo do paper. ",
      "Ano = sessão + 1945.\nFonte: IdealpointestimatesAll_Jun2024.csv."
    )
  ) +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold", size = 17),
    plot.subtitle = ggplot2::element_text(size = 12),
    plot.caption = ggplot2::element_text(size = 9.5, hjust = 0),
    plot.margin = ggplot2::margin(16, 22, 16, 16)
  )

# 2. Medianas posteriores dos pontos ideais já usadas no paper.
pontos_ideais <- dados |>
  dplyr::filter(session == 74L) |>
  dplyr::transmute(
    ano_referencia = as.integer(session + 1945L),
    sessao = as.integer(session),
    iso3c = as.character(iso3c),
    pais = as.character(Countryname),
    n_votacoes = as.integer(NVotesAll),
    ponto_ideal = as.numeric(.data[["Q50%All"]]),
    limite_inf_90 = as.numeric(.data[["Q5%All"]]),
    limite_sup_90 = as.numeric(.data[["Q95%All"]])
  ) |>
  dplyr::arrange(ponto_ideal)

stopifnot(
  nrow(pontos_ideais) == 193L,
  anyDuplicated(pontos_ideais$iso3c) == 0L,
  !anyNA(pontos_ideais),
  all(pontos_ideais$ano_referencia == 2019L),
  all(pontos_ideais$n_votacoes > 0L),
  all(pontos_ideais$limite_inf_90 <= pontos_ideais$ponto_ideal),
  all(pontos_ideais$ponto_ideal <= pontos_ideais$limite_sup_90),
  all(c("USA", "RUS", "BRA", "CHN") %in% pontos_ideais$iso3c)
)

# O deslocamento vertical só evita a sobreposição de pontos no gráfico.
set.seed(20260923)
pontos_ideais$altura <- stats::runif(nrow(pontos_ideais), -0.19, 0.19)
destaques <- c(
  "USA" = "EUA", "RUS" = "Rússia",
  "BRA" = "Brasil", "CHN" = "China"
)
pontos_ideais$rotulo <- unname(destaques[pontos_ideais$iso3c])
pontos_ideais$grupo <- ifelse(
  is.na(pontos_ideais$rotulo), "Demais países", pontos_ideais$rotulo
)
pontos_ideais$grupo <- factor(
  pontos_ideais$grupo,
  levels = c("Demais países", "EUA", "Rússia", "Brasil", "China")
)
cores <- c(
  "Demais países" = "#9CA3AF", "EUA" = "#C2410C",
  "Rússia" = "#7C3AED", "Brasil" = "#15803D", "China" = "#1D4ED8"
)

grafico_pontos <- ggplot2::ggplot(
  pontos_ideais, ggplot2::aes(x = ponto_ideal, y = altura)
) +
  ggplot2::geom_vline(
    xintercept = 0, color = "#D1D5DB", linewidth = 0.5
  ) +
  ggplot2::geom_point(
    data = function(x) x[is.na(x$rotulo), ],
    color = cores[["Demais países"]], alpha = 0.62, size = 2.2
  ) +
  ggplot2::geom_segment(
    data = function(x) x[!is.na(x$rotulo), ],
    ggplot2::aes(
      x = limite_inf_90, xend = limite_sup_90,
      y = altura, yend = altura, color = grupo
    ),
    linewidth = 0.8, alpha = 0.55
  ) +
  ggplot2::geom_point(
    data = function(x) x[!is.na(x$rotulo), ],
    ggplot2::aes(color = grupo), size = 3.7
  ) +
  ggrepel::geom_label_repel(
    data = function(x) x[!is.na(x$rotulo), ],
    ggplot2::aes(label = rotulo, color = grupo),
    fill = "white", size = 4, fontface = "bold",
    box.padding = 0.4, point.padding = 0.5,
    min.segment.length = 0, seed = 20260923,
    show.legend = FALSE
  ) +
  ggplot2::scale_color_manual(values = cores, guide = "none") +
  ggplot2::scale_y_continuous(limits = c(-0.28, 0.35), breaks = NULL) +
  ggplot2::labs(
    title = "Pontos ideais da AGNU em 2019 (sessão 74)",
    subtitle = "193 países; EUA, Rússia, Brasil e China identificados",
    x = "Ponto ideal publicado (escala relativa, sem unidade)",
    y = NULL,
    caption = paste0(
      "Valores do paper: mediana posterior Q50%All. ",
      "Traços nos destaques: intervalo de 5% a 95%.\n",
      "Ano de referência = início da sessão da AGNU. ",
      "Deslocamento vertical apenas para mostrar os pontos.\n",
      "Fonte: IdealpointestimatesAll_Jun2024.csv, arquivo local do paper."
    )
  ) +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold", size = 17),
    plot.subtitle = ggplot2::element_text(size = 12),
    plot.caption = ggplot2::element_text(size = 9.5, hjust = 0),
    plot.margin = ggplot2::margin(16, 22, 16, 16)
  )

dir.create(diretorio_saida, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  serie,
  file.path(diretorio_saida, "serie_convergencia_china_eua_1990_2019.csv"),
  row.names = FALSE, na = ""
)
utils::write.csv(
  pontos_ideais |>
    dplyr::select(
      ano_referencia, sessao, iso3c, pais, n_votacoes,
      ponto_ideal, limite_inf_90, limite_sup_90
    ),
  file.path(diretorio_saida, "pontos_ideais_2019.csv"),
  row.names = FALSE, na = ""
)

ggplot2::ggsave(
  file.path(diretorio_saida, "figura_convergencia_china_eua_1990_2019.png"),
  grafico_serie, width = 10, height = 5.8, units = "in", dpi = 180,
  bg = "white"
)
ggplot2::ggsave(
  file.path(diretorio_saida, "figura_convergencia_china_eua_1990_2019.pdf"),
  grafico_serie, width = 10, height = 5.8, units = "in",
  device = grDevices::pdf, bg = "white"
)
ggplot2::ggsave(
  file.path(diretorio_saida, "figura_pontos_ideais_2019.png"),
  grafico_pontos, width = 10, height = 6.2, units = "in", dpi = 180,
  bg = "white"
)
ggplot2::ggsave(
  file.path(diretorio_saida, "figura_pontos_ideais_2019.pdf"),
  grafico_pontos, width = 10, height = 6.2, units = "in",
  device = grDevices::pdf, bg = "white"
)

cat("Série do paper:", min(serie$ano), "a", max(serie$ano),
    "; sessões:", nrow(serie), "\n")
cat("Pontos ideais do paper:", nrow(pontos_ideais),
    "países na sessão", unique(pontos_ideais$sessao), "\n")
print(
  pontos_ideais |>
    dplyr::filter(!is.na(rotulo)) |>
    dplyr::select(
      rotulo, ponto_ideal, limite_inf_90, limite_sup_90, n_votacoes
    )
)
