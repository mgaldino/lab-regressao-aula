# Convergência dos votos de China e EUA na AGNU, 1997-2016.
# Execute a partir da raiz de lab-regressao-aula.
# Entrada: projeto_agna/data/processed/painel_pais_ano_1997_2016.csv.
# Saídas: projeto_agna/output/aula_05/figura_china_eua_convergencia_anual.{png,pdf}.

entrada <- file.path(
  "projeto_agna", "data", "processed", "painel_pais_ano_1997_2016.csv"
)
diretorio_saida <- file.path("projeto_agna", "output", "aula_05")

if (!file.exists(entrada)) {
  stop("Execute o script a partir da raiz de lab-regressao-aula.")
}

painel_anual <- utils::read.csv(entrada, stringsAsFactors = FALSE)
china_eua <- painel_anual |>
  dplyr::filter(pais_iso3 == "USA") |>
  dplyr::select(
    ano, n_pares_validos, n_convergentes_china, taxa_convergencia_china
  ) |>
  dplyr::arrange(ano)

stopifnot(
  nrow(china_eua) == 20L,
  identical(china_eua$ano, 1997:2016),
  !anyNA(china_eua),
  all(china_eua$n_pares_validos > 0),
  all(china_eua$n_convergentes_china >= 0),
  all(china_eua$n_convergentes_china <= china_eua$n_pares_validos),
  all(abs(
    china_eua$n_convergentes_china / china_eua$n_pares_validos -
      china_eua$taxa_convergencia_china
  ) < 1e-12)
)

amplitude_denominador <- range(china_eua$n_pares_validos)

grafico <- ggplot2::ggplot(
  china_eua,
  ggplot2::aes(x = ano, y = taxa_convergencia_china)
) +
  ggplot2::geom_line(color = "#166A80", linewidth = 1.1) +
  ggplot2::geom_point(color = "#166A80", size = 2.8) +
  ggplot2::scale_x_continuous(
    breaks = c(1997, 2000, 2004, 2008, 2012, 2016)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 0.4),
    breaks = seq(0, 0.4, by = 0.1),
    labels = function(x) paste0(round(100 * x), "%"),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::labs(
    title = "China e EUA: votos iguais na Assembleia Geral da ONU",
    subtitle = "Proporção anual de resoluções em que os dois países votaram da mesma forma, 1997 a 2016",
    x = "Ano da votação",
    y = "Votos iguais (%)",
    caption = paste0(
      "Cada ponto representa um ano. Denominador: resoluções com votos observados de ambos os países ",
      "(", amplitude_denominador[1], " a ", amplitude_denominador[2], " por ano).\n",
      "Fonte: unvotes 0.3.0, painel didático AGNA."
    )
  ) +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold", size = 17),
    plot.subtitle = ggplot2::element_text(size = 11.5),
    plot.caption = ggplot2::element_text(size = 9.5, hjust = 0),
    plot.margin = ggplot2::margin(16, 22, 16, 16)
  )

dir.create(diretorio_saida, recursive = TRUE, showWarnings = FALSE)
base_saida <- file.path(diretorio_saida, "figura_china_eua_convergencia_anual")

ggplot2::ggsave(
  paste0(base_saida, ".png"), grafico,
  width = 10, height = 5.8, units = "in", dpi = 180, bg = "white"
)
ggplot2::ggsave(
  paste0(base_saida, ".pdf"), grafico,
  width = 10, height = 5.8, units = "in", device = grDevices::pdf,
  bg = "white"
)

cat("Pares válidos:", sum(china_eua$n_pares_validos), "\n")
cat("Votos iguais:", sum(china_eua$n_convergentes_china), "\n")
cat("Taxa total:",
    round(100 * sum(china_eua$n_convergentes_china) /
            sum(china_eua$n_pares_validos), 1), "%\n")
cat("PNG:", paste0(base_saida, ".png"), "\n")
