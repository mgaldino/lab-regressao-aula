# Série China-EUA (1990-2019) e pontos ideais dos países em 2019.
#
# Fonte única: arquivo local data/raw/unvotes/unvotes_0.3.0.tar.gz.
# Execute a partir da raiz de lab-regressao-aula:
# Rscript --vanilla projeto_agna/scripts/08_serie_china_eua_pontos_ideais_2019.R
#
# Os pontos ideais são estimativas ilustrativas de um modelo espacial
# bayesiano unidimensional (IRT 2PL) dos votos sim/não em 2019.
# Abstenções são ausentes no ajuste; as cadeias MCMC são conferidas por R-hat.

options(scipen = 999)

raiz <- here::here()
arquivo_unvotes <- file.path(
  raiz, "projeto_agna", "data", "raw", "unvotes", "unvotes_0.3.0.tar.gz"
)
arquivo_painel <- file.path(
  raiz, "projeto_agna", "data", "processed", "painel_pais_ano_1997_2016.csv"
)
diretorio_saida <- file.path(raiz, "projeto_agna", "output", "aula_05")

if (!file.exists(arquivo_unvotes) || !file.exists(arquivo_painel)) {
  stop("Os arquivos locais esperados não foram encontrados.")
}
hash_unvotes <- digest::digest(
  arquivo_unvotes, algo = "sha256", file = TRUE, serialize = FALSE
)
stopifnot(identical(
  hash_unvotes,
  "5c826e9ab6d6aa6bb52fe936050783e2ce5ba879166ba0444fe2f6e530dfdeed"
))

ler_unvotes <- function(arquivo) {
  diretorio_temporario <- tempfile("unvotes_0_3_0_")
  dir.create(diretorio_temporario)
  on.exit(unlink(diretorio_temporario, recursive = TRUE, force = TRUE))
  utils::untar(
    arquivo,
    files = c("unvotes/data/un_votes.rda", "unvotes/data/un_roll_calls.rda"),
    exdir = diretorio_temporario
  )
  ambiente <- new.env(parent = emptyenv())
  load(file.path(diretorio_temporario, "unvotes/data/un_votes.rda"),
       envir = ambiente)
  load(file.path(diretorio_temporario, "unvotes/data/un_roll_calls.rda"),
       envir = ambiente)
  list(votos = ambiente$un_votes, votacoes = ambiente$un_roll_calls)
}
fonte <- ler_unvotes(arquivo_unvotes)

votos <- fonte$votos |>
  dplyr::mutate(
    country_code = as.character(country_code),
    country = as.character(country),
    vote = as.character(vote)
  )
votacoes <- fonte$votacoes |>
  dplyr::mutate(ano = as.integer(format(date, "%Y"))) |>
  dplyr::filter(dplyr::between(ano, 1990L, 2019L)) |>
  dplyr::select(rcid, date, ano)

stopifnot(
  anyDuplicated(votacoes$rcid) == 0L,
  anyDuplicated(votos[!is.na(votos$country_code),
                       c("rcid", "country_code")]) == 0L,
  all(votos$vote %in% c("yes", "no", "abstain")),
  all(as.integer(format(votacoes$date, "%Y")) == votacoes$ano)
)

# 1. Série anual: mesmo voto na mesma votação nominal.
votos_china <- votos |>
  dplyr::filter(country_code == "CN") |>
  dplyr::select(rcid, voto_china = vote)
votos_eua <- votos |>
  dplyr::filter(country_code == "US") |>
  dplyr::select(rcid, voto_eua = vote)

pares <- votos_china |>
  dplyr::inner_join(votos_eua, by = "rcid") |>
  dplyr::inner_join(votacoes, by = "rcid") |>
  dplyr::mutate(votos_iguais = as.integer(voto_china == voto_eua))

serie <- pares |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
    n_pares_validos = dplyr::n(),
    n_votos_iguais = sum(votos_iguais),
    proporcao_iguais = mean(votos_iguais),
    .groups = "drop"
  ) |>
  dplyr::arrange(ano)

stopifnot(
  nrow(serie) == 30L,
  identical(serie$ano, 1990:2019),
  sum(serie$n_pares_validos) == nrow(pares),
  all(serie$n_pares_validos > 0L),
  all(dplyr::between(serie$proporcao_iguais, 0, 1))
)

# Conferência independente no painel anual já construído para 1997-2016.
painel_eua <- utils::read.csv(arquivo_painel) |>
  dplyr::filter(pais_iso3 == "USA") |>
  dplyr::select(
    ano,
    n_pares_validos_painel = n_pares_validos,
    n_votos_iguais_painel = n_convergentes_china,
    proporcao_painel = taxa_convergencia_china
  )
conferencia <- serie |>
  dplyr::inner_join(painel_eua, by = "ano")
stopifnot(
  nrow(conferencia) == 20L,
  all(conferencia$n_pares_validos == conferencia$n_pares_validos_painel),
  all(conferencia$n_votos_iguais == conferencia$n_votos_iguais_painel),
  all(abs(conferencia$proporcao_iguais -
            conferencia$proporcao_painel) < 1e-12)
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
    limits = c(0, 0.4), breaks = seq(0, 0.4, 0.1),
    labels = function(x) paste0(round(100 * x), "%"),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::labs(
    title = "China e EUA: votos iguais na Assembleia Geral da ONU",
    subtitle = "Proporção anual de votações nominais, 1990 a 2019",
    x = "Ano da votação",
    y = "Votos iguais (%)",
    caption = paste0(
      "Denominador: votações com votos observados de ambos os países em cada ano ",
      "(", min(serie$n_pares_validos), " a ", max(serie$n_pares_validos),
      " votações por ano).\nFonte: arquivo local unvotes 0.3.0."
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

# 2. Pontos ideais em 2019: IRT unidimensional com votos sim/não.
votacoes_2019 <- votacoes |>
  dplyr::filter(ano == 2019L)
votos_2019 <- votos |>
  dplyr::semi_join(votacoes_2019, by = "rcid")
paises_2019 <- votos_2019 |>
  dplyr::distinct(country_code, country) |>
  dplyr::arrange(country_code)

stopifnot(
  nrow(votacoes_2019) == 90L,
  nrow(paises_2019) == 193L,
  !anyNA(paises_2019[c("country_code", "country")]),
  anyDuplicated(paises_2019$country_code) == 0L,
  all(c("US", "RU", "BR", "CN") %in% paises_2019$country_code)
)

# Votações informativas: pelo menos cinco votos sim e cinco votos não.
# A regra evita itens unanimemente aprovados e separação quase total.
itens <- votos_2019 |>
  dplyr::filter(vote %in% c("yes", "no")) |>
  dplyr::count(rcid, vote, name = "n") |>
  tidyr::pivot_wider(names_from = vote, values_from = n, values_fill = 0L) |>
  dplyr::filter(yes >= 5L, no >= 5L) |>
  dplyr::arrange(rcid)

votos_modelo <- votos_2019 |>
  dplyr::filter(rcid %in% itens$rcid, vote %in% c("yes", "no")) |>
  dplyr::select(country_code, rcid, vote)

matriz <- matrix(
  NA_real_, nrow = nrow(paises_2019), ncol = nrow(itens),
  dimnames = list(paises_2019$country_code, as.character(itens$rcid))
)
indices <- cbind(
  match(votos_modelo$country_code, rownames(matriz)),
  match(votos_modelo$rcid, as.integer(colnames(matriz)))
)
stopifnot(
  nrow(itens) == 46L,
  !anyNA(indices),
  anyDuplicated(as.data.frame(indices)) == 0L
)
matriz[indices] <- as.integer(votos_modelo$vote == "yes")
n_votos_modelo <- rowSums(!is.na(matriz))
stopifnot(all(n_votos_modelo > 0L))

votacao_pscl <- pscl::rollcall(
  matriz, yea = 1, nay = 0, missing = NA, notInLegis = 9,
  legis.names = rownames(matriz), vote.names = colnames(matriz)
)
sementes <- c(20260923L, 20260924L, 20260925L)
amostras <- lapply(sementes, function(semente) {
  set.seed(semente)
  ajuste <- pscl::ideal(
    votacao_pscl, d = 1, maxiter = 60000L, burnin = 15000L,
    thin = 30L, normalize = TRUE, verbose = FALSE
  )
  cadeia <- ajuste$x[, , 1]
  # O sinal da dimensão é arbitrário. Orientar cada cadeia do mesmo modo.
  if (mean(cadeia[, "US"]) > mean(cadeia[, "CN"])) {
    cadeia <- -cadeia
  }
  cadeia
})
nomes_paises <- paises_2019$country_code
stopifnot(all(vapply(
  amostras, function(x) identical(colnames(x), nomes_paises), logical(1)
)))
rhat <- coda::gelman.diag(
  coda::mcmc.list(lapply(amostras, coda::mcmc)),
  autoburnin = FALSE, multivariate = FALSE
)$psrf[, 1]
if (any(!is.finite(rhat)) || any(rhat > 1.10)) {
  print(head(sort(rhat, decreasing = TRUE), 10L))
  stop("Ao menos um país tem R-hat acima de 1,10; rever o ajuste MCMC.")
}
posterior <- do.call(rbind, amostras)

n_votos_2019 <- as.integer(table(factor(
  votos_2019$country_code, levels = paises_2019$country_code
)))
pontos_ideais <- paises_2019 |>
  dplyr::mutate(
    n_votos_2019 = n_votos_2019,
    n_votos_modelo = as.integer(n_votos_modelo),
    ponto_ideal = as.numeric(colMeans(posterior)),
    desvio_posterior = as.numeric(apply(posterior, 2, stats::sd)),
    limite_inf_95 = as.numeric(apply(
      posterior, 2, stats::quantile, probs = 0.025
    )),
    limite_sup_95 = as.numeric(apply(
      posterior, 2, stats::quantile, probs = 0.975
    )),
    rhat = as.numeric(rhat)
  )

stopifnot(
  nrow(pontos_ideais) == 193L,
  !anyNA(pontos_ideais[c(
    "ponto_ideal", "desvio_posterior", "limite_inf_95", "limite_sup_95"
  )]),
  all(is.finite(pontos_ideais$ponto_ideal)),
  all(pontos_ideais$desvio_posterior > 0),
  all(pontos_ideais$limite_inf_95 <= pontos_ideais$ponto_ideal),
  all(pontos_ideais$ponto_ideal <= pontos_ideais$limite_sup_95)
)

# Todos os países aparecem como pontos. O deslocamento vertical é apenas visual.
set.seed(20260923)
pontos_ideais$altura <- stats::runif(nrow(pontos_ideais), -0.19, 0.19)
destaques <- c("US" = "EUA", "RU" = "Rússia",
               "BR" = "Brasil", "CN" = "China")
pontos_ideais$rotulo <- unname(destaques[pontos_ideais$country_code])
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
  ggplot2::geom_vline(xintercept = 0, color = "#D1D5DB", linewidth = 0.5) +
  ggplot2::geom_point(
    data = function(x) x[is.na(x$rotulo), ],
    color = cores[["Demais países"]], alpha = 0.62, size = 2.2
  ) +
  ggplot2::geom_segment(
    data = function(x) x[!is.na(x$rotulo), ],
    ggplot2::aes(
      x = limite_inf_95,
      xend = limite_sup_95,
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
  ggplot2::scale_y_continuous(limits = c(-0.42, 0.55), breaks = NULL) +
  ggplot2::labs(
    title = "Pontos ideais estimados dos países na AGNU, 2019",
    subtitle = "193 países; EUA, Rússia, Brasil e China identificados",
    x = "Ponto ideal estimado (escala relativa, sem unidade)",
    y = NULL,
    caption = paste0(
      "Modelo espacial bayesiano unidimensional (IRT 2PL) de ", nrow(itens),
      " votações com pelo menos 5 votos sim e 5 não. Abstenções fora do ajuste.\n",
      "Traços nos quatro destaques: intervalos de credibilidade de 95%. ",
      "O sinal foi orientado com os EUA à esquerda da China.\n",
      "Deslocamento vertical apenas para mostrar os pontos. ",
      "Fonte: arquivo local unvotes 0.3.0."
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
      country_code, country, n_votos_2019, n_votos_modelo,
      ponto_ideal, desvio_posterior, limite_inf_95, limite_sup_95, rhat
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

cat("Série China-EUA:", min(serie$ano), "a", max(serie$ano),
    "; pares válidos:", sum(serie$n_pares_validos), "\n")
cat("2019:", nrow(paises_2019), "países e", nrow(itens),
    "votações usadas para estimar pontos ideais\n")
cat("Maior R-hat:", max(pontos_ideais$rhat), "\n")
print(
  pontos_ideais |>
    dplyr::filter(!is.na(rotulo)) |>
    dplyr::select(
      rotulo, ponto_ideal, limite_inf_95, limite_sup_95,
      n_votos_modelo, rhat
    )
)
