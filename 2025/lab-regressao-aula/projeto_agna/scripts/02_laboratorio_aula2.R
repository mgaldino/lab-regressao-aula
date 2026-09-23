# Laboratório da Aula 2: distribuições, momentos, variância e correlação.
#
# Este script lê a base didática já processada, valida sua estrutura e produz
# tabelas e figuras usadas no laboratório. A unidade de análise é a resolução
# de votação nominal associado a uma resolução da Assembleia Geral da ONU
# entre 2005 e 2012.

options(scipen = 999)
set.seed(20260826)
invisible(try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE))

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(here)
})

arquivo_entrada <- here::here(
  "projeto_agna", "data", "processed", "dados_ensino_agna.csv"
)
diretorio_saida <- here::here("projeto_agna", "output", "aula_02")
dir.create(diretorio_saida, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(arquivo_entrada)) {
  stop(
    "Base didática não encontrada: ", arquivo_entrada,
    ". Execute primeiro projeto_agna/scripts/01_preparar_dados_agna.R."
  )
}

dados <- readr::read_csv(
  arquivo_entrada,
  locale = readr::locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  progress = FALSE
) |>
  dplyr::select(
    resolucao_id,
    simbolo_resolucao,
    ano,
    periodo_2009,
    periodo_fonte,
    tema,
    voto_importante,
    convergente,
    eleitorado_valido,
    percentual_apoio_convergente
  )

colunas_obrigatorias <- c(
  "resolucao_id", "simbolo_resolucao", "ano", "periodo_2009",
  "periodo_fonte", "tema", "voto_importante", "convergente",
  "eleitorado_valido", "percentual_apoio_convergente"
)

if (!identical(names(dados), colunas_obrigatorias)) {
  stop("A ordem ou o conjunto de colunas não coincide com o dicionário esperado.")
}

checagens_logicas <- tibble::tibble(
  checagem = c(
    "A base contém ao menos uma observação",
    "Cada resolucao_id identifica uma única linha",
    "ano está no intervalo documentado de 2005 a 2012",
    "convergente está codificada como 0 ou 1",
    "eleitorado_valido é estritamente positivo quando observado",
    "percentual_apoio_convergente está entre 0 e 100 quando observado",
    "periodo_2009 é compatível com o ano",
    "eleitorado_valido contém números inteiros",
    "ausência em percentual_apoio_convergente é estrutural",
    "tema está preenchido"
  ),
  passou = c(
    nrow(dados) > 0L,
    anyDuplicated(dados$resolucao_id) == 0L,
    all(dados$ano %in% 2005:2012),
    all(dados$convergente %in% c(0L, 1L)),
    all(dados$eleitorado_valido > 0, na.rm = TRUE),
    all(
      dados$percentual_apoio_convergente >= 0 &
        dados$percentual_apoio_convergente <= 100,
      na.rm = TRUE
    ),
    all(
      (dados$ano <= 2008 & dados$periodo_2009 == "2005-2008") |
        (dados$ano >= 2009 & dados$periodo_2009 == "2009-2012")
    ),
    all(dados$eleitorado_valido == round(dados$eleitorado_valido)),
    all(
      is.na(dados$percentual_apoio_convergente) ==
        (dados$convergente == 0L)
    ),
    all(!is.na(dados$tema) & dados$tema != "")
  )
)

if (!all(checagens_logicas$passou)) {
  falhas <- checagens_logicas |>
    dplyr::filter(!passou) |>
    dplyr::pull(checagem)
  stop("Falha nas checagens lógicas: ", paste(falhas, collapse = "; "))
}

tabela_1_validacao <- tibble::tibble(
  indicador = c(
    "Observações",
    "Variáveis",
    "IDs duplicados",
    "Ano mínimo",
    "Ano máximo",
    "Ausências em simbolo_resolucao",
    "Ausências em voto_importante",
    "Ausências em convergente",
    "Ausências em eleitorado_valido",
    "Ausências estruturais em percentual_apoio_convergente"
  ),
  valor = c(
    nrow(dados),
    ncol(dados),
    sum(duplicated(dados$resolucao_id)),
    min(dados$ano),
    max(dados$ano),
    sum(is.na(dados$simbolo_resolucao)),
    sum(is.na(dados$voto_importante)),
    sum(is.na(dados$convergente)),
    sum(is.na(dados$eleitorado_valido)),
    sum(is.na(dados$percentual_apoio_convergente))
  )
)

tabela_2_frequencias_convergencia <- dados |>
  dplyr::count(convergente, name = "frequencia") |>
  dplyr::mutate(
    resultado = dplyr::if_else(
      convergente == 1L,
      "Brasil e China votaram da mesma forma",
      "Brasil e China votaram de forma diferente"
    ),
    proporcao = frequencia / sum(frequencia),
    percentual = 100 * proporcao
  ) |>
  dplyr::select(resultado, convergente, frequencia, proporcao, percentual)

tabela_3_frequencias_ano <- dados |>
  dplyr::count(ano, name = "frequencia") |>
  dplyr::mutate(percentual = 100 * frequencia / sum(frequencia)) |>
  dplyr::select(ano, frequencia, percentual)

tabela_4_frequencias_tema <- dados |>
  dplyr::count(tema, name = "frequencia", sort = TRUE) |>
  dplyr::mutate(percentual = 100 * frequencia / sum(frequencia)) |>
  dplyr::select(tema, frequencia, percentual)

tabela_4b_frequencias_eleitorado <- dados |>
  dplyr::mutate(
    faixa_eleitorado = cut(
      eleitorado_valido,
      breaks = seq(60, 200, by = 10),
      right = FALSE,
      include.lowest = TRUE
    )
  ) |>
  dplyr::count(faixa_eleitorado, .drop = FALSE, name = "frequencia") |>
  dplyr::mutate(percentual = 100 * frequencia / sum(frequencia)) |>
  dplyr::select(faixa_eleitorado, frequencia, percentual)

diagnostico_temas <- tibble::tibble(
  indicador = c(
    "Rótulos temáticos exatos",
    "Registros com mais de um tema",
    "Percentual de registros com mais de um tema"
  ),
  valor = c(
    dplyr::n_distinct(dados$tema),
    sum(grepl(";", dados$tema, fixed = TRUE)),
    100 * mean(grepl(";", dados$tema, fixed = TRUE))
  )
)

variancia_empirica <- function(x) {
  x <- x[!is.na(x)]
  mean((x - mean(x))^2)
}

tabela_5_resumos_numericos <- dplyr::bind_rows(
  dados |>
    dplyr::summarise(
      variavel = "convergente",
      n = sum(!is.na(convergente)),
      ausencias = sum(is.na(convergente)),
      media = mean(convergente, na.rm = TRUE),
      variancia_empirica_n = variancia_empirica(convergente),
      variancia_amostral_n_menos_1 = stats::var(convergente, na.rm = TRUE),
      desvio_padrao_amostral = stats::sd(convergente, na.rm = TRUE),
      minimo = min(convergente, na.rm = TRUE),
      maximo = max(convergente, na.rm = TRUE)
    ),
  dados |>
    dplyr::summarise(
      variavel = "eleitorado_valido",
      n = sum(!is.na(eleitorado_valido)),
      ausencias = sum(is.na(eleitorado_valido)),
      media = mean(eleitorado_valido, na.rm = TRUE),
      variancia_empirica_n = variancia_empirica(eleitorado_valido),
      variancia_amostral_n_menos_1 = stats::var(
        eleitorado_valido,
        na.rm = TRUE
      ),
      desvio_padrao_amostral = stats::sd(eleitorado_valido, na.rm = TRUE),
      minimo = min(eleitorado_valido, na.rm = TRUE),
      maximo = max(eleitorado_valido, na.rm = TRUE)
    )
)

media_convergencia <- mean(dados$convergente)
alvos_constantes <- c(0, 0.5, media_convergencia, 1)

tabela_6_mse <- tibble::tibble(
  alvo_constante = alvos_constantes,
  mse = vapply(
    alvos_constantes,
    function(alvo) mean((dados$convergente - alvo)^2),
    numeric(1)
  )
) |>
  dplyr::mutate(
    alvo = c("Sempre prever 0", "Prever 0,5", "Prever a média", "Sempre prever 1"),
    rmse = sqrt(mse)
  ) |>
  dplyr::select(alvo, alvo_constante, mse, rmse)

tabela_7_covariancia_correlacao <- tibble::tibble(
  par = c(
    "ano × eleitorado_valido",
    "convergente × eleitorado_valido",
    "ano × convergente"
  ),
  covariancia = c(
    stats::cov(dados$ano, dados$eleitorado_valido, use = "complete.obs"),
    stats::cov(
      dados$convergente,
      dados$eleitorado_valido,
      use = "complete.obs"
    ),
    stats::cov(dados$ano, dados$convergente, use = "complete.obs")
  ),
  correlacao = c(
    stats::cor(dados$ano, dados$eleitorado_valido, use = "complete.obs"),
    stats::cor(
      dados$convergente,
      dados$eleitorado_valido,
      use = "complete.obs"
    ),
    stats::cor(dados$ano, dados$convergente, use = "complete.obs")
  )
)

convergencia_por_ano <- dados |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
    resolucoes = dplyr::n(),
    convergentes = sum(convergente),
    proporcao_convergente = mean(convergente),
    .groups = "drop"
  )

figura_1 <- ggplot2::ggplot(
  dados,
  ggplot2::aes(x = eleitorado_valido)
) +
  ggplot2::geom_histogram(
    binwidth = 10,
    boundary = 60,
    color = "white",
    fill = "#3D8DFF"
  ) +
  ggplot2::geom_vline(
    xintercept = mean(dados$eleitorado_valido),
    color = "#D1495B",
    linewidth = 1,
    linetype = "dashed"
  ) +
  ggplot2::annotate(
    "text",
    x = mean(dados$eleitorado_valido) - 2,
    y = Inf,
    label = sprintf(
      "Média = %.1f\nDP = %.1f",
      mean(dados$eleitorado_valido),
      stats::sd(dados$eleitorado_valido)
    ),
    hjust = 1,
    vjust = 1.3,
    size = 4,
    color = "#8B1E2D"
  ) +
  ggplot2::scale_x_continuous(breaks = seq(60, 200, by = 20)) +
  ggplot2::labs(
    title = "Figura 1. Distribuição do eleitorado válido",
    subtitle = "A maioria dos registros mobilizou entre 170 e 189 países",
    x = "Países com voto válido (inclui Brasil e China)",
    y = "Registros de votação nominal",
    caption = paste(
      "Fonte: base didática do projeto AGNU, 2005–2012.",
      "Unidade de análise: registro de votação nominal (rcid)."
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(hjust = 0),
    axis.title = ggplot2::element_text(face = "bold")
  )

figura_2 <- ggplot2::ggplot(
  convergencia_por_ano,
  ggplot2::aes(x = factor(ano), y = proporcao_convergente)
) +
  ggplot2::geom_col(width = 0.72, fill = "#3D8DFF") +
  ggplot2::geom_text(
    ggplot2::aes(
      label = sprintf("%.1f%%\n%d/%d", 100 * proporcao_convergente, convergentes, resolucoes)
    ),
    vjust = -0.25,
    size = 3.6,
    lineheight = 0.9
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1.08),
    breaks = seq(0, 1, by = 0.2),
    labels = function(x) paste0(round(100 * x), "%"),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::labs(
    title = "Figura 2. Convergência dos votos de Brasil e China por ano",
    subtitle = "Percentual e denominador de registros com votos iguais",
    x = "Ano da votação",
    y = "Registros com votos convergentes",
    caption = paste(
      "Fonte: base didática do projeto AGNU, 2005–2012.",
      "Unidade de análise: registro de votação nominal (rcid)."
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(hjust = 0),
    axis.title = ggplot2::element_text(face = "bold")
  )

figura_3 <- ggplot2::ggplot(
  dados,
  ggplot2::aes(x = factor(ano), y = eleitorado_valido)
) +
  ggplot2::geom_boxplot(
    width = 0.64,
    fill = "#D0EDFA",
    color = "#1F4E79",
    outlier.alpha = 0.45
  ) +
  ggplot2::labs(
    title = "Figura 3. Distribuição do eleitorado válido por ano",
    subtitle = "A dispersão muda ao longo do período observado",
    x = "Ano da votação",
    y = "Número de votos válidos dos demais países",
    caption = paste(
      "Fonte: base didática do projeto AGNU, 2005–2012.",
      "Unidade de análise: registro de votação nominal (rcid)."
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(hjust = 0),
    axis.title = ggplot2::element_text(face = "bold")
  )

saidas_csv <- list(
  "tabela_1_validacao.csv" = tabela_1_validacao,
  "tabela_2_frequencias_convergencia.csv" = tabela_2_frequencias_convergencia,
  "tabela_3_frequencias_ano.csv" = tabela_3_frequencias_ano,
  "tabela_4_frequencias_tema.csv" = tabela_4_frequencias_tema,
  "tabela_4b_frequencias_eleitorado.csv" = tabela_4b_frequencias_eleitorado,
  "diagnostico_temas.csv" = diagnostico_temas,
  "tabela_5_resumos_numericos.csv" = tabela_5_resumos_numericos,
  "tabela_6_mse.csv" = tabela_6_mse,
  "tabela_7_covariancia_correlacao.csv" = tabela_7_covariancia_correlacao,
  "checagens_logicas.csv" = checagens_logicas,
  "convergencia_por_ano.csv" = convergencia_por_ano
)

invisible(lapply(names(saidas_csv), function(nome) {
  readr::write_csv(saidas_csv[[nome]], file.path(diretorio_saida, nome), na = "")
}))

ggplot2::ggsave(
  filename = file.path(diretorio_saida, "figura_1_distribuicao_eleitorado.png"),
  plot = figura_1,
  width = 10,
  height = 6,
  dpi = 200,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(diretorio_saida, "figura_2_convergencia_por_ano.png"),
  plot = figura_2,
  width = 10,
  height = 6,
  dpi = 200,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(diretorio_saida, "figura_3_eleitorado_por_ano.png"),
  plot = figura_3,
  width = 10,
  height = 6,
  dpi = 200,
  bg = "white"
)

resumo_execucao <- c(
  paste0("status=PASS"),
  paste0("data_execucao=", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
  paste0("fonte=", arquivo_entrada),
  paste0("unidade_analise=registro de votacao nominal (rcid) associado a resolucao da AGNU"),
  paste0("n_observacoes=", nrow(dados)),
  paste0("n_variaveis=", ncol(dados)),
  paste0("n_convergentes=", sum(dados$convergente == 1L)),
  paste0("n_divergentes=", sum(dados$convergente == 0L)),
  paste0("proporcao_convergente=", format(media_convergencia, digits = 8)),
  paste0("n_ids_duplicados=", sum(duplicated(dados$resolucao_id))),
  paste0("n_missing_simbolo_resolucao=", sum(is.na(dados$simbolo_resolucao))),
  paste0("n_missing_voto_importante=", sum(is.na(dados$voto_importante)))
)

writeLines(
  resumo_execucao,
  con = file.path(diretorio_saida, "resumo_execucao.txt"),
  useBytes = TRUE
)

message("Laboratório da Aula 2 concluído: ", diretorio_saida)
message(
  "Observações: ", nrow(dados),
  "; convergentes: ", sum(dados$convergente == 1L),
  "; divergentes: ", sum(dados$convergente == 0L)
)
