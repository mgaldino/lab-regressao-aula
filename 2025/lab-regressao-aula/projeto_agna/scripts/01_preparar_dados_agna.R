# Preparar a base didática do projeto Brasil–China na AGNU.
#
# A unidade de análise é a resolução votada pela Assembleia Geral da ONU.
# O arquivo bruto é uma saída processada do diagnóstico do projeto RDD Trade;
# este script cria uma cópia enxuta para uso no curso e não altera a fonte.

options(scipen = 999)
set.seed(20260825)
invisible(Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8"))

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

arquivo_bruto <- "brazil_china_vote_alignment_by_resolution_2005_2012.csv"

encontrar_raiz <- function() {
  candidatos <- unique(normalizePath(
    c(
      getwd(),
      file.path(getwd(), "projeto_agna"),
      file.path(getwd(), ".."),
      file.path(getwd(), "../..")
    ),
    winslash = "/",
    mustWork = FALSE
  ))

  candidatos <- candidatos[
    file.exists(file.path(candidatos, "data", "raw", arquivo_bruto))
  ]

  if (length(candidatos) == 0L) {
    stop(
      "Não encontrei projeto_agna/data/raw/", arquivo_bruto,
      ". Execute o script a partir do diretório do curso ou de projeto_agna/."
    )
  }

  candidatos[[1L]]
}

raiz <- encontrar_raiz()
arquivo_entrada <- file.path(raiz, "data", "raw", arquivo_bruto)
arquivo_saida <- file.path(raiz, "data", "processed", "dados_ensino_agna.csv")
arquivo_dicionario <- file.path(
  raiz, "data", "processed", "dicionario_variaveis.csv"
)
arquivo_validacao <- file.path(
  raiz, "data", "processed", "validacao_base.csv"
)

dir.create(dirname(arquivo_saida), recursive = TRUE, showWarnings = FALSE)

dados_brutos <- readr::read_csv(
  arquivo_entrada,
  locale = readr::locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  progress = FALSE
)

colunas_obrigatorias <- c(
  "rcid", "doc_symbol", "year", "period", "issue_family",
  "importantvote", "brazil_china_convergent", "valid_electorate_n",
  "pct_same_as_both_when_convergent"
)

if (!all(colunas_obrigatorias %in% names(dados_brutos))) {
  ausentes <- setdiff(colunas_obrigatorias, names(dados_brutos))
  stop("Colunas ausentes na fonte: ", paste(ausentes, collapse = ", "))
}

dados_ensino <- dados_brutos |>
  dplyr::select(
    rcid,
    doc_symbol,
    year,
    period,
    issue_family,
    importantvote,
    brazil_china_convergent,
    valid_electorate_n,
    pct_same_as_both_when_convergent
  ) |>
  dplyr::rename(
    resolucao_id = rcid,
    simbolo_resolucao = doc_symbol,
    ano = year,
    periodo_fonte = period,
    tema = issue_family,
    voto_importante = importantvote,
    convergente_fonte = brazil_china_convergent,
    eleitorado_valido = valid_electorate_n,
    percentual_apoio_convergente = pct_same_as_both_when_convergent
  ) |>
  dplyr::mutate(
    periodo_2009 = dplyr::if_else(
      ano >= 2009L,
      "2009-2012",
      "2005-2008"
    ),
    convergente = as.integer(convergente_fonte),
    tema = stringr::str_squish(as.character(tema)),
    simbolo_resolucao = as.character(simbolo_resolucao),
    voto_importante = as.integer(voto_importante),
    eleitorado_valido = as.numeric(eleitorado_valido)
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

if (anyDuplicated(dados_ensino$resolucao_id) > 0L) {
  stop("A base didática contém resoluções duplicadas.")
}

if (!all(dados_ensino$convergente %in% c(0L, 1L))) {
  stop("A variável convergente não está codificada como 0/1.")
}

if (!all(dados_ensino$ano %in% 2005:2012)) {
  stop("Há anos fora do intervalo documentado de 2005 a 2012.")
}

if (any(dados_ensino$eleitorado_valido <= 0, na.rm = TRUE)) {
  stop("Há eleitorados válidos não positivos.")
}

dicionario <- tibble::tribble(
  ~variavel, ~tipo, ~descricao, ~unidade, ~uso_no_curso,
  "resolucao_id", "inteiro", "Identificador da votação", "resolução", "chave da observação",
  "simbolo_resolucao", "texto", "Símbolo oficial da resolução", "resolução", "identificação e auditoria",
  "ano", "inteiro", "Ano da votação", "ano", "tendência descritiva",
  "periodo_2009", "categórica", "Período 2005-2008 ou 2009-2012", "período", "comparação descritiva",
  "periodo_fonte", "texto", "Período original do diagnóstico", "período", "proveniência",
  "tema", "categórica", "Família temática da resolução", "resolução", "esperança condicional e interação",
  "voto_importante", "binária", "Indicador de voto importante; pode ter ausências", "resolução", "preditor e estratificação",
  "convergente", "binária", "1 se Brasil e China votaram igual; 0 caso contrário", "resolução", "variável resposta principal",
  "eleitorado_valido", "contínua", "Número de votos válidos dos demais países", "resolução", "escala da votação",
  "percentual_apoio_convergente", "contínua", "Percentual de apoio coincidente quando há convergência", "resolução", "extensão descritiva"
)

validacao <- tibble::tribble(
  ~checagem, ~valor,
  "n_observacoes", nrow(dados_ensino),
  "n_variaveis", ncol(dados_ensino),
  "n_resolucoes_convergentes", sum(dados_ensino$convergente == 1L),
  "n_resolucoes_divergentes", sum(dados_ensino$convergente == 0L),
  "ano_minimo", min(dados_ensino$ano),
  "ano_maximo", max(dados_ensino$ano),
  "n_ids_duplicados", sum(duplicated(dados_ensino$resolucao_id)),
  "n_missing_voto_importante", sum(is.na(dados_ensino$voto_importante))
)

readr::write_csv(dados_ensino, arquivo_saida, na = "")
readr::write_csv(dicionario, arquivo_dicionario, na = "")
readr::write_csv(validacao, arquivo_validacao, na = "")

message("Base didática criada: ", arquivo_saida)
message("Observações: ", nrow(dados_ensino), "; variáveis: ", ncol(dados_ensino))
