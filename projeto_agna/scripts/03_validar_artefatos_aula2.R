# Validação final e manifesto dos artefatos da Aula 2.

options(scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
})

raiz_repo <- here::here()
raiz_projeto <- file.path(raiz_repo, "projeto_agna")
diretorio_processado <- file.path(raiz_projeto, "data", "processed")
diretorio_saida <- file.path(raiz_projeto, "output", "aula_02")
diretorio_pdf <- file.path(raiz_projeto, "output", "pdf")

falhar <- function(mensagem) {
  stop(mensagem, call. = FALSE)
}

assert_true <- function(condicao, mensagem) {
  if (!isTRUE(condicao)) {
    falhar(mensagem)
  }
}

arquivo_painel <- file.path(
  diretorio_processado,
  "painel_pais_votacao_1997_2016.csv.gz"
)
arquivo_brasil <- file.path(
  diretorio_processado,
  "brasil_convergencia_china_1997_2016.csv"
)
arquivo_unidades <- file.path(
  diretorio_processado,
  "unidades_autoritativas_1997_2016.csv"
)
pdf_roteiro <- file.path(
  diretorio_pdf,
  "laboratorio_aula_02_convergencia_brasil_china.pdf"
)
pdf_slides <- file.path(
  diretorio_pdf,
  "slides_aula_02_convergencia_brasil_china.pdf"
)

arquivos_minimos <- c(
  arquivo_painel,
  arquivo_brasil,
  arquivo_unidades,
  pdf_roteiro,
  pdf_slides
)
assert_true(
  all(file.exists(arquivos_minimos)),
  "Faltam arquivos mínimos para a validação final."
)

painel <- readr::read_csv(
  arquivo_painel,
  locale = readr::locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  progress = FALSE
)
brasil <- readr::read_csv(
  arquivo_brasil,
  locale = readr::locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  progress = FALSE
)
unidades <- readr::read_csv(
  arquivo_unidades,
  locale = readr::locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  progress = FALSE
)

resumo_periodo <- brasil |>
  dplyr::group_by(periodo_2009) |>
  dplyr::summarise(
    n = dplyr::n(),
    taxa = mean(convergente),
    .groups = "drop"
  ) |>
  dplyr::select(periodo_2009, n, taxa)

taxa_pre <- resumo_periodo |>
  dplyr::filter(periodo_2009 == "1997-2008") |>
  dplyr::pull(taxa)
taxa_pos <- resumo_periodo |>
  dplyr::filter(periodo_2009 == "2009-2016") |>
  dplyr::pull(taxa)

assert_true(nrow(unidades) == 96L, "O universo não contém 96 unidades.")
assert_true(
  sum(unidades$pais_iso3 == "BRA") == 1L,
  "O Brasil não aparece exatamente uma vez na lista."
)
assert_true(
  sum(unidades$donor_pool == 1L) == 95L,
  "A lista não contém exatamente 95 unidades do donor pool."
)
assert_true(
  sum(unidades$pais_iso3 == "CHN") == 0L,
  "A China não deve ser uma das 96 unidades."
)
assert_true(nrow(painel) == 174048L, "A grade país × votação está incompleta.")
assert_true(
  dplyr::n_distinct(painel$rcid) == 1813L,
  "O número de votações na janela está incorreto."
)
assert_true(
  dplyr::n_distinct(painel$ano) == 20L &&
    min(painel$ano) == 1997L && max(painel$ano) == 2016L,
  "A janela temporal não é 1997–2016."
)
assert_true(
  sum(duplicated(painel[c("pais_iso3", "rcid")])) == 0L,
  "Há chaves país × votação duplicadas."
)
assert_true(nrow(brasil) == 1762L, "A base do laboratório não tem 1.762 pares.")
assert_true(
  sum(duplicated(brasil$rcid)) == 0L,
  "Há votações duplicadas na base do laboratório."
)
assert_true(
  sum(is.na(brasil$convergente)) == 0L &&
    all(brasil$convergente %in% c(0L, 1L)),
  "O resultado contém ausência ou valor fora de 0/1."
)
assert_true(
  sum(brasil$convergente == 1L) == 1412L &&
    sum(brasil$convergente == 0L) == 350L,
  "As frequências de convergência não coincidem com o resultado validado."
)
assert_true(
  abs(mean(brasil$convergente) - 0.801362088536) < 1e-12,
  "A média geral de convergência mudou."
)
assert_true(
  abs(taxa_pre - 0.786111111111) < 1e-12 &&
    abs(taxa_pos - 0.825513196481) < 1e-12,
  "As taxas pré/pós-2009 mudaram."
)

assert_true(nzchar(Sys.which("pdfinfo")), "O programa pdfinfo não está disponível.")
assert_true(nzchar(Sys.which("pdftotext")), "O programa pdftotext não está disponível.")

numero_paginas <- function(arquivo) {
  info <- system2("pdfinfo", arquivo, stdout = TRUE, stderr = TRUE)
  linha <- grep("^Pages:", info, value = TRUE)
  as.integer(sub("^Pages:[[:space:]]*", "", linha))
}

assert_true(numero_paginas(pdf_roteiro) == 9L, "O roteiro PDF não tem 9 páginas.")
assert_true(numero_paginas(pdf_slides) == 20L, "Os slides PDF não têm 20 páginas.")

termo_excluido <- paste0("eleitorado", "_", "valido")
padrao_janela_rejeitada <- paste0("2005", ".{0,3}", "2012")

arquivos_texto <- list.files(
  raiz_projeto,
  pattern = "\\.(R|Rmd|md|csv|txt|tex)$",
  recursive = TRUE,
  full.names = TRUE
)
arquivos_texto <- arquivos_texto[!grepl("/tmp/", arquivos_texto, fixed = TRUE)]

conteudo_texto <- unlist(
  lapply(
    arquivos_texto,
    readLines,
    warn = FALSE,
    encoding = "UTF-8"
  ),
  use.names = FALSE
)

assert_true(
  !any(grepl(termo_excluido, conteudo_texto, fixed = TRUE)),
  "Um termo excluído ainda aparece nos artefatos textuais."
)
assert_true(
  !any(grepl(padrao_janela_rejeitada, conteudo_texto, perl = TRUE)),
  "A janela rejeitada ainda aparece nos artefatos textuais."
)

texto_pdfs <- unlist(
  lapply(
    c(pdf_roteiro, pdf_slides),
    function(arquivo) {
      system2("pdftotext", c(arquivo, "-"), stdout = TRUE, stderr = TRUE)
    }
  ),
  use.names = FALSE
)

assert_true(
  !any(grepl(termo_excluido, texto_pdfs, fixed = TRUE)),
  "Um termo excluído ainda aparece nos PDFs."
)
assert_true(
  !any(grepl(padrao_janela_rejeitada, texto_pdfs, perl = TRUE)),
  "A janela rejeitada ainda aparece nos PDFs."
)

artefatos_relativos <- c(
  "README.md",
  "scripts/01_construir_painel_agna.R",
  "scripts/02_analise_descritiva_aula2.R",
  "scripts/03_validar_artefatos_aula2.R",
  "laboratorios/aula_02_convergencia_brasil_china.Rmd",
  "laboratorios/slides_aula_02_convergencia_brasil_china.Rmd",
  "laboratorios/roteiro_laboratorios.md",
  "data/processed/painel_pais_votacao_1997_2016.csv.gz",
  "data/processed/painel_pais_ano_1997_2016.csv",
  "data/processed/brasil_convergencia_china_1997_2016.csv",
  "data/processed/unidades_autoritativas_1997_2016.csv",
  "data/processed/dicionario_variaveis.csv",
  "data/processed/manifesto_dados.csv",
  "data/processed/SHA256SUMS",
  "output/aula_02/tabela_1_validacao.csv",
  "output/aula_02/tabela_2_frequencias_convergencia.csv",
  "output/aula_02/tabela_3_resumo_pre_pos_2009.csv",
  "output/aula_02/tabela_4_convergencia_anual.csv",
  "output/aula_02/tabela_5_momentos.csv",
  "output/aula_02/tabela_6_covariancia_correlacao.csv",
  "output/aula_02/tabela_7_frequencias_votos_brasil_china.csv",
  "output/aula_02/tabela_8_mse.csv",
  "output/aula_02/figura_1_convergencia_anual.png",
  "output/aula_02/figura_2_convergencia_pre_pos_2009.png",
  "output/aula_02/manifesto_outputs.csv",
  "output/aula_02/resumo_execucao.txt",
  "output/pdf/laboratorio_aula_02_convergencia_brasil_china.pdf",
  "output/pdf/slides_aula_02_convergencia_brasil_china.pdf"
)

artefatos_absolutos <- file.path(raiz_projeto, artefatos_relativos)
assert_true(
  all(file.exists(artefatos_absolutos)),
  "Um ou mais artefatos esperados não existem."
)

sha256_arquivo <- function(arquivo) {
  saida <- suppressWarnings(
    system2(
      "shasum",
      c("-a", "256", arquivo),
      stdout = TRUE,
      stderr = FALSE,
      env = c("LC_ALL=pt_BR.UTF-8", "LANG=pt_BR.UTF-8")
    )
  )
  hash <- sub("[[:space:]].*$", "", saida)
  if (length(hash) != 1L || !grepl("^[0-9a-f]{64}$", hash)) {
    falhar(paste0("Hash SHA-256 inválido para: ", arquivo))
  }
  hash
}

manifesto_artefatos <- tibble::tibble(
  arquivo = artefatos_relativos,
  tipo = dplyr::case_when(
    grepl("^scripts/", arquivo) ~ "script R",
    grepl("^laboratorios/.*\\.Rmd$", arquivo) ~ "fonte Rmd",
    grepl("^output/pdf/", arquivo) ~ "PDF",
    grepl("\\.png$", arquivo) ~ "figura",
    grepl("\\.csv", arquivo) ~ "dados ou tabela",
    TRUE ~ "documentação ou checksum"
  ),
  bytes = file.info(artefatos_absolutos)$size,
  sha256 = vapply(artefatos_absolutos, sha256_arquivo, character(1)),
  status = "PASS"
) |>
  dplyr::select(arquivo, tipo, bytes, sha256, status)

arquivo_manifesto <- file.path(diretorio_saida, "manifesto_artefatos.csv")
readr::write_csv(manifesto_artefatos, arquivo_manifesto, na = "")

arquivos_checksum <- c(artefatos_relativos, "output/aula_02/manifesto_artefatos.csv")
hashes_checksum <- vapply(
  file.path(raiz_projeto, arquivos_checksum),
  sha256_arquivo,
  character(1)
)
linhas_checksum <- paste(hashes_checksum, arquivos_checksum, sep = "  ")
writeLines(
  linhas_checksum,
  file.path(diretorio_saida, "SHA256SUMS_ARTEFATOS"),
  useBytes = TRUE
)

cat("PASS: artefatos da Aula 2 validados.\n")
cat("Universo: 96 unidades totais = Brasil + 95 donors.\n")
cat("Janela: 1997-2016; corte: pré/pós-2009.\n")
cat("Roteiro PDF: 9 páginas; slides Beamer: 20 páginas.\n")
