# Tabela de votos por seção (Presidente), formato largo, a partir dos arquivos oficiais do TSE.
# Uso: Rscript --vanilla R/eleicao2026/06b_section_votes.R <ano: 2022|2026> <turno: 1|2> [fonte 2026: oficial|bu] [dir bu]
# Saída: quality_reports/results/eleicao2026_t1/data/section_votes_<ano>_T<turno>.parquet
#
# 2022: detalhe por seção do arquivo bruto (data/raw/tse/2022/section_results/*/detalhe_votacao_secao_2022.zip) e votos por
#       candidato de data/processed/brasil_2022_secao.parquet.
# 2026: votacao_secao_2026_BR.zip e detalhe_votacao_secao_2026.zip (data/raw/tse/2026/section_results/<UTC>/), do recibo
#       mais recente com status "downloaded". O layout de 2026 é conferido contra as colunas usadas antes de ler.
# 2026, fonte "bu": boletins de urna da API de resultados decodificados pela sessão de coleta (electoralfraud-d5) em
#       data/processed/tse_2026_bu_R1/<rodada>/decode_<UTC>/ (uma pasta por execução do decodificador). Por UF, usa a
#       decodificação mais recente com completeness = TRUE (sections_<UF>.csv, votes_<UF>.csv da mesma pasta do summary);
#       para se faltar UF (salvo ALLOW_PARTIAL=1) ou se houver conflitos, erros de decodificação ou soma de votos
#       diferente do comparecimento (salvo ALLOW_INCONSISTENT=1). Grava a cobertura em *_cobertura.csv.
#       Votos no número 28 (PRTB, candidatura sem votos válidos) contam como nulos, como na divulgação oficial.
# Seções agregadas: os votos já vêm na seção que recebe; nada a fazer aqui.

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(jsonlite)
})
setDTthreads(4)
args <- commandArgs(trailingOnly = TRUE)
ano <- as.integer(args[1])
turno <- as.integer(args[2])
fonte <- if (length(args) >= 3) args[3] else "oficial"
root <- getwd()
out <- file.path(root, sprintf("quality_reports/results/eleicao2026_t1/data/section_votes_%d_T%d.parquet", ano, turno))
det_cols <- c("NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO", "CD_CARGO", "QT_APTOS", "QT_COMPARECIMENTO",
              "QT_ABSTENCOES", "QT_VOTOS_NOMINAIS", "QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS", "NR_LOCAL_VOTACAO")

# procedência: hora UTC e caminho de cada leitura de dados de 2026
log_access <- function(what, level, where) {
  f <- file.path(root, "quality_reports/results/eleicao2026_t1/access_log.md")
  cat(sprintf("| %s | %s | %s | %s |\n", format(Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC"), what, level, where),
      file = f, append = TRUE)
}
read_member <- function(z, member, cols) {
  hdr <- names(fread(cmd = paste("unzip -p", shQuote(z), shQuote(member), "| head -2"), encoding = "Latin-1"))
  miss <- setdiff(cols, hdr)
  if (length(miss)) stop("colunas ausentes em ", member, ": ", paste(miss, collapse = ", "))
  fread(cmd = paste("unzip -p", shQuote(z), shQuote(member)), encoding = "Latin-1", select = cols)
}
pick_member <- function(z, regex) {
  m <- grep(regex, unzip(z, list = TRUE)$Name, value = TRUE)
  if (length(m) != 1) stop("membro ambíguo ou ausente em ", z, ": ", regex, " -> ", paste(m, collapse = ", "))
  m
}

if (ano == 2022) {
  z <- sort(Sys.glob(file.path(root, "data/raw/tse/2022/section_results/*/detalhe_votacao_secao_2022.zip")), decreasing = TRUE)[1]
  det <- read_member(z, "detalhe_votacao_secao_2022_BRASIL.csv", det_cols)
  v <- as.data.table(read_parquet(file.path(root, "data/processed/brasil_2022_secao.parquet"),
                                  col_select = c("NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO", "CD_CARGO",
                                                 "NR_VOTAVEL", "QT_VOTOS")))
} else if (fonte == "bu") {
  # Decodificador v2: data/processed/tse_2026_bu_R1/<rodada>/decode_<UTC>/ (uma pasta por execução, só com as UFs dela).
  # Por UF, usa a decodificação mais recente com completeness = TRUE e lê sections/votes da mesma pasta do summary.
  # Sem argumento, a rodada é a última (em ordem alfabética) entre as pastas que têm decode_*; share/ e tse_layout/ ficam de fora.
  rodadas <- unique(dirname(Sys.glob(file.path(root, "data/processed/tse_2026_bu_R1/*/decode_*"))))
  run_dir <- if (length(args) >= 4) args[4] else sort(rodadas, decreasing = TRUE)[1]
  sm <- Sys.glob(file.path(run_dir, "decode_*", "summary_*.json"))
  if (!length(sm)) stop("nenhum summary_<UF>.json em ", run_dir, "/decode_*/")
  cob <- rbindlist(lapply(sm, function(f) {
    j <- fromJSON(f)
    hs <- j$hash_st
    data.table(uf = j$uf, pasta = dirname(f), completeness = isTRUE(j$completeness),
               secoes_lista = j$non_aggregated_sections, selecionadas = j$selected, sem_bu = j$no_bu,
               conflitos = j$conflict_sections, erros_decodificacao = j$decode_errors,
               totalizado = if (is.null(hs$Totalizado)) 0L else hs$Totalizado,
               recebido = if (is.null(hs$Recebido)) 0L else hs$Recebido,
               pres_soma_dif_comparecimento = j$pres_sum_ne_comp, decoder_sha256 = j$decoder_sha256)
  }))
  cob <- cob[completeness == TRUE][order(uf, -pasta)][, .SD[1], by = uf]
  todas <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ",
             "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO", "ZZ")
  falta <- setdiff(todas, cob$uf)
  cat("BU decodificados:", run_dir, "| UFs completas:", nrow(cob), "| faltam:", paste(falta, collapse = " "), "\n")
  if (length(falta) && !identical(Sys.getenv("ALLOW_PARTIAL"), "1")) stop("UFs sem decodificação completa; ALLOW_PARTIAL=1 para seguir")
  if (cob[, any(conflitos > 0 | erros_decodificacao > 0 | pres_soma_dif_comparecimento > 0)] &&
      !identical(Sys.getenv("ALLOW_INCONSISTENT"), "1")) {
    print(cob[conflitos > 0 | erros_decodificacao > 0 | pres_soma_dif_comparecimento > 0])
    stop("conflitos, erros de decodificação ou soma de votos != comparecimento; ALLOW_INCONSISTENT=1 para seguir")
  }
  log_access(sprintf("Leitura dos votos de 2026 por seção (06b, boletins decodificados, Presidente), %d UFs", nrow(cob)), "seção",
             paste(unique(sub(paste0(root, "/"), "", cob$pasta)), collapse = ", "))
  sec_cols <- c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO", "NR_LOCAL_VOTACAO", "F_QT_APTOS", "F_QT_COMPARECIMENTO", "HASH_ST")
  vot_cols <- c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO", "DS_CARGO", "TP_VOTO", "NR_VOTAVEL", "QT_VOTOS")
  rd <- function(f, cols) {
    hdr <- names(fread(f, nrows = 0))
    miss <- setdiff(cols, hdr)
    if (length(miss)) stop("colunas ausentes em ", f, ": ", paste(miss, collapse = ", "))
    fread(f, select = cols, encoding = "UTF-8")
  }
  sec <- rbindlist(lapply(seq_len(nrow(cob)), function(i) rd(file.path(cob$pasta[i], sprintf("sections_%s.csv", cob$uf[i])), sec_cols)))
  vv <- rbindlist(lapply(seq_len(nrow(cob)), function(i) rd(file.path(cob$pasta[i], sprintf("votes_%s.csv", cob$uf[i])), vot_cols)))
  vv <- vv[tolower(DS_CARGO) == "presidente"]
  cat("tipos de voto (Presidente):\n")
  print(vv[, .(votos = sum(QT_VOTOS)), by = .(TP_VOTO, nulo_tecnico = NR_VOTAVEL == 28)])
  cat("status dos boletins:\n")
  print(sec[, .N, by = HASH_ST])
  vv[, classe := fifelse(NR_VOTAVEL == 95, "branco", fifelse(NR_VOTAVEL %in% c(96, 28) | tolower(TP_VOTO) == "nulo", "nulo", "nominal"))]
  agg <- dcast(vv, SG_UF + CD_MUNICIPIO + NR_ZONA + NR_SECAO ~ classe, value.var = "QT_VOTOS", fun.aggregate = sum, fill = 0)
  for (cl in c("branco", "nulo", "nominal")) if (!cl %in% names(agg)) agg[, (cl) := 0]
  det <- merge(sec, agg, by = c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO"), all.x = TRUE)
  det[, `:=`(NR_TURNO = turno, CD_CARGO = 1L, QT_APTOS = F_QT_APTOS, QT_COMPARECIMENTO = F_QT_COMPARECIMENTO,
             QT_ABSTENCOES = F_QT_APTOS - F_QT_COMPARECIMENTO, QT_VOTOS_NOMINAIS = fcoalesce(as.numeric(nominal), 0),
             QT_VOTOS_BRANCOS = fcoalesce(as.numeric(branco), 0), QT_VOTOS_NULOS = fcoalesce(as.numeric(nulo), 0))]
  det <- det[, ..det_cols]
  v <- vv[classe == "nominal", .(NR_TURNO = turno, SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO, CD_CARGO = 1L, NR_VOTAVEL, QT_VOTOS)]
  out <- sub("\\.parquet$", "_bu.parquet", out)
  cob[, ufs_faltando := paste(falta, collapse = " ")]
  fwrite(cob, sub("\\.parquet$", "_cobertura.csv", out))
} else {
  zs <- Sys.glob(file.path(root, "data/raw/tse/2026/section_results/*/detalhe_votacao_secao_2026.zip"))
  zv <- Sys.glob(file.path(root, "data/raw/tse/2026/section_results/*/votacao_secao_2026_BR.zip"))
  if (!length(zs) || !length(zv)) stop("arquivos de 2026 por seção ainda não baixados")
  pick_latest <- function(paths) {
    fin <- vapply(paths, function(p) {
      r <- fromJSON(file.path(dirname(p), "receipt.json"))
      if (!identical(r$status, "downloaded")) NA_character_ else r$access_finished_utc
    }, character(1))
    paths[order(fin, decreasing = TRUE, na.last = NA)][1]
  }
  zs <- pick_latest(zs)
  zv <- pick_latest(zv)
  det <- read_member(zs, pick_member(zs, "BR(ASIL)?\\.csv$"), det_cols)
  v <- read_member(zv, pick_member(zv, "\\.csv$"),
                   c("NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO", "CD_CARGO", "NR_VOTAVEL", "QT_VOTOS"))
  cat("fontes:", zs, zv, "\n")
  log_access("Leitura dos arquivos oficiais de 2026 por seção (Presidente)", "seção", paste0("`", sub(paste0(root, "/"), "", c(zs, zv)), "`", collapse = ", "))
}

det <- det[CD_CARGO == 1 & NR_TURNO == turno]
v <- v[CD_CARGO == 1 & NR_TURNO == turno]
key <- c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")
for (k in c("CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")) {
  set(det, j = k, value = as.integer(det[[k]]))
  set(v, j = k, value = as.integer(v[[k]]))
}
stopifnot(det[, .N, by = key][, all(N == 1)])
# 95 branco, 96 nulo; 28 (PRTB, candidatura sem votos válidos) é nulo técnico e já está em QT_VOTOS_NULOS no detalhe
# oficial; aqui ele sai das colunas de candidatos. Qualquer outra inconsistência para o script (abaixo).
vw <- dcast(v[!NR_VOTAVEL %in% c(95, 96, 28)], SG_UF + CD_MUNICIPIO + NR_ZONA + NR_SECAO ~ paste0("c", NR_VOTAVEL),
            value.var = "QT_VOTOS", fun.aggregate = sum, fill = 0)
s <- merge(det, vw, by = key, all.x = TRUE)
cc <- grep("^c[0-9]+$", names(s), value = TRUE)
for (j in cc) set(s, which(is.na(s[[j]])), j, 0)
s[, validos := rowSums(.SD), .SDcols = cc]
bad <- s[validos != QT_VOTOS_NOMINAIS | QT_COMPARECIMENTO != QT_VOTOS_NOMINAIS + QT_VOTOS_BRANCOS + QT_VOTOS_NULOS]
cat("seções:", nrow(s), " inconsistentes (válidos ou comparecimento):", nrow(bad), "\n")
if (nrow(bad) && !identical(Sys.getenv("ALLOW_INCONSISTENT"), "1")) {
  print(head(bad))
  stop("seções com válidos != nominais ou comparecimento != nominais + brancos + nulos; ALLOW_INCONSISTENT=1 para seguir")
}
cat("seções com votos sem detalhe:", nrow(vw[!det, on = key]), "\n")
setnames(s, c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO", "NR_LOCAL_VOTACAO", "QT_APTOS", "QT_COMPARECIMENTO",
              "QT_ABSTENCOES", "QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS"),
         c("uf", "cd_mun", "zona", "secao", "local", "aptos", "comparecimento", "abstencao", "brancos", "nulos"))
s[, c("NR_TURNO", "CD_CARGO", "QT_VOTOS_NOMINAIS") := NULL]
write_parquet(s, out)
tot <- s[, lapply(.SD, sum), .SDcols = c("aptos", "comparecimento", "abstencao", "validos", "brancos", "nulos", cc)]
print(tot)
cat("->", out, "\n")
