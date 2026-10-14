# Aula 7: verifica a versão de dados observados que inclui Exterior (ZZ).
# Este script verifica cobertura e denominadores. Não ajusta regressões.
# Executar após o script 17 com --incluir-exterior.

library(here)
here::i_am("projeto_agna/scripts/21_validar_dados_com_exterior.R")
pasta <- here::here("projeto_agna", "data", "eleicoes_2022_2026")
ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS",
         "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC",
         "SE", "SP", "TO", "ZZ")
z22 <- read.csv(file.path(pasta, "processed", "presidente_2022_zonas_com_exterior.csv"))
z26 <- read.csv(file.path(pasta, "processed", "presidente_2026_t1_zonas_com_exterior.csv"))
for (d in list(z22, z26)) {
  stopifnot(!anyNA(d), !anyDuplicated(d[c("uf", "zona")]),
            setequal(d$uf, ufs), all(d$validos_t1 > 0),
            sum(d$uf == "ZZ") == 1L)
}
stopifnot(nrow(z22) == 2637L, nrow(z26) == 2641L)

# A parte das 27 UFs conserva os dados observados da preparação anterior.
for (nome in c("presidente_2022_zonas", "presidente_2026_t1_zonas")) {
  antigo <- read.csv(file.path(pasta, "processed", paste0(nome, ".csv")))
  atual <- read.csv(file.path(pasta, "processed", paste0(nome, "_com_exterior.csv")))
  atual <- atual[atual$uf != "ZZ", ]
  rownames(atual) <- NULL
  stopifnot(identical(antigo, atual))
}

# Recalcula as somas do exterior diretamente dos snapshots por seção.
for (caso in list(c("2022", "T1"), c("2022", "T2"), c("2026", "T1"))) {
  ano <- caso[1]
  turno <- tolower(caso[2])
  arquivo <- paste0("section_votes_", ano, "_", caso[2],
                    if (ano == "2026") "_bu" else "", ".parquet")
  s <- arrow::read_parquet(file.path(pasta, "raw", arquivo))
  s <- s[s$uf == "ZZ", ]
  z <- if (ano == "2022") z22[z22$uf == "ZZ", ] else z26[z26$uf == "ZZ", ]
  nome_outro <- if (ano == "2022") "bolsonaro" else "flavio"
  stopifnot(z[[paste0("lula_", turno)]] == sum(s$c13),
            z[[paste0(nome_outro, "_", turno)]] == sum(s$c22),
            z[[paste0("validos_", turno)]] == sum(s$validos),
            z[[paste0("brancos_", turno)]] == sum(s$brancos),
            z[[paste0("nulos_", turno)]] == sum(s$nulos),
            z[[paste0("secoes_", turno)]] == nrow(s),
            z[[paste0("municipios_", turno)]] == length(unique(s$cd_mun)))
  stopifnot(abs(z[[paste0("pct_lula_", turno)]] -
              100 * sum(s$c13) / sum(s$validos)) < 1e-10)
}
stopifnot(all(abs(z22$pct_lula_t2 + z22$pct_bolsonaro_t2 - 100) < 1e-10))

# Total de votos válidos observado em 2026, incluindo o exterior.
jsons <- list.files(file.path(pasta, "raw"), recursive = TRUE, full.names = TRUE,
                    pattern = "presidente_nacional_2026_t1.json$")
oficial <- jsonlite::fromJSON(tail(sort(jsons), 1), simplifyVector = FALSE)
stopifnot(sum(z26$validos_t1) == as.numeric(oficial$v$vv))
relatorio <- c("VALIDACAO_DADOS_COM_EXTERIOR_OK",
  paste("Zonas 2022:", nrow(z22), "| Zonas 2026:", nrow(z26)),
  "Cobertura: 27 UFs e Exterior (ZZ). Uma observação ZZ por ano/turno.",
  "Sem ausências ou duplicidades de chave. Denominadores positivos.",
  "Votos, denominadores e percentuais do exterior conferidos diretamente nas seções.",
  "Dados observados das 27 UFs preservados. Total de votos válidos igual ao TSE.",
  "Validação de dados observados. Não inclui estimação ou previsão eleitoral.")
writeLines(relatorio, file.path(pasta, "docs", "validacao_dados_com_exterior.txt"))
arquivos <- c("processed/presidente_2022_zonas_com_exterior.csv",
              "processed/presidente_2026_t1_zonas_com_exterior.csv",
              "docs/validacao_preparacao_com_exterior.txt",
              "docs/validacao_dados_com_exterior.txt")
hashes <- vapply(file.path(pasta, arquivos), function(f) {
  digest::digest(file = f, algo = "sha256")
}, character(1))
writeLines(paste(hashes, arquivos, sep = "  "),
           file.path(pasta, "checksums_com_exterior.sha256"))
cat(paste(relatorio, collapse = "\n"), "\n")
