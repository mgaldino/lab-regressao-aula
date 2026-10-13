# Aula 7: prepara votos presidenciais por zona eleitoral.
# Dados públicos do TSE, preservados em snapshots por seção de 05/10/2026.
# Executar na pasta 2026/lab-regressao-aula, depois do script Python 16.
# Os snapshots incluídos permitem reproduzir esta preparação sem rede.
# Argumento opcional: diretório de origem dos snapshots para a importação.

library(here)
library(dplyr)
here::i_am("projeto_agna/scripts/17_preparar_dados_eleicoes.R")
options(scipen = 999)
diretorio <- here::here("projeto_agna", "data", "eleicoes_2022_2026")
for (pasta in c("raw", "processed", "docs")) {
  dir.create(file.path(diretorio, pasta), recursive = TRUE, showWarnings = FALSE)
}
argumentos <- commandArgs(trailingOnly = TRUE)
origem <- if (length(argumentos)) argumentos[1] else
  "/Users/manoelgaldino/Documents/DCP/Papers/electoralFraud/quality_reports/results/eleicao2026_t1/data"
nomes <- c("section_votes_2022_T1.parquet", "section_votes_2022_T2.parquet",
           "section_votes_2026_T1_bu.parquet", "section_votes_2026_T1_bu_cobertura.csv")
for (nome in nomes) {
  destino <- file.path(diretorio, "raw", nome)
  if (!file.exists(destino)) {
    stopifnot(file.exists(file.path(origem, nome)))
    stopifnot(file.copy(file.path(origem, nome), destino, overwrite = FALSE))
  }
}
secoes <- lapply(nomes[1:3], function(nome) {
  as_tibble(arrow::read_parquet(file.path(diretorio, "raw", nome)))
})
ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS",
         "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC",
         "SE", "SP", "TO")

# Cada seção aparece uma vez: o denominador não é repetido por candidato.
for (d in secoes) {
  candidatos <- dplyr::select(d, matches("^c[0-9]+$"))
  chave <- dplyr::select(d, uf, cd_mun, zona, secao)
  stopifnot(!anyNA(chave), !anyDuplicated(chave), !anyNA(candidatos),
            !anyNA(d$validos), all(as.matrix(candidatos) >= 0),
            all(as.matrix(candidatos) == floor(as.matrix(candidatos))),
            all(rowSums(candidatos) == d$validos),
            all(d$validos + d$brancos + d$nulos == d$comparecimento),
            all(d$comparecimento <= d$aptos),
            all(d$zona > 0), setequal(unique(d$uf), c(ufs, "ZZ")))
}
stopifnot(sum(secoes[[1]]$c13) == 57259504,
          sum(secoes[[1]]$c22) == 51072345,
          sum(secoes[[2]]$c13) == 60345999,
          sum(secoes[[2]]$c22) == 58206354,
          all(secoes[[2]]$c13 + secoes[[2]]$c22 == secoes[[2]]$validos))

# Leitura dos JSONs oficiais preservados pelo script 16.
ler_json <- function(nome) {
  arquivos <- list.files(file.path(diretorio, "raw"),
    pattern = paste0(nome, ".json$"), recursive = TRUE, full.names = TRUE)
  if (!length(arquivos)) stop("Execute antes o script 16 de coleta das fontes: ", nome)
  jsonlite::fromJSON(tail(sort(arquivos), 1), simplifyVector = FALSE)
}
votos_json <- function(j) {
  v <- list()
  for (cargo in j$carg) for (agrupamento in cargo$agr)
    for (partido in agrupamento$par) for (candidato in partido$cand) {
      v[[paste0("c", candidato$n)]] <- as.numeric(candidato$vap)
    }
  v
}
nacional <- ler_json("presidente_nacional_2026_t1")
stopifnot(nacional$t == "1", nacional$ele == "6257", nacional$s$st == nacional$s$ts)
oficiais <- votos_json(nacional)

# O snapshot de BUs tem 15 seções ausentes no Brasil. Elas estão em Betim
# (MG, zona 319) e Carapicuíba (SP, zona 388). O total municipal oficial
# permite completar a soma da zona porque TODAS as seções ausentes de cada
# município pertencem à MESMA zona. Não se imputa voto por seção.
# As listas oficiais identificam as chaves ausentes e impedem rateio ambíguo.
alvos <- tibble(uf = c("MG", "SP"), cd_mun = c(41335L, 63134L),
                zona = c(319L, 388L), nome = c("betim", "carapicuiba"),
                faltantes = c(3L, 12L))
correcoes <- list()
for (i in seq_len(nrow(alvos))) {
  alvo <- alvos[i, ]
  cfg <- ler_json(paste0("secoes_", tolower(alvo$uf), "_2026"))
  esperadas <- list()
  for (a in cfg$abr) for (m in a$mu) if (as.integer(m$cd) == alvo$cd_mun) {
    for (z in m$zon) for (s in z$sec) if (is.null(s$nsp)) {
      esperadas[[length(esperadas) + 1L]] <- tibble(
        uf = toupper(a$cd), cd_mun = as.integer(m$cd),
        zona = as.integer(z$cd), secao = as.integer(s$ns))
    }
  }
  ausentes <- anti_join(bind_rows(esperadas),
    dplyr::select(secoes[[3]], uf, cd_mun, zona, secao),
    by = c("uf", "cd_mun", "zona", "secao"))
  stopifnot(nrow(ausentes) == alvo$faltantes, all(ausentes$zona == alvo$zona))
  municipal <- ler_json(paste0("presidente_", alvo$nome, "_2026_t1"))
  stopifnot(municipal$s$st == municipal$s$ts, municipal$t == "1")
  total_mun <- c(votos_json(municipal), list(validos = as.numeric(municipal$v$vv),
    brancos = as.numeric(municipal$v$vb), nulos = as.numeric(municipal$v$tvn)))
  bu_mun <- filter(secoes[[3]], uf == alvo$uf, cd_mun == alvo$cd_mun)
  delta <- vapply(names(total_mun), function(coluna) {
    total_mun[[coluna]] - sum(bu_mun[[coluna]])
  }, numeric(1))
  stopifnot(all(delta >= 0), sum(delta[grepl("^c[0-9]+$", names(delta))]) == delta[["validos"]])
  correcoes[[i]] <- bind_cols(dplyr::select(alvo, uf, cd_mun, zona, faltantes),
                             as_tibble(as.list(delta)))
}
correcoes <- bind_rows(correcoes)
write.csv(correcoes, file.path(diretorio, "docs", "complementos_totais_de_zona_2026.csv"),
          row.names = FALSE)
for (coluna in names(oficiais)) {
  stopifnot(sum(secoes[[3]][[coluna]]) + sum(correcoes[[coluna]]) == oficiais[[coluna]])
}
stopifnot(sum(secoes[[3]]$validos) + sum(correcoes$validos) == as.numeric(nacional$v$vv))

# Zona é identificada por UF + número. Agrega todas as seções e municípios
# abrangidos pela zona. Somar porcentagens municipais daria outro resultado.
agregar <- function(d, turno) {
  z <- d |>
    filter(uf %in% ufs) |>
    group_by(uf, zona) |>
    summarise(lula = sum(c13), adversario = sum(c22), validos = sum(validos),
              brancos = sum(brancos), nulos = sum(nulos),
              secoes = n(), municipios = n_distinct(cd_mun), .groups = "drop") |>
    arrange(uf, zona)
  stopifnot(!anyNA(z), all(z$validos > 0),
            sum(z$lula) == sum(d$c13[d$uf %in% ufs]),
            sum(z$validos) == sum(d$validos[d$uf %in% ufs]))
  names(z)[-(1:2)] <- paste0(names(z)[-(1:2)], "_", turno)
  z
}
z1 <- agregar(secoes[[1]], "t1")
z2 <- agregar(secoes[[2]], "t2")
stopifnot(nrow(anti_join(z1, z2, by = c("uf", "zona"))) == 0,
          nrow(anti_join(z2, z1, by = c("uf", "zona"))) == 0)
treino <- inner_join(z1, z2, by = c("uf", "zona")) |>
  rename(bolsonaro_t1 = adversario_t1, bolsonaro_t2 = adversario_t2) |>
  mutate(pct_lula_t1 = 100 * lula_t1 / validos_t1,
         pct_lula_t2 = 100 * lula_t2 / validos_t2,
         pct_bolsonaro_t1 = 100 * bolsonaro_t1 / validos_t1,
         pct_bolsonaro_t2 = 100 * bolsonaro_t2 / validos_t2)
z2026 <- agregar(secoes[[3]], "t1")
z2026$secoes_completadas_por_total_municipal_t1 <- 0L
for (i in seq_len(nrow(correcoes))) {
  c <- correcoes[i, ]
  linha <- which(z2026$uf == c$uf & z2026$zona == c$zona)
  stopifnot(length(linha) == 1L)
  for (par in list(c("lula_t1", "c13"), c("adversario_t1", "c22"),
                  c("validos_t1", "validos"), c("brancos_t1", "brancos"),
                  c("nulos_t1", "nulos"), c("secoes_t1", "faltantes"))) {
    z2026[[par[1]]][linha] <- z2026[[par[1]]][linha] + c[[par[2]]]
  }
  z2026$secoes_completadas_por_total_municipal_t1[linha] <- c$faltantes
}
aplicacao <- z2026 |>
  rename(flavio_t1 = adversario_t1) |>
  mutate(pct_lula_t1 = 100 * lula_t1 / validos_t1,
         pct_flavio_t1 = 100 * flavio_t1 / validos_t1)
for (d in list(treino, aplicacao)) {
  stopifnot(!anyDuplicated(d[c("uf", "zona")]), !anyNA(d),
            setequal(d$uf, ufs),
            all(as.matrix(dplyr::select(d, starts_with("pct_"))) >= 0),
            all(as.matrix(dplyr::select(d, starts_with("pct_"))) <= 100))
}
stopifnot(all(abs(treino$pct_lula_t2 + treino$pct_bolsonaro_t2 - 100) < 1e-10))
write.csv(treino, file.path(diretorio, "processed", "presidente_2022_zonas.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(aplicacao, file.path(diretorio, "processed", "presidente_2026_t1_zonas.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
chaves <- full_join(mutate(dplyr::select(treino, uf, zona), em_2022 = TRUE),
                   mutate(dplyr::select(aplicacao, uf, zona), em_2026 = TRUE),
                   by = c("uf", "zona")) |>
  mutate(em_2022 = coalesce(em_2022, FALSE), em_2026 = coalesce(em_2026, FALSE))
write.csv(filter(chaves, !em_2022 | !em_2026),
          file.path(diretorio, "docs", "zonas_com_chave_em_apenas_um_ano.csv"), row.names = FALSE)
cobertura <- read.csv(file.path(diretorio, "raw", nomes[4]))
stopifnot(all(cobertura$selecionadas + cobertura$sem_bu == cobertura$secoes_lista),
          all(cobertura$conflitos == 0), all(cobertura$erros_decodificacao == 0))
relatorio <- c(
  "VALIDACAO_DADOS_ELEITORAIS_OK",
  paste("Zonas 2022:", nrow(treino), "| Zonas 2026:", nrow(aplicacao), "| UFs: 27"),
  "Unidade: UF + zona eleitoral. Exterior (ZZ) excluído das entradas do laboratório.",
  "2022: chaves de zona iguais nos dois turnos, sem perdas no cruzamento.",
  "2026: após complemento de duas zonas, os 12 totais de candidatos e votos válidos igualam o JSON nacional.",
  paste("Seções sem BU no recibo de 2026:", sum(cobertura$sem_bu[cobertura$uf != "ZZ"]),
        "no Brasil e", sum(cobertura$sem_bu[cobertura$uf == "ZZ"]), "no exterior."),
  "As 15 seções do Brasil ausentes nos BUs somam 3.935 votos válidos, recuperados sem rateio entre zonas.",
  "Os complementos provêm de totais municipais oficiais. As listas de seções fixam sua única zona possível.",
  paste("Chaves presentes em apenas um ano:", sum(!chaves$em_2022 | !chaves$em_2026)),
  "Coincidência de código entre anos não garante território idêntico. Não é um painel de zonas fixas.")
writeLines(relatorio, file.path(diretorio, "docs", "validacao_preparacao.txt"))
cat(paste(relatorio, collapse = "\n"), "\n")
