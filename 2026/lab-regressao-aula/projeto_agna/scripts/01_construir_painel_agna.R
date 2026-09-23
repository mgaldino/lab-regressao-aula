# Reconstruir o painel pais x votacao da AGNU para 1997-2016.
#
# Fontes preservadas:
# - tarball do pacote unvotes 0.3.0;
# - synth_data.rds, usado como lista autoritativa de unidades e como fonte das
#   covariaveis anuais do desenho do paper.
#
# A lista autoritativa contem 96 unidades totais: Brasil e 95 paises do donor
# pool. A China nao integra essa lista; seu voto e usado como referencia para
# calcular a convergencia direta em cada votacao.

options(scipen = 999)

suppressPackageStartupMessages({
  library(countrycode)
  library(digest)
  library(dplyr)
  library(here)
  library(readr)
  library(stringr)
  library(tidyr)
})

raiz_repo <- here::here()
raiz_projeto <- file.path(raiz_repo, "projeto_agna")

arquivo_unvotes <- file.path(
  raiz_projeto,
  "data", "raw", "unvotes", "unvotes_0.3.0.tar.gz"
)
arquivo_synth <- file.path(
  raiz_projeto,
  "data", "raw", "donor_pool", "synth_data.rds"
)
diretorio_processado <- file.path(raiz_projeto, "data", "processed")

dir.create(diretorio_processado, recursive = TRUE, showWarnings = FALSE)

hash_arquivo <- function(caminho) {
  digest::digest(caminho, algo = "sha256", file = TRUE, serialize = FALSE)
}

hashes_esperados <- c(
  unvotes = "5c826e9ab6d6aa6bb52fe936050783e2ce5ba879166ba0444fe2f6e530dfdeed",
  synth = "c71d3257d125888808119eb9ef20118b78fa1b3bde341742de5a28a503becbc4"
)

if (!file.exists(arquivo_unvotes) || !file.exists(arquivo_synth)) {
  stop("As duas fontes preservadas nao foram encontradas.")
}

hashes_observados <- c(
  unvotes = hash_arquivo(arquivo_unvotes),
  synth = hash_arquivo(arquivo_synth)
)

if (!identical(unname(hashes_observados), unname(hashes_esperados))) {
  stop("Os bytes de uma ou mais fontes diferem dos hashes autoritativos.")
}

# O tarball e extraido somente em diretorio temporario. A fonte preservada nao
# e alterada e nao e necessario instalar o pacote unvotes.
diretorio_temporario <- tempfile("unvotes_0_3_0_")
dir.create(diretorio_temporario)
on.exit(unlink(diretorio_temporario, recursive = TRUE, force = TRUE), add = TRUE)

utils::untar(arquivo_unvotes, exdir = diretorio_temporario)

ambiente_unvotes <- new.env(parent = emptyenv())
for (nome_arquivo in c(
  "un_votes.rda",
  "un_roll_calls.rda",
  "un_roll_call_issues.rda"
)) {
  load(
    file.path(
      diretorio_temporario,
      "unvotes", "data", nome_arquivo
    ),
    envir = ambiente_unvotes
  )
}

objetos_necessarios <- c(
  "un_votes", "un_roll_calls", "un_roll_call_issues"
)
if (!all(objetos_necessarios %in% ls(ambiente_unvotes))) {
  stop("O tarball nao contem todos os objetos esperados do pacote unvotes.")
}

un_votes <- ambiente_unvotes$un_votes
un_roll_calls <- ambiente_unvotes$un_roll_calls
un_roll_call_issues <- ambiente_unvotes$un_roll_call_issues
synth_data <- readRDS(arquivo_synth)

colunas_synth <- c(
  "year", "iso3c", "treatment", "abs_distance_china", "gpi",
  "abs_distance_usa", "perc_trade_with_us", "perc_trade_with_china",
  "pci_cur", "exachange_rate", "distance_us", "us_power_gap",
  "hog_left", "CA_GDP", "latin_america", "govdef_GDP",
  "inst_parliamentary", "inst_military_exec", "us_trade_agreement"
)
if (!all(colunas_synth %in% names(synth_data))) {
  stop("synth_data.rds nao contem todas as colunas esperadas.")
}

synth_data <- synth_data |>
  dplyr::select(dplyr::all_of(colunas_synth)) |>
  dplyr::mutate(
    year = as.integer(year),
    iso3c = as.character(iso3c)
  )

if (anyDuplicated(synth_data[c("iso3c", "year")]) > 0L) {
  stop("synth_data.rds contem chaves pais-ano duplicadas.")
}

unidades <- synth_data |>
  dplyr::distinct(iso3c) |>
  dplyr::arrange(iso3c) |>
  dplyr::mutate(
    iso2c = countrycode::countrycode(
      iso3c,
      origin = "iso3c",
      destination = "iso2c"
    ),
    pais_nome = countrycode::countrycode(
      iso3c,
      origin = "iso3c",
      destination = "country.name"
    ),
    grupo_unidade = dplyr::if_else(
      iso3c == "BRA",
      "Brasil",
      "Donor pool"
    ),
    donor_pool = as.integer(iso3c != "BRA")
  ) |>
  dplyr::select(
    pais_iso3 = iso3c,
    pais_iso2 = iso2c,
    pais_nome,
    grupo_unidade,
    donor_pool
  )

if (
  nrow(unidades) != 96L ||
    sum(unidades$pais_iso3 == "BRA") != 1L ||
    sum(unidades$donor_pool == 1L) != 95L ||
    any(unidades$pais_iso3 == "CHN") ||
    anyNA(unidades$pais_iso2)
) {
  stop(
    "O universo autoritativo deve conter exatamente Brasil e 95 donors, ",
    "sem incluir a China como unidade."
  )
}

completude_synth <- synth_data |>
  dplyr::count(iso3c, name = "n_anos")
if (
  nrow(synth_data) != 1920L ||
    !all(sort(unique(synth_data$year)) == 1997:2016) ||
    !all(completude_synth$n_anos == 20L)
) {
  stop("O painel anual autoritativo nao e balanceado em 1997-2016.")
}

status_unidade <- synth_data |>
  dplyr::group_by(iso3c) |>
  dplyr::summarise(
    alguma_vez_principal_destino = any(treatment == 1, na.rm = TRUE),
    sempre_zero_principal_destino = all(treatment == 0, na.rm = TRUE),
    .groups = "drop"
  )

if (
  sum(status_unidade$alguma_vez_principal_destino) != 1L ||
    !status_unidade$alguma_vez_principal_destino[
      status_unidade$iso3c == "BRA"
    ] ||
    sum(
      status_unidade$sempre_zero_principal_destino &
        status_unidade$iso3c != "BRA"
    ) != 95L
) {
  stop("O indicador do paper nao corresponde a Brasil versus 95 donors.")
}

votacoes <- un_roll_calls |>
  dplyr::mutate(ano = as.integer(format(date, "%Y"))) |>
  dplyr::filter(dplyr::between(ano, 1997L, 2016L)) |>
  dplyr::select(
    rcid,
    data = date,
    ano,
    sessao = session,
    simbolo_resolucao = unres,
    voto_importante = importantvote,
    emenda = amend,
    paragrafo = para
  )

if (nrow(votacoes) != 1813L || anyDuplicated(votacoes$rcid) > 0L) {
  stop("A janela 1997-2016 nao contem as 1.813 votacoes esperadas.")
}

temas <- un_roll_call_issues |>
  dplyr::mutate(tema = as.character(issue)) |>
  dplyr::filter(!is.na(tema), tema != "") |>
  dplyr::distinct(rcid, tema) |>
  dplyr::group_by(rcid) |>
  dplyr::summarise(
    tema = paste(sort(unique(tema)), collapse = "; "),
    .groups = "drop"
  )

votacoes <- votacoes |>
  dplyr::left_join(temas, by = "rcid") |>
  dplyr::mutate(
    tema = dplyr::coalesce(tema, "Sem codificacao tematica")
  )

votos_janela <- un_votes |>
  dplyr::semi_join(votacoes, by = "rcid") |>
  dplyr::transmute(
    rcid,
    pais_iso2 = as.character(country_code),
    voto_pais = as.character(vote)
  )

if (anyDuplicated(votos_janela[c("rcid", "pais_iso2")]) > 0L) {
  stop("O tarball contem chaves pais-votacao duplicadas.")
}

votos_china <- votos_janela |>
  dplyr::filter(pais_iso2 == "CN") |>
  dplyr::select(rcid, voto_china = voto_pais)

covariaveis_anuais <- synth_data |>
  dplyr::left_join(
    status_unidade,
    by = "iso3c"
  ) |>
  dplyr::transmute(
    pais_iso3 = iso3c,
    ano = year,
    china_principal_destino_paper = as.integer(treatment),
    donor_nunca_china_principal = as.integer(
      iso3c != "BRA" & sempre_zero_principal_destino
    ),
    abs_distance_china,
    gpi,
    abs_distance_usa,
    perc_trade_with_us,
    perc_trade_with_china,
    pci_cur,
    exachange_rate,
    distance_us,
    us_power_gap,
    hog_left,
    CA_GDP,
    latin_america,
    govdef_GDP,
    inst_parliamentary,
    inst_military_exec,
    us_trade_agreement
  )

painel_pais_votacao <- tidyr::crossing(unidades, votacoes) |>
  dplyr::left_join(
    votos_janela,
    by = c("rcid", "pais_iso2")
  ) |>
  dplyr::left_join(votos_china, by = "rcid") |>
  dplyr::left_join(
    covariaveis_anuais,
    by = c("pais_iso3", "ano")
  ) |>
  dplyr::mutate(
    periodo_2009 = dplyr::if_else(
      ano >= 2009L,
      "2009-2016",
      "1997-2008"
    ),
    pos_2009 = as.integer(ano >= 2009L),
    voto_pais_disponivel = as.integer(!is.na(voto_pais)),
    voto_china_disponivel = as.integer(!is.na(voto_china)),
    par_valido = as.integer(
      voto_pais_disponivel == 1L & voto_china_disponivel == 1L
    ),
    convergente_china = dplyr::if_else(
      par_valido == 1L,
      as.integer(voto_pais == voto_china),
      NA_integer_
    )
  ) |>
  dplyr::arrange(pais_iso3, ano, data, rcid) |>
  dplyr::select(
    pais_iso3,
    pais_iso2,
    pais_nome,
    grupo_unidade,
    donor_pool,
    rcid,
    data,
    ano,
    periodo_2009,
    pos_2009,
    sessao,
    simbolo_resolucao,
    tema,
    voto_importante,
    emenda,
    paragrafo,
    voto_pais,
    voto_china,
    voto_pais_disponivel,
    voto_china_disponivel,
    par_valido,
    convergente_china,
    china_principal_destino_paper,
    donor_nunca_china_principal,
    abs_distance_china,
    gpi,
    abs_distance_usa,
    perc_trade_with_us,
    perc_trade_with_china,
    pci_cur,
    exachange_rate,
    distance_us,
    us_power_gap,
    hog_left,
    CA_GDP,
    latin_america,
    govdef_GDP,
    inst_parliamentary,
    inst_military_exec,
    us_trade_agreement
  )

if (
  nrow(painel_pais_votacao) != 174048L ||
    dplyr::n_distinct(painel_pais_votacao$pais_iso3) != 96L ||
    dplyr::n_distinct(painel_pais_votacao$rcid) != 1813L ||
    anyDuplicated(painel_pais_votacao[c("pais_iso3", "rcid")]) > 0L ||
    anyNA(painel_pais_votacao$china_principal_destino_paper)
) {
  stop("A base pais x votacao falhou nas checagens de dimensao ou chave.")
}

painel_pais_ano <- painel_pais_votacao |>
  dplyr::group_by(
    pais_iso3,
    pais_iso2,
    pais_nome,
    grupo_unidade,
    donor_pool,
    ano,
    periodo_2009,
    pos_2009
  ) |>
  dplyr::summarise(
    n_votacoes = dplyr::n(),
    n_votos_pais = sum(voto_pais_disponivel),
    n_votos_china = sum(voto_china_disponivel),
    n_pares_validos = sum(par_valido),
    n_convergentes_china = sum(convergente_china == 1L, na.rm = TRUE),
    n_divergentes_china = sum(convergente_china == 0L, na.rm = TRUE),
    taxa_convergencia_china = mean(convergente_china, na.rm = TRUE),
    china_principal_destino_paper = dplyr::first(
      china_principal_destino_paper
    ),
    donor_nunca_china_principal = dplyr::first(
      donor_nunca_china_principal
    ),
    abs_distance_china = dplyr::first(abs_distance_china),
    gpi = dplyr::first(gpi),
    abs_distance_usa = dplyr::first(abs_distance_usa),
    perc_trade_with_us = dplyr::first(perc_trade_with_us),
    perc_trade_with_china = dplyr::first(perc_trade_with_china),
    pci_cur = dplyr::first(pci_cur),
    exachange_rate = dplyr::first(exachange_rate),
    distance_us = dplyr::first(distance_us),
    us_power_gap = dplyr::first(us_power_gap),
    hog_left = dplyr::first(hog_left),
    CA_GDP = dplyr::first(CA_GDP),
    latin_america = dplyr::first(latin_america),
    govdef_GDP = dplyr::first(govdef_GDP),
    inst_parliamentary = dplyr::first(inst_parliamentary),
    inst_military_exec = dplyr::first(inst_military_exec),
    us_trade_agreement = dplyr::first(us_trade_agreement),
    .groups = "drop"
  ) |>
  dplyr::arrange(pais_iso3, ano)

if (
  nrow(painel_pais_ano) != 1920L ||
    anyDuplicated(painel_pais_ano[c("pais_iso3", "ano")]) > 0L
) {
  stop("A agregacao pais-ano falhou nas checagens de dimensao ou chave.")
}

brasil_lab <- painel_pais_votacao |>
  dplyr::filter(pais_iso3 == "BRA", par_valido == 1L) |>
  dplyr::transmute(
    rcid,
    data,
    ano,
    periodo_2009,
    pos_2009,
    sessao,
    simbolo_resolucao,
    tema,
    voto_importante,
    voto_brasil = voto_pais,
    voto_china,
    convergente = convergente_china
  ) |>
  dplyr::arrange(ano, data, rcid)

if (
  nrow(brasil_lab) != 1762L ||
    anyDuplicated(brasil_lab$rcid) > 0L ||
    anyNA(brasil_lab$convergente) ||
    !all(brasil_lab$convergente %in% c(0L, 1L))
) {
  stop("A base do laboratorio do Brasil falhou nas checagens finais.")
}

dicionario <- tibble::tribble(
  ~base, ~variavel, ~tipo, ~unidade, ~descricao, ~uso,
  "todas", "pais_iso3", "texto", "pais", "Codigo ISO3 da unidade", "chave",
  "todas", "ano", "inteiro", "ano", "Ano da votacao ou do agregado", "tempo",
  "painel_pais_votacao", "rcid", "inteiro", "votacao nominal", "Identificador unico da votacao no unvotes", "chave",
  "painel_pais_votacao", "data", "data", "dia", "Data da votacao nominal", "auditoria",
  "painel_pais_votacao", "periodo_2009", "categorica", "periodo", "1997-2008 ou 2009-2016", "descricao pre-pos",
  "painel_pais_votacao", "pos_2009", "binaria", "votacao", "1 para anos de 2009 a 2016", "associacao descritiva",
  "painel_pais_votacao", "simbolo_resolucao", "texto", "votacao", "Simbolo da resolucao no objeto un_roll_calls", "auditoria",
  "painel_pais_votacao", "tema", "texto", "votacao", "Temas agregados do objeto un_roll_call_issues", "descricao",
  "painel_pais_votacao", "voto_pais", "categorica", "pais-votacao", "yes, no ou abstain; ausente quando nao ha registro valido", "resultado observado",
  "painel_pais_votacao", "voto_china", "categorica", "votacao", "Voto da China usado como referencia", "referencia",
  "painel_pais_votacao", "par_valido", "binaria", "pais-votacao", "1 quando os votos do pais e da China sao observados", "denominador",
  "painel_pais_votacao", "convergente_china", "binaria", "pais-votacao", "1 quando os votos validos do pais e da China sao iguais", "resultado descritivo",
  "painel_pais_votacao", "china_principal_destino_paper", "binaria", "pais-ano", "Indicador treatment preservado do synth_data; extensao futura do desenho do paper", "nao usado na Aula 2",
  "painel_pais_votacao", "donor_nunca_china_principal", "binaria", "pais", "1 para os 95 donors com treatment sempre zero em 1997-2016", "comparacao futura",
  "painel_pais_votacao", "perc_trade_with_china", "continua", "pais-ano", "Campo preservado do synth_data; escala transformada deve ser documentada antes de interpretacao", "progressao associacional futura",
  "painel_pais_votacao", "perc_trade_with_us", "continua", "pais-ano", "Campo preservado do synth_data; escala transformada deve ser documentada antes de interpretacao", "progressao associacional futura",
  "painel_pais_ano", "n_pares_validos", "inteiro", "pais-ano", "Numero de votacoes com ambos os votos observados", "denominador anual",
  "painel_pais_ano", "taxa_convergencia_china", "continua", "pais-ano", "Media de convergente_china entre pares validos", "resultado associacional futuro",
  "brasil_lab", "voto_brasil", "categorica", "votacao", "Voto valido do Brasil", "descricao",
  "brasil_lab", "convergente", "binaria", "votacao", "1 quando Brasil e China votam da mesma forma", "resultado da Aula 2"
)

caminhos_saida <- c(
  painel_pais_votacao = file.path(
    diretorio_processado,
    "painel_pais_votacao_1997_2016.csv.gz"
  ),
  painel_pais_ano = file.path(
    diretorio_processado,
    "painel_pais_ano_1997_2016.csv"
  ),
  brasil_lab = file.path(
    diretorio_processado,
    "brasil_convergencia_china_1997_2016.csv"
  ),
  unidades = file.path(
    diretorio_processado,
    "unidades_autoritativas_1997_2016.csv"
  ),
  dicionario = file.path(
    diretorio_processado,
    "dicionario_variaveis.csv"
  )
)

readr::write_csv(
  painel_pais_votacao,
  caminhos_saida[["painel_pais_votacao"]],
  na = ""
)
readr::write_csv(
  painel_pais_ano,
  caminhos_saida[["painel_pais_ano"]],
  na = ""
)
readr::write_csv(
  brasil_lab,
  caminhos_saida[["brasil_lab"]],
  na = ""
)
readr::write_csv(unidades, caminhos_saida[["unidades"]], na = "")
readr::write_csv(dicionario, caminhos_saida[["dicionario"]], na = "")

arquivos_manifesto <- c(
  arquivo_unvotes,
  arquivo_synth,
  caminhos_saida
)

linhas_manifesto <- c(
  NA_integer_,
  nrow(synth_data),
  nrow(painel_pais_votacao),
  nrow(painel_pais_ano),
  nrow(brasil_lab),
  nrow(unidades),
  nrow(dicionario)
)
colunas_manifesto <- c(
  NA_integer_,
  ncol(synth_data),
  ncol(painel_pais_votacao),
  ncol(painel_pais_ano),
  ncol(brasil_lab),
  ncol(unidades),
  ncol(dicionario)
)

manifesto <- tibble::tibble(
  tipo = c("fonte", "fonte", rep("derivado", 5L)),
  arquivo = sub(
    paste0("^", raiz_projeto, "/"),
    "",
    arquivos_manifesto
  ),
  sha256 = vapply(arquivos_manifesto, hash_arquivo, character(1)),
  bytes = as.numeric(file.info(arquivos_manifesto)$size),
  linhas = linhas_manifesto,
  colunas = colunas_manifesto,
  janela = "1997-2016",
  unidade_autoritativa = c(
    "pacote R preservado",
    "pais-ano: 96 unidades totais",
    "pais-votacao: 96 unidades x 1.813 votacoes",
    "pais-ano",
    "votacao nominal do Brasil com par valido",
    "pais: Brasil e 95 donors",
    "variavel"
  ),
  observacao = c(
    "China e referencia de voto; nao e unidade do donor pool",
    "96 unidades totais = Brasil + 95 donors; nao ha 96 donors + Brasil",
    "base-mae completa; convergencia ausente quando um voto nao e observado",
    "agregado anual para progressoes associacionais futuras",
    "base exclusiva do laboratorio descritivo da Aula 2",
    "lista extraida diretamente de synth_data.rds",
    "campos de comercio e treatment nao sao usados na Aula 2"
  )
)

caminho_manifesto <- file.path(
  diretorio_processado,
  "manifesto_dados.csv"
)
readr::write_csv(manifesto, caminho_manifesto, na = "")

arquivos_checksum <- c(arquivos_manifesto, caminho_manifesto)
linhas_checksum <- paste(
  vapply(arquivos_checksum, hash_arquivo, character(1)),
  sub(paste0("^", raiz_projeto, "/"), "", arquivos_checksum),
  sep = "  "
)
writeLines(
  linhas_checksum,
  con = file.path(diretorio_processado, "SHA256SUMS"),
  useBytes = TRUE
)

message("PASS: painel pais x votacao construido para 1997-2016.")
message(
  "Unidades totais: ", nrow(unidades),
  " (Brasil + ", sum(unidades$donor_pool), " donors)."
)
message(
  "Votacoes: ", nrow(votacoes),
  "; linhas da base-mae: ", nrow(painel_pais_votacao), "."
)
message(
  "Pares validos Brasil-China para a Aula 2: ", nrow(brasil_lab), "."
)
