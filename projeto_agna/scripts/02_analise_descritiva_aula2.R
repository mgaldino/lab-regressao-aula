# Analise descritiva da convergencia direta Brasil-China, 1997-2016.
#
# Escopo da Aula 2:
# - uma observacao e uma votacao nominal com votos validos de Brasil e China;
# - o corte separa 1997-2008 e 2009-2016;
# - todos os resultados sao descritivos, sem interpretacao causal.

options(scipen = 999)


library(dplyr)
library(ggplot2)
library(here)
library(readr)
library(scales)
library(tidyr)

# veja o que here() mostra
here()

#agora, com caminho de pastas
here("projeto_agna", "data", "processed")

# para nao ficar escrevendo here toda vez
diretorio_processado <- here("projeto_agna", "data", "processed")
diretorio_saida <- here("projeto_agna", "output", "aula_02")
dir.create(diretorio_saida, recursive = TRUE, showWarnings = FALSE)

# uma linha por país × votação da ONU.
painel <- tibble::as_tibble(
  data.table::fread(
    file.path(
      diretorio_processado,
      "painel_pais_votacao_1997_2016.csv.gz"
    )
  )
)

# inspecionando os dados
glimpse(painel)

# uma linha por país × ano
painel_anual <- tibble::as_tibble(
  data.table::fread(
    file.path(diretorio_processado, "painel_pais_ano_1997_2016.csv")
  )
)

glimpse(painel_anual)
# painel anual agrega as votações de cada país no ano e calcula:
# - número de votações;
# - pares de votos válidos país–China;
# - votos convergentes e divergentes;
# - taxa_convergencia_china;
# - fluxo de comércio e demais variáveis país-ano.


brasil <- tibble::as_tibble(
  data.table::fread(
    file.path(diretorio_processado, "brasil_convergencia_china_1997_2016.csv")
  )
)

glimpse(brasil)

#
# O banco brasil é uma versão simplificada do painel de votações, contendo apenas:
# - Brasil;
# - votações em que os votos de Brasil e China estão disponíveis;
# - uma linha por votação;
# - convergente = 1 quando Brasil e China votaram igual e 0 quando votaram diferente.
# Ele tem 1.762 linhas. Das 1.813 votações existentes, 51 são excluídas porque não há um par válido Brasil–China.




frequencias_convergencia <- brasil |>
  dplyr::count(convergente, name = "n") |>
  dplyr::mutate(
    resultado = dplyr::if_else(
      convergente == 1L,
      "Convergente",
      "Divergente"
    ),
    percentual = 100 * n / sum(n)
  ) |>
  dplyr::select(resultado, n, percentual)

resumo_periodo <- brasil |>
  dplyr::group_by(periodo_2009) |>
  dplyr::summarise(
    n_pares_validos = dplyr::n(),
    n_convergentes = sum(convergente == 1L),
    n_divergentes = sum(convergente == 0L),
    taxa_convergencia = mean(convergente),
    variancia_amostral = var(convergente),
    desvio_padrao_amostral = sd(convergente),
    .groups = "drop"
  ) |>
  dplyr::mutate(percentual_convergencia = 100 * taxa_convergencia)

resumo_anual <- brasil |>
  dplyr::group_by(ano, periodo_2009) |>
  dplyr::summarise(
    n_pares_validos = dplyr::n(),
    n_convergentes = sum(convergente == 1L),
    n_divergentes = sum(convergente == 0L),
    taxa_convergencia = mean(convergente),
    .groups = "drop"
  ) |>
  dplyr::mutate(percentual_convergencia = 100 * taxa_convergencia)

frequencias_votos <- brasil |>
  dplyr::count(voto_brasil, voto_china, name = "n") |>
  dplyr::mutate(percentual = 100 * n / sum(n)) |>
  dplyr::arrange(voto_brasil, voto_china)

momentos <- tibble::tibble(
  variavel = c("convergente", "pos_2009"),
  media = c(mean(brasil$convergente), mean(brasil$pos_2009)),
  variancia_amostral = c(var(brasil$convergente), var(brasil$pos_2009)),
  desvio_padrao_amostral = c(sd(brasil$convergente), sd(brasil$pos_2009)),
  variancia_empirica_N = c(
    mean((brasil$convergente - mean(brasil$convergente))^2),
    mean((brasil$pos_2009 - mean(brasil$pos_2009))^2)
  )
)

taxa_pre <- resumo_periodo |>
  dplyr::filter(periodo_2009 == "1997-2008") |>
  dplyr::pull(taxa_convergencia)

taxa_pos <- resumo_periodo |>
  dplyr::filter(periodo_2009 == "2009-2016") |>
  dplyr::pull(taxa_convergencia)

associacao_pre_pos <- tibble::tibble(
  par = "convergente x pos_2009",
  covariancia_amostral = cov(brasil$convergente, brasil$pos_2009),
  correlacao = cor(brasil$convergente, brasil$pos_2009),
  diferenca_pos_menos_pre = taxa_pos - taxa_pre,
  diferenca_pontos_percentuais = 100 * (taxa_pos - taxa_pre)
)

p <- mean(brasil$convergente)
erro_quadratico_medio <- tibble::tibble(
  preditor_constante = c(
    "Media observada",
    "Sempre prever convergencia",
    "Sempre prever divergencia"
  ),
  valor_predito = c(p, 1, 0),
  mse = c(
    mean((brasil$convergente - p)^2),
    mean((brasil$convergente - 1)^2),
    mean((brasil$convergente - 0)^2)
  )
)

figura_anual <- ggplot2::ggplot(
  resumo_anual,
  ggplot2::aes(x = ano, y = taxa_convergencia)
) +
  ggplot2::geom_vline(
    xintercept = 2008.5,
    linetype = "dashed",
    color = "#6B7280",
    linewidth = 0.7
  ) +
  ggplot2::geom_line(color = "#2563EB", linewidth = 1) +
  ggplot2::geom_point(color = "#0F172A", size = 2.5) +
  ggplot2::annotate(
    "text",
    x = 2008.2,
    y = 0.90,
    label = "Corte descritivo: 2009",
    hjust = 1,
    size = 3.6,
    color = "#374151"
  ) +
  ggplot2::scale_x_continuous(breaks = seq(1997, 2016, by = 2)) +
  ggplot2::scale_y_continuous(
    labels = scales::label_percent(
      accuracy = 1,
      decimal.mark = ","
    ),
    limits = c(0.65, 0.92),
    breaks = seq(0.65, 0.90, by = 0.05)
  ) +
  ggplot2::labs(
    title = "A convergência Brasil-China varia ao longo dos anos",
    subtitle = "Percentual de votações nominais com votos iguais",
    x = "Ano",
    y = "Convergência direta",
    caption = paste(
      "Fonte: unvotes 0.3.0. A linha apenas separa os períodos;",
      "não representa identificação causal."
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

figura_periodo <- ggplot2::ggplot(
  resumo_periodo,
  ggplot2::aes(x = periodo_2009, y = taxa_convergencia, fill = periodo_2009)
) +
  ggplot2::geom_col(width = 0.62, show.legend = FALSE) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = scales::percent(
        taxa_convergencia,
        accuracy = 0.1,
        decimal.mark = ","
      )
    ),
    vjust = -0.5,
    size = 4.2,
    fontface = "bold"
  ) +
  ggplot2::scale_fill_manual(values = c(
    "1997-2008" = "#94A3B8",
    "2009-2016" = "#2563EB"
  )) +
  ggplot2::scale_y_continuous(
    labels = scales::label_percent(
      accuracy = 1,
      decimal.mark = ","
    ),
    limits = c(0, 0.90),
    expand = ggplot2::expansion(mult = c(0, 0.03))
  ) +
  ggplot2::labs(
    title = "A convergência observada é maior no período pós-2009",
    subtitle = paste0(
      "Diferença descritiva: ",
      format(
        round(associacao_pre_pos$diferenca_pontos_percentuais, 1),
        decimal.mark = ",",
        nsmall = 1
      ),
      " pontos percentuais"
    ),
    x = "Período da votação",
    y = "Convergência direta",
    caption = paste(
      "Fonte: unvotes 0.3.0. Denominador: pares com votos válidos",
      "de Brasil e China; comparação estritamente descritiva."
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

caminhos_tabelas <- c(
  tabela_2 = file.path(
    diretorio_saida,
    "tabela_2_frequencias_convergencia.csv"
  ),
  tabela_3 = file.path(
    diretorio_saida,
    "tabela_3_resumo_pre_pos_2009.csv"
  ),
  tabela_4 = file.path(
    diretorio_saida,
    "tabela_4_convergencia_anual.csv"
  ),
  tabela_5 = file.path(diretorio_saida, "tabela_5_momentos.csv"),
  tabela_6 = file.path(
    diretorio_saida,
    "tabela_6_covariancia_correlacao.csv"
  ),
  tabela_7 = file.path(
    diretorio_saida,
    "tabela_7_frequencias_votos_brasil_china.csv"
  ),
  tabela_8 = file.path(diretorio_saida, "tabela_8_mse.csv")
)

readr::write_csv(
  frequencias_convergencia,
  caminhos_tabelas[["tabela_2"]],
  na = ""
)
readr::write_csv(resumo_periodo, caminhos_tabelas[["tabela_3"]], na = "")
readr::write_csv(resumo_anual, caminhos_tabelas[["tabela_4"]], na = "")
readr::write_csv(momentos, caminhos_tabelas[["tabela_5"]], na = "")
readr::write_csv(
  associacao_pre_pos,
  caminhos_tabelas[["tabela_6"]],
  na = ""
)
readr::write_csv(
  frequencias_votos,
  caminhos_tabelas[["tabela_7"]],
  na = ""
)
readr::write_csv(
  erro_quadratico_medio,
  caminhos_tabelas[["tabela_8"]],
  na = ""
)

caminho_figura_1 <- file.path(
  diretorio_saida,
  "figura_1_convergencia_anual.png"
)
caminho_figura_2 <- file.path(
  diretorio_saida,
  "figura_2_convergencia_pre_pos_2009.png"
)

ggplot2::ggsave(
  caminho_figura_1,
  plot = figura_anual,
  width = 9,
  height = 5.4,
  dpi = 300,
  bg = "white"
)
ggplot2::ggsave(
  caminho_figura_2,
  plot = figura_periodo,
  width = 8,
  height = 5.4,
  dpi = 300,
  bg = "white"
)

manifesto_outputs <- tibble::tribble(
  ~arquivo, ~numero, ~tipo, ~caption,
  basename(caminhos_tabelas[["tabela_2"]]), "Tabela 2", "tabela", "Frequências da convergência direta Brasil-China, 1997-2016.",
  basename(caminhos_tabelas[["tabela_3"]]), "Tabela 3", "tabela", "Convergência direta antes e depois de 2009.",
  basename(caminhos_tabelas[["tabela_4"]]), "Tabela 4", "tabela", "Convergência direta por ano, 1997-2016.",
  basename(caminhos_tabelas[["tabela_5"]]), "Tabela 5", "tabela", "Média, variância e desvio-padrão das variáveis binárias.",
  basename(caminhos_tabelas[["tabela_6"]]), "Tabela 6", "tabela", "Covariância, correlação e diferença descritiva entre períodos.",
  basename(caminhos_tabelas[["tabela_7"]]), "Tabela 7", "tabela", "Combinações observadas dos votos de Brasil e China.",
  basename(caminhos_tabelas[["tabela_8"]]), "Tabela 8", "tabela", "Erro quadrático médio de três preditores constantes.",
  basename(caminho_figura_1), "Figura 1", "figura", "Convergência direta Brasil-China por ano, 1997-2016.",
  basename(caminho_figura_2), "Figura 2", "figura", "Convergência direta antes e depois de 2009."
)
readr::write_csv(
  manifesto_outputs,
  file.path(diretorio_saida, "manifesto_outputs.csv"),
  na = ""
)

resumo_execucao <- c(
  "Aula 2 - convergência direta Brasil-China, 1997-2016",
  "Escopo: estatísticas descritivas; nenhuma interpretação causal.",
  paste0("Pares validos: ", nrow(brasil)),
  paste0("Convergentes: ", sum(brasil$convergente == 1L)),
  paste0("Divergentes: ", sum(brasil$convergente == 0L)),
  paste0("Convergência geral: ", sprintf("%.6f", p)),
  paste0("Pré-2009: ", sprintf("%.6f", taxa_pre)),
  paste0("Pós-2009: ", sprintf("%.6f", taxa_pos)),
  paste0(
    "Diferença descritiva em pontos percentuais: ",
    sprintf("%.6f", associacao_pre_pos$diferenca_pontos_percentuais)
  ),
  paste0(
    "Correlação convergente-pos_2009: ",
    sprintf("%.6f", associacao_pre_pos$correlacao)
  )
)
writeLines(
  resumo_execucao,
  con = file.path(diretorio_saida, "resumo_execucao.txt"),
  useBytes = TRUE
)

message("PASS: analise descritiva da Aula 2 executada.")
message(
  "Convergencia geral: ", sprintf("%.1f%%", 100 * p),
  "; pre-2009: ", sprintf("%.1f%%", 100 * taxa_pre),
  "; pos-2009: ", sprintf("%.1f%%", 100 * taxa_pos), "."
)


# ##
# validacao <- tibble::tibble(
#   checagem = c(
#     "Unidades totais na base-mãe",
#     "Unidades do donor pool",
#     "Brasil na lista autoritativa",
#     "China na lista de unidades",
#     "Anos cobertos",
#     "Votações nominais na janela",
#     "Linhas país x votação",
#     "Chaves país x votação duplicadas",
#     "Pares válidos Brasil-China",
#     "Chaves rcid duplicadas no laboratório",
#     "Ausências no resultado convergente",
#     "Valores do resultado fora de 0/1"
#   ),
#   valor = c(
#     nrow(unidades),
#     sum(unidades$donor_pool == 1L),
#     sum(unidades$pais_iso3 == "BRA"),
#     sum(unidades$pais_iso3 == "CHN"),
#     dplyr::n_distinct(painel$ano),
#     dplyr::n_distinct(painel$rcid),
#     nrow(painel),
#     sum(duplicated(painel[c("pais_iso3", "rcid")])),
#     nrow(brasil),
#     sum(duplicated(brasil$rcid)),
#     sum(is.na(brasil$convergente)),
#     sum(!brasil$convergente %in% c(0L, 1L))
#   ),
#   esperado = c(
#     96L, 95L, 1L, 0L, 20L, 1813L, 174048L, 0L,
#     1762L, 0L, 0L, 0L
#   )
# ) |>
#   dplyr::mutate(status = dplyr::if_else(valor == esperado, "PASS", "FAIL"))
