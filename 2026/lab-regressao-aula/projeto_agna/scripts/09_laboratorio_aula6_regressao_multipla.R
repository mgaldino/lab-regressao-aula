# Aula 6: regressão múltipla nas votações da AGNU.
#
# Abra lab-regressao-aula-2026.Rproj no RStudio e execute bloco a bloco.
# Cada linha usada nos modelos representa um país em 2016.
# Pergunta: como a taxa de convergência de votos com a China varia entre
# países, condicionalmente aos preditores incluídos em cada modelo?
# Os coeficientes descrevem associação, sem leitura causal.

options(scipen = 999)
library(data.table)
library(dplyr)
library(ggplot2)
library(here)

# 1. Ler e conhecer os dados --------------------------------------------

painel <- data.table::fread(
  here::here(
    "projeto_agna", "data", "processed",
    "painel_pais_ano_1997_2016.csv"
  )
)

# Uma linha do arquivo original representa um par país-ano.
dim(painel)
data.table::uniqueN(painel$pais_iso3)
range(painel$ano)

# Agora mudamos para um corte transversal: uma linha por país em 2016.
dados_2016 <- painel |>
  dplyr::filter(ano == 2016) |>
  dplyr::select(
    pais_iso3, ano, n_votacoes, n_pares_validos,
    n_convergentes_china, n_divergentes_china,
    taxa_convergencia_china, perc_trade_with_china,
    perc_trade_with_us, us_power_gap, latin_america,
    pci_cur, CA_GDP
  ) |>
  dplyr::arrange(pais_iso3)

# 2. Checagens antes de estimar ------------------------------------------

# A taxa é n_convergentes_china / n_pares_validos: o denominador conta
# somente pares em que o voto do país e o da China estão observados.
validacao_2016 <- data.frame(
  medida = c(
    "Países", "Países da América Latina", "Votações em 2016",
    "Menor denominador válido", "Mediana do denominador válido",
    "Maior denominador válido", "Ausências nas variáveis do modelo"
  ),
  valor = c(
    nrow(dados_2016), sum(dados_2016$latin_america),
    unique(dados_2016$n_votacoes), min(dados_2016$n_pares_validos),
    median(dados_2016$n_pares_validos), max(dados_2016$n_pares_validos),
    sum(is.na(dados_2016))
  )
)
validacao_2016

ausentes_2016 <- colSums(is.na(dados_2016))
ausentes_2016

stopifnot(
  nrow(dados_2016) == 96,
  !anyDuplicated(dados_2016$pais_iso3),
  all(dados_2016$n_pares_validos > 0),
  all(dados_2016$n_pares_validos <= dados_2016$n_votacoes),
  all(dados_2016$n_convergentes_china +
        dados_2016$n_divergentes_china == dados_2016$n_pares_validos),
  max(abs(
    dados_2016$taxa_convergencia_china -
      dados_2016$n_convergentes_china / dados_2016$n_pares_validos
  )) < 1e-12
)

# 3. Escala dos preditores -----------------------------------------------

# O campo de comércio está transformado na fonte; a unidade original
# da transformação não foi documentada. Para interpretá-lo com segurança,
# usamos a média e o desvio-padrão DOS 96 PAÍSES DE 2016.
# scale() subtrai a média e divide pelo desvio-padrão amostral.
dados_2016$z_trade_china <- as.numeric(scale(
  dados_2016$perc_trade_with_china
))
dados_2016$z_trade_eua <- as.numeric(scale(
  dados_2016$perc_trade_with_us
))
dados_2016$z_pares_validos <- as.numeric(scale(
  dados_2016$n_pares_validos
))
dados_2016$z_hiato_poder <- as.numeric(scale(
  dados_2016$us_power_gap
))
dados_2016$z_pci <- as.numeric(scale(
  dados_2016$pci_cur
))
dados_2016$z_ca <- as.numeric(scale(
  dados_2016$CA_GDP
))

# A primeira categoria será a referência da regressão.
dados_2016$regiao <- factor(
  ifelse(dados_2016$latin_america, "América Latina", "Outras regiões"),
  levels = c("Outras regiões", "América Latina")
)
table(dados_2016$regiao)

# 4. Modelos progressivos: mesmos 96 países -----------------------------

# M1: associação bivariada.
modelo_1 <- lm(
  taxa_convergencia_china ~ z_trade_china,
  data = dados_2016
)

# M2: adiciona cobertura da votação e a categoria geográfica.
modelo_2 <- lm(
  taxa_convergencia_china ~ z_trade_china + z_pares_validos + regiao,
  data = dados_2016
)

# M3: adiciona comércio com EUA e hiato de poder em relação aos EUA.
modelo_3 <- lm(
  taxa_convergencia_china ~ z_trade_china + z_pares_validos + regiao +
    z_trade_eua + z_hiato_poder,
  data = dados_2016
)

# M4: adiciona dois controles econômicos, também padronizados.
modelo_4 <- lm(
  taxa_convergencia_china ~ z_trade_china + z_pares_validos + regiao +
    z_trade_eua + z_hiato_poder + z_pci + z_ca,
  data = dados_2016
)

# Olhe só os coeficientes. A saída de summary() contém inferência
# que será estudada nas Aulas 9 e 10.
coef(modelo_1)
coef(modelo_2)
coef(modelo_3)
coef(modelo_4)

# A taxa Y está entre 0 e 1. Multiplicar seu coeficiente por 100
# converte proporção em pontos percentuais da taxa de convergência.
# Para o comércio, X vale um desvio-padrão do campo transformado.
tabela_modelos <- data.frame(
  modelo = c("M1", "M2", "M3", "M4"),
  n_paises = c(nobs(modelo_1), nobs(modelo_2),
               nobs(modelo_3), nobs(modelo_4)),
  r2_amostral = c(
    summary(modelo_1)$r.squared, summary(modelo_2)$r.squared,
    summary(modelo_3)$r.squared, summary(modelo_4)$r.squared
  ),
  china_trade_pp_por_dp = 100 * c(
    coef(modelo_1)[["z_trade_china"]],
    coef(modelo_2)[["z_trade_china"]],
    coef(modelo_3)[["z_trade_china"]],
    coef(modelo_4)[["z_trade_china"]]
  ),
  regiao_latina_pp = 100 * c(
    NA_real_,
    coef(modelo_2)[["regiaoAmérica Latina"]],
    coef(modelo_3)[["regiaoAmérica Latina"]],
    coef(modelo_4)[["regiaoAmérica Latina"]]
  )
)
tabela_modelos

# 5. Dois perfis hipotéticos, mudando somente a categoria ---------------

# Zero nos preditores padronizados = média de 2016. Os perfis são
# previsões da reta ajustada, e não dois países emparelhados.
perfis <- data.frame(
  z_trade_china = c(0, 0),
  z_pares_validos = c(0, 0),
  regiao = factor(
    c("Outras regiões", "América Latina"),
    levels = levels(dados_2016$regiao)
  ),
  z_trade_eua = c(0, 0),
  z_hiato_poder = c(0, 0),
  z_pci = c(0, 0),
  z_ca = c(0, 0)
)
perfis$taxa_ajustada_percentual <- 100 * as.numeric(
  predict(modelo_4, newdata = perfis)
)
perfis

# 6. Duas figuras para discutir em sala ---------------------------------

figura_1 <- ggplot2::ggplot(
  dados_2016,
  ggplot2::aes(
    x = z_trade_china, y = 100 * taxa_convergencia_china,
    color = regiao, shape = regiao
  )
) +
  ggplot2::geom_point(size = 2.8, alpha = 0.85) +
  ggplot2::geom_smooth(
    mapping = ggplot2::aes(
      x = z_trade_china, y = 100 * taxa_convergencia_china, group = 1
    ),
    inherit.aes = FALSE,
    method = "lm", formula = y ~ x, se = FALSE,
    color = "#C2410C", linewidth = 1.1
  ) +
  ggplot2::scale_color_manual(values = c(
    "Outras regiões" = "#1D4ED8", "América Latina" = "#047857"
  )) +
  ggplot2::labs(
    title = "Figura 1. Comércio transformado e convergência de votos, 2016",
    x = "Campo de comércio com a China (desvios-padrão na amostra)",
    y = "Convergência com a China (% dos pares válidos)",
    color = NULL, shape = NULL,
    caption = paste(
      "Unidade: país (96); denominador por país: 5 a 113 pares válidos.",
      "Fonte: painel didático da AGNU e synth_data.rds. Associação, sem leitura causal."
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(size = 9, hjust = 0)
  )
if (interactive()) print(figura_1)

figura_2 <- ggplot2::ggplot(
  tabela_modelos,
  ggplot2::aes(x = modelo, y = china_trade_pp_por_dp, group = 1)
) +
  ggplot2::geom_hline(yintercept = 0, color = "#94A3B8") +
  ggplot2::geom_line(color = "#1D4ED8", linewidth = 0.9) +
  ggplot2::geom_point(color = "#1D4ED8", size = 3.2) +
  ggplot2::scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5)) +
  ggplot2::labs(
    title = "Figura 2. Coeficiente do campo de comércio com a China",
    x = "Especificação (mesmos 96 países)",
    y = "Pontos percentuais da taxa por 1 desvio-padrão",
    caption = paste(
      "M1: comércio China; M2: + cobertura e região;",
      "M3: + comércio EUA e hiato de poder; M4: + controles econômicos.",
      "Fonte: painel didático da AGNU e synth_data.rds."
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(size = 9, hjust = 0)
  )
if (interactive()) print(figura_2)

# 7. Perguntas para a dupla ---------------------------------------------

# a) Como muda o coeficiente do comércio com a China de M1 para M4?
# b) Qual é a categoria de referência para América Latina?
# c) Quais valores foram mantidos iguais nos dois perfis?
# d) Como o menor denominador (5 pares) limita a leitura da taxa?
# e) Por que os quatro modelos não estabelecem efeitos causais?
