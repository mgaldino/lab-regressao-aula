# Aula 6: regressão múltipla — laboratório
#
# Abra lab-regressao-aula-2026.Rproj no RStudio e execute bloco a bloco,
# em duplas ou trios.
# Unidade de análise: país em 2016.
# Variável resposta: convergência dos votos do país com os da China na AGNU,
# em % dos pares de votos válidos.
#
# Partes (cerca de 90 minutos):
# 1. Base de 2016 ..................................... 10 min
# 2. Continente como preditor categórico .............. 15 min
# 3. Europa como variável omitida ..................... 20 min
# 4. Decomposição do coeficiente ...................... 20 min
# 5. Modelos progressivos ............................. 25 min

library(data.table)
library(dplyr)
library(ggplot2)
library(here)

# 1. Base de 2016 ---------------------------------------------------------

# Painel país-ano de votos na AGNU: uma linha por país e ano, 1997-2016.
painel_votos <- data.table::fread(
  here::here("projeto_agna", "data", "processed", "painel_pais_ano_1997_2016.csv")
)

# Covariáveis país-ano em escala original: continente, exportações para a
# China e para os EUA (% das exportações do país), hiato de poder em relação
# aos EUA, PIB per capita (mil US$) e conta corrente (% do PIB).
# Proveniência: data/processed/PROVENIENCIA_covariaveis.md.
covariaveis <- data.table::fread(
  here::here("projeto_agna", "data", "processed", "covariaveis_pais_ano_1997_2016.csv"),
  encoding = "UTF-8"
)

# paises_2016: votos e covariáveis juntos pela chave país-ano, só em 2016.
paises_2016 <- painel_votos |>
  dplyr::select(pais_iso3, pais_nome, ano, taxa_convergencia_china) |>
  dplyr::inner_join(covariaveis, by = c("pais_iso3", "ano")) |>
  dplyr::filter(ano == 2016) |>
  dplyr::mutate(convergencia = 100 * taxa_convergencia_china)

# São 96 países: o Brasil e 95 países em que a China não foi o principal
# destino das exportações de bens em nenhum ano de 1997-2016.
nrow(paises_2016)
summary(paises_2016$convergencia)
summary(paises_2016$exportacoes_china_pct)

# O Brasil tem a maior participação da China nas exportações da amostra.
# Os EUA estão na amostra, com hiato de poder zero.
paises_2016 |>
  dplyr::filter(pais_iso3 %in% c("BRA", "USA")) |>
  dplyr::select(pais_nome, convergencia, exportacoes_china_pct, hiato_poder_eua)

# 2. Continente como preditor categórico ----------------------------------

# factor() fixa a ordem das categorias. A primeira, África, é a referência.
paises_2016 <- paises_2016 |>
  dplyr::mutate(
    continente = factor(
      continente,
      levels = c("África", "Américas", "Ásia e Oceania", "Europa")
    )
  )

# Tabela 1. Convergência média com a China por continente, 2016
# (% dos pares de votos válidos; 96 países).
tabela_1 <- paises_2016 |>
  dplyr::group_by(continente) |>
  dplyr::summarise(
    paises = dplyr::n(),
    convergencia_media = mean(convergencia)
  )
tabela_1

# Regressão só com o continente. Intercepto: média da África.
# Cada coeficiente: média do continente menos a média da África.
modelo_continente <- lm(convergencia ~ continente, data = paises_2016)
coef(modelo_continente)

# Referência nas Américas: os coeficientes mudam.
modelo_americas <- lm(
  convergencia ~ relevel(continente, ref = "Américas"),
  data = paises_2016
)
coef(modelo_americas)

# Os valores ajustados continuam iguais às médias por continente.
all.equal(fitted(modelo_continente), fitted(modelo_americas))

# Armadilha das indicadoras: as quatro indicadoras e o intercepto.
paises_2016 <- paises_2016 |>
  dplyr::mutate(
    africa = as.integer(continente == "África"),
    americas = as.integer(continente == "Américas"),
    asia_oceania = as.integer(continente == "Ásia e Oceania"),
    europa = as.integer(continente == "Europa")
  )

modelo_armadilha <- lm(
  convergencia ~ africa + americas + asia_oceania + europa,
  data = paises_2016
)
coef(modelo_armadilha)
# Um coeficiente sai NA: as quatro indicadoras somam 1, que é a coluna
# do intercepto.

# 3. Europa como variável omitida -----------------------------------------

# Regressão curta: convergência em exportações para a China.
regressao_curta <- lm(convergencia ~ exportacoes_china_pct, data = paises_2016)

# Regressão longa: acrescenta a indicadora de Europa.
regressao_longa <- lm(
  convergencia ~ exportacoes_china_pct + europa,
  data = paises_2016
)

# Regressão auxiliar: indicadora de Europa em exportações para a China.
regressao_auxiliar <- lm(europa ~ exportacoes_china_pct, data = paises_2016)

coef_curto <- coef(regressao_curta)[["exportacoes_china_pct"]]
coef_longo <- coef(regressao_longa)[["exportacoes_china_pct"]]
coef_europa <- coef(regressao_longa)[["europa"]]
inclinacao_auxiliar <- coef(regressao_auxiliar)[["exportacoes_china_pct"]]

# Tabela 2. Regressões curta, longa e auxiliar (96 países, 2016).
# Unidades: p.p. de convergência por p.p. de exportações (linhas 1 e 2);
# p.p. de convergência (linha 3); variação da indicadora de Europa por p.p.
# de exportações (linha 4).
tabela_2 <- data.frame(
  termo = c(
    "gamma_1: exportações, regressão curta",
    "beta_1: exportações, regressão longa",
    "beta_2: Europa, regressão longa",
    "delta: exportações, regressão auxiliar",
    "beta_1 + beta_2 * delta"
  ),
  valor = c(
    coef_curto,
    coef_longo,
    coef_europa,
    inclinacao_auxiliar,
    coef_longo + coef_europa * inclinacao_auxiliar
  )
)
tabela_2
# A primeira e a última linha são iguais: gamma_1 = beta_1 + beta_2 * delta.

# Exportações médias para a China na Europa (1) e nos demais continentes (0).
paises_2016 |>
  dplyr::group_by(europa) |>
  dplyr::summarise(exportacoes_china_media = mean(exportacoes_china_pct))

# 4. Decomposição do coeficiente ------------------------------------------

# M2: exportações para a China e continente.
modelo_m2 <- lm(
  convergencia ~ exportacoes_china_pct + continente,
  data = paises_2016
)

# Passo 1: parte das exportações para a China que o continente não prevê.
# Com um preditor categórico, é a diferença em relação à média do continente.
paises_2016$residuo_exportacoes <- residuals(
  lm(exportacoes_china_pct ~ continente, data = paises_2016)
)

# Passo 2: regressão simples da convergência nesse resíduo.
modelo_residuo <- lm(convergencia ~ residuo_exportacoes, data = paises_2016)

# Os dois coeficientes são iguais.
coef(modelo_residuo)[["residuo_exportacoes"]]
coef(modelo_m2)[["exportacoes_china_pct"]]

# Figura 1. Convergência com a China e exportações para a China menos a
# média do continente, 2016 (96 países), com a reta de MQO.
figura_1 <- ggplot(
  paises_2016,
  aes(x = residuo_exportacoes, y = convergencia)
) +
  geom_point(aes(color = continente), size = 2.4) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#C2410C") +
  labs(
    title = "Figura 1. Convergência e resíduo das exportações para a China, 2016",
    x = "Exportações para a China menos a média do continente (p.p.)",
    y = "Convergência com a China (%)",
    color = NULL,
    caption = paste(
      "Fontes: votações nominais da AGNU (pacote unvotes);",
      "ITPD-E, release 3 (USITC)."
    )
  ) +
  theme_minimal(base_size = 12)
figura_1

# 5. Modelos progressivos -------------------------------------------------

# M1: exportações para a China.
modelo_m1 <- lm(convergencia ~ exportacoes_china_pct, data = paises_2016)

# M2: M1 + continente (estimado na parte 4).

# M3: M2 + exportações para os EUA e hiato de poder em relação aos EUA.
# O hiato é |GPI dos EUA - GPI do país|: vale 0 para os EUA e perto de 0,25
# para os países pequenos. Um coeficiente de 180 equivale a 1,8 p.p. de
# convergência por 0,01 de hiato.
modelo_m3 <- lm(
  convergencia ~ exportacoes_china_pct + continente +
    exportacoes_eua_pct + hiato_poder_eua,
  data = paises_2016
)

# M4: M3 + PIB per capita (mil US$) e conta corrente (% do PIB).
modelo_m4 <- lm(
  convergencia ~ exportacoes_china_pct + continente +
    exportacoes_eua_pct + hiato_poder_eua +
    pib_per_capita_mil_usd + conta_corrente_pct_pib,
  data = paises_2016
)

coef(modelo_m3)
coef(modelo_m4)

# Tabela 3. Coeficiente das exportações para a China em quatro modelos
# (p.p. de convergência por p.p. de exportações; 96 países, 2016).
tabela_3 <- data.frame(
  modelo = c("M1", "M2", "M3", "M4"),
  preditores = c(
    "exportações para a China",
    "M1 + continente",
    "M2 + exportações para os EUA + hiato de poder",
    "M3 + PIB per capita + conta corrente"
  ),
  coef_exportacoes_china = c(
    coef(modelo_m1)[["exportacoes_china_pct"]],
    coef(modelo_m2)[["exportacoes_china_pct"]],
    coef(modelo_m3)[["exportacoes_china_pct"]],
    coef(modelo_m4)[["exportacoes_china_pct"]]
  )
)
tabela_3

# 6. Perguntas ------------------------------------------------------------

# 1. Na Tabela 1 e em coef(modelo_continente), onde está a média da Europa?
# 2. Na Tabela 2, por que o coeficiente das exportações muda de sinal da
#    regressão curta para a longa? Use os sinais de coef_europa e de
#    inclinacao_auxiliar.
# 3. Na Figura 1, qual é a inclinação da reta? Compare com o coeficiente
#    das exportações em modelo_m2.
# 4. Escreva um parágrafo que interprete o coeficiente das exportações para
#    a China em M4 (Tabela 3), com as unidades de X e de Y.
# 5. (Opcional) Estime M3 sem os EUA. O que acontece com o coeficiente do
#    hiato de poder?
#
# Entrega: script, Tabela 3 e o parágrafo da pergunta 4.
