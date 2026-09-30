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
# 2. Continente como preditor categórico .............. 10 min
# 3. Europa como variável omitida ..................... 15 min
# 4. Decomposição do coeficiente ...................... 20 min
# 5. Modelos progressivos ............................. 15 min
# 6. MQO em forma matricial ........................... 10 min
# 7. Erro comum com três preditores (simulação) ....... 10 min

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

# Referência nas Américas: relevel() muda a primeira categoria do fator no
# banco. Criamos um banco novo para manter a África como referência no resto
# do roteiro.
paises_2016_americas <- paises_2016 |>
  dplyr::mutate(continente = relevel(continente, ref = "Américas"))
levels(paises_2016_americas$continente)

# Mesma regressão, no banco com a nova referência: os coeficientes mudam.
modelo_americas <- lm(convergencia ~ continente, data = paises_2016_americas)
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

# Objetivo: recuperar, só com regressões simples, o coeficiente das
# exportações na regressão longa da parte 3 (coef_longo), e ver no gráfico
# o que muda quando a indicadora de Europa é retirada de X1 e de Y.
# X1: exportações para a China. X2: indicadora de Europa.

# grupo: rótulo da indicadora de Europa para os gráficos.
paises_2016$grupo <- ifelse(paises_2016$europa == 1, "Europa", "Demais países")

# Médias de X1 e de Y em cada grupo. Com X2 binária, residualizar em X2 é
# subtrair essas médias.
medias_grupo <- paises_2016 |>
  dplyr::group_by(grupo) |>
  dplyr::summarise(
    exportacoes_china_pct = mean(exportacoes_china_pct),
    convergencia = mean(convergencia)
  )
medias_grupo

cores_grupo <- c("Demais países" = "#1D4ED8", "Europa" = "#7C3AED")

# Figura 1. Relação sem controle, 2016 (96 países). A reta é a regressão
# simples (coef_curto); os X marcam as médias de cada grupo.
figura_1 <- ggplot(
  paises_2016,
  aes(x = exportacoes_china_pct, y = convergencia, color = grupo)
) +
  geom_point(size = 2.4, alpha = 0.8) +
  geom_point(data = medias_grupo, shape = 4, size = 6, stroke = 2, show.legend = FALSE) +
  geom_smooth(
    aes(group = 1), method = "lm", formula = y ~ x, se = FALSE,
    color = "#C2410C"
  ) +
  scale_color_manual(values = cores_grupo) +
  labs(
    title = "Figura 1. Convergência e exportações para a China, 2016",
    subtitle = paste(
      "Sem controle: inclinação de",
      format(round(coef_curto, 2), decimal.mark = ","),
      "p.p. por p.p.; X = média de cada grupo"
    ),
    x = "Exportações para a China (% das exportações do país)",
    y = "Convergência com a China (%)",
    color = NULL,
    caption = paste(
      "Fontes: votações nominais da AGNU (pacote unvotes);",
      "ITPD-E, release 3 (USITC)."
    )
  ) +
  theme_minimal(base_size = 12)
figura_1

# Passo 1: regressão de X1 em X2. O resíduo é a parte das exportações para
# a China que a indicadora de Europa não prevê linearmente.
paises_2016$residuo_exportacoes <- residuals(
  lm(exportacoes_china_pct ~ europa, data = paises_2016)
)

# Passo 2: regressão de Y em X2. O resíduo é a parte da convergência que a
# indicadora de Europa não prevê linearmente.
paises_2016$residuo_convergencia <- residuals(
  lm(convergencia ~ europa, data = paises_2016)
)

# Passo 3: regressão simples de um resíduo no outro. O intercepto é zero,
# porque os dois resíduos têm média zero.
modelo_residuos <- lm(
  residuo_convergencia ~ residuo_exportacoes,
  data = paises_2016
)
coef(modelo_residuos)

# A inclinação é igual ao coeficiente das exportações na regressão longa.
coef(modelo_residuos)[["residuo_exportacoes"]]
coef_longo

# Os resíduos também são os mesmos da regressão longa.
all.equal(
  as.numeric(residuals(modelo_residuos)),
  as.numeric(residuals(regressao_longa))
)

# Cuidado: a decomposição exige residualizar X1, o preditor do eixo
# horizontal. Residualizar só Y e manter X1 original não recupera
# coef_longo: a inclinação encolhe pela fração da variância de X1 que
# sobra no resíduo.
coef(lm(residuo_convergencia ~ exportacoes_china_pct, data = paises_2016))
fracao_variancia <- sum(paises_2016$residuo_exportacoes^2) /
  sum((paises_2016$exportacoes_china_pct - mean(paises_2016$exportacoes_china_pct))^2)
fracao_variancia
coef_longo * fracao_variancia
# Aqui a diferença é pequena porque a indicadora de Europa explica pouco das
# exportações (fracao_variancia perto de 1). No exemplo de quatro unidades
# dos slides, a inclinação cai de 2 para 1. Com vários preditores e só Y
# residualizado, os coeficientes misturam os da regressão múltipla e podem
# até trocar de sinal (Hull, 2018, "On Residualized Outcome Regressions").

# Correlação parcial entre convergência e exportações, dada a indicadora de
# Europa: a correlação entre os dois resíduos. Compare com a correlação
# simples, sem controle.
correlacao_parcial <- cor(
  paises_2016$residuo_convergencia,
  paises_2016$residuo_exportacoes
)
correlacao_parcial
cor(paises_2016$convergencia, paises_2016$exportacoes_china_pct)

# Como na Aula 5, inclinação = correlação x dp(Y) / dp(X), agora com os
# resíduos no lugar de Y e X.
correlacao_parcial *
  sd(paises_2016$residuo_convergencia) / sd(paises_2016$residuo_exportacoes)

# Figura 2. Relação com a indicadora de Europa retirada de X1 e de Y, 2016
# (96 países). Cada grupo foi deslocado para ter média zero nos dois eixos;
# a reta é a regressão dos resíduos (coef_longo).
figura_2 <- ggplot(
  paises_2016,
  aes(x = residuo_exportacoes, y = residuo_convergencia, color = grupo)
) +
  geom_hline(yintercept = 0, color = "#94A3B8") +
  geom_vline(xintercept = 0, color = "#94A3B8") +
  geom_point(size = 2.4, alpha = 0.8) +
  geom_smooth(
    aes(group = 1), method = "lm", formula = y ~ x, se = FALSE,
    color = "#C2410C"
  ) +
  scale_color_manual(values = cores_grupo) +
  labs(
    title = "Figura 2. Resíduos da convergência e das exportações para a China, 2016",
    subtitle = paste(
      "Com a indicadora de Europa: inclinação de",
      format(round(coef(modelo_residuos)[["residuo_exportacoes"]], 2), decimal.mark = ","),
      "p.p. por p.p."
    ),
    x = "Exportações para a China: resíduo na indicadora de Europa (p.p.)",
    y = "Convergência: resíduo na indicadora de Europa (p.p.)",
    color = NULL,
    caption = paste(
      "Fontes: votações nominais da AGNU (pacote unvotes);",
      "ITPD-E, release 3 (USITC)."
    )
  ) +
  theme_minimal(base_size = 12)
figura_2

# 5. Modelos progressivos -------------------------------------------------

# M1: exportações para a China.
modelo_m1 <- lm(convergencia ~ exportacoes_china_pct, data = paises_2016)

# M2: M1 + continente.
modelo_m2 <- lm(
  convergencia ~ exportacoes_china_pct + continente,
  data = paises_2016
)

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

# 6. MQO em forma matricial -----------------------------------------------

# matriz_x: uma linha por país e uma coluna por coeficiente de M4 (a coluna
# de uns do intercepto, as exportações, as três indicadoras de continente e
# os demais preditores). model.matrix() monta a matriz a partir do modelo.
matriz_x <- model.matrix(modelo_m4)
dim(matriz_x)
head(matriz_x)

# vetor_y: a convergência dos 96 países, em coluna.
vetor_y <- paises_2016$convergencia

# X'X e X'Y: t() transpõe e %*% multiplica matrizes.
x_linha_x <- t(matriz_x) %*% matriz_x
x_linha_y <- t(matriz_x) %*% vetor_y

# (X'X)^{-1} X'Y: solve() calcula a inversa.
beta_chapeu <- solve(x_linha_x) %*% x_linha_y

# Tabela 4. Coeficientes de M4 pela fórmula matricial e por lm().
tabela_4 <- data.frame(
  coeficiente = colnames(matriz_x),
  formula_matricial = as.numeric(beta_chapeu),
  lm = as.numeric(coef(modelo_m4))
)
tabela_4

# Equações normais: X'e é um vetor de zeros (a menos de erro numérico).
t(matriz_x) %*% residuals(modelo_m4)

# 7. Erro comum com três preditores (simulação) ---------------------------

# Dados simulados: conhecemos a regra que gera Y. X1 e X2 são independentes
# entre si, e os dois têm correlação 0,6 com X3, o controle.
set.seed(6183)
n <- 1000
simulacao <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
simulacao$x3 <- 0.6 * simulacao$x1 + 0.6 * simulacao$x2 + sqrt(0.28) * rnorm(n)
simulacao$y <- 1 * simulacao$x1 + 3 * simulacao$x2 - 2 * simulacao$x3 + rnorm(n)

round(cor(simulacao[, c("x1", "x2", "x3")]), 2)

# Regressão múltipla: coeficientes próximos de 1, 3 e -2.
modelo_completo <- lm(y ~ x1 + x2 + x3, data = simulacao)
coef(modelo_completo)

# Erro comum: residualizar só Y no controle X3 e regredir esse resíduo em X1
# e X2 originais. O coeficiente de X1 troca de sinal.
simulacao$y_res_x3 <- residuals(lm(y ~ x3, data = simulacao))
modelo_errado <- lm(y_res_x3 ~ x1 + x2, data = simulacao)
coef(modelo_errado)

# Forma correta: residualizar também X1 e X2 em X3. Os coeficientes voltam a
# ser os da regressão múltipla.
simulacao$x1_res_x3 <- residuals(lm(x1 ~ x3, data = simulacao))
simulacao$x2_res_x3 <- residuals(lm(x2 ~ x3, data = simulacao))
modelo_certo <- lm(y_res_x3 ~ x1_res_x3 + x2_res_x3, data = simulacao)
coef(modelo_certo)

# Por que o sinal troca (Hull, 2018): coeficientes errados = Omega x
# coeficientes certos, com Omega = (D'D)^{-1} D' D_til. D reúne X1 e X2
# centrados; D_til reúne X1 e X2 residualizados em X3. Os termos fora da
# diagonal de Omega misturam o coeficiente de X2, que é grande, no de X1.
matriz_d <- scale(as.matrix(simulacao[, c("x1", "x2")]), scale = FALSE)
matriz_d_til <- as.matrix(simulacao[, c("x1_res_x3", "x2_res_x3")])
omega <- solve(t(matriz_d) %*% matriz_d) %*% t(matriz_d) %*% matriz_d_til
omega
omega %*% coef(modelo_completo)[c("x1", "x2")]

# Figura 3. Gráfico com o erro comum: Y residualizado em X3 contra X1
# original. A reta desce, embora o coeficiente de X1 seja positivo.
figura_3 <- ggplot(simulacao, aes(x = x1, y = y_res_x3)) +
  geom_point(alpha = 0.3, color = "#334155") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#B91C1C") +
  labs(
    title = "Figura 3. Erro comum: só Y residualizado",
    subtitle = paste0(
      "Y residualizado em X3 contra X1 original: inclinação de ",
      format(round(coef(lm(y_res_x3 ~ x1, data = simulacao))[["x1"]], 2), decimal.mark = ",")
    ),
    x = "X1 original",
    y = "Y residualizado em X3",
    caption = "Dados simulados (n = 1.000); na regra que gera Y, o coeficiente de X1 é 1."
  ) +
  theme_minimal(base_size = 12)
figura_3

# Gráfico correto: residualizar Y e X1 em todos os outros preditores (X2 e
# X3). A inclinação é o coeficiente de X1 na regressão múltipla.
simulacao$x1_res_x2x3 <- residuals(lm(x1 ~ x2 + x3, data = simulacao))
simulacao$y_res_x2x3 <- residuals(lm(y ~ x2 + x3, data = simulacao))

# Figura 4. Forma correta: Y e X1 residualizados em X2 e X3.
figura_4 <- ggplot(simulacao, aes(x = x1_res_x2x3, y = y_res_x2x3)) +
  geom_point(alpha = 0.3, color = "#334155") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#047857") +
  labs(
    title = "Figura 4. Forma correta: Y e X1 residualizados em X2 e X3",
    subtitle = paste0(
      "Inclinação de ",
      format(round(coef(modelo_completo)[["x1"]], 2), decimal.mark = ","),
      ", igual ao coeficiente de X1 na regressão múltipla"
    ),
    x = "X1 residualizado em X2 e X3",
    y = "Y residualizado em X2 e X3",
    caption = "Dados simulados (n = 1.000)."
  ) +
  theme_minimal(base_size = 12)
figura_4

# 8. Perguntas ------------------------------------------------------------

# 1. Na Tabela 1 e em coef(modelo_continente), onde está a média da Europa?
# 2. Na Tabela 2, por que o coeficiente das exportações muda de sinal da
#    regressão curta para a longa? Use os sinais de coef_europa e de
#    inclinacao_auxiliar.
# 3. Compare as Figuras 1 e 2. Onde estão os países europeus em cada uma?
#    Por que a inclinação passa de positiva a negativa? Por que a
#    inclinação da Figura 2 é igual a coef_longo? Compare também
#    correlacao_parcial com a correlação simples. Por que um gráfico com
#    o resíduo de Y e as exportações originais no eixo horizontal daria
#    outra inclinação?
# 4. Escreva um parágrafo que interprete o coeficiente das exportações para
#    a China em M4 (Tabela 3), com as unidades de X e de Y.
# 5. Quais são as dimensões de matriz_x e de x_linha_x? Por que matriz_x
#    tem nove colunas?
# 6. Na simulação (parte 7), por que o coeficiente de X1 troca de sinal em
#    modelo_errado? Use omega. Qual das Figuras 3 e 4 mostra a relação
#    entre X1 e Y com X2 e X3 fixos?
# 7. (Opcional) Estime M3 sem os EUA. O que acontece com o coeficiente do
#    hiato de poder?
#
# Entrega: script, Tabela 3 e o parágrafo da pergunta 4.
