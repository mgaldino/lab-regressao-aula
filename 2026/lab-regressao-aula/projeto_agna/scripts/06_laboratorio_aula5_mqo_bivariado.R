# Laboratório da Aula 5: MQO bivariado e interpretação.
#
# Pergunta desta etapa do projeto aplicado com votações da AGNU:
# qual é a associação linear entre o ano da votação e a convergência média
# anual dos votos de Brasil e China na AGNU?
#
# Antes de começar, abra `lab-regressao-aula-2026.Rproj` no RStudio.
# Em aula, execute um bloco de cada vez. As linhas com o nome de um objeto
# mostram tabelas no console ou gráficos no painel Plots do RStudio.

options(scipen = 999)

library(data.table)
library(dplyr)
library(ggplot2)
library(here)

# 1. Leitura direta do único banco usado na aula -------------------------

# Cada linha de `banco_resolucoes` representa uma resolução em que os votos
# de Brasil e China foram observados.
banco_resolucoes <- fread(
  here(
    "projeto_agna",
    "data",
    "processed",
    "brasil_convergencia_china_1997_2016.csv"
  )
)

# 2. Unidade de análise e validações simples -----------------------------

# `validacao_base` documenta dimensões, unicidade e cobertura temporal.
validacao_base <- banco_resolucoes |>
  dplyr::summarise(
    n_linhas = dplyr::n(),
    n_colunas = ncol(banco_resolucoes),
    n_resolucoes_distintas = dplyr::n_distinct(rcid),
    n_anos = dplyr::n_distinct(ano),
    primeiro_ano = min(ano),
    ultimo_ano = max(ano),
    primeira_data = min(data),
    ultima_data = max(data)
  )


validacao_base

# `ausentes` conta valores não observados nas variáveis usadas no laboratório.
ausentes <- banco_resolucoes |>
  dplyr::select(
    rcid,
    data,
    ano,
    voto_brasil,
    voto_china,
    convergente
  ) |>
  dplyr::summarise(
    dplyr::across(dplyr::everything(), ~ sum(is.na(.x)))
  )

ausentes

# `validacoes_logicas` procura datas e valores incompatíveis com o desenho.
validacoes_logicas <- data.frame(
  verificacao = c(
    "Uma linha por resolução",
    "Ano entre 1997 e 2016",
    "Data entre 1997 e 2016",
    "Ano coincide com a data",
    "Votos assumem yes, no ou abstain",
    "Convergência assume apenas 0 ou 1",
    "Convergência coincide com a igualdade dos votos"
  ),
  problemas = c(
    sum(duplicated(banco_resolucoes$rcid)),
    sum(!banco_resolucoes$ano %in% 1997:2016),
    sum(
      banco_resolucoes$data < as.Date("1997-01-01") |
        banco_resolucoes$data > as.Date("2016-12-31")
    ),
    sum(
      as.integer(format(banco_resolucoes$data, "%Y")) !=
        banco_resolucoes$ano
    ),
    sum(
      !banco_resolucoes$voto_brasil %in% c("yes", "no", "abstain") |
        !banco_resolucoes$voto_china %in% c("yes", "no", "abstain")
    ),
    sum(!banco_resolucoes$convergente %in% c(0, 1)),
    sum(
      banco_resolucoes$convergente !=
        as.integer(
          banco_resolucoes$voto_brasil == banco_resolucoes$voto_china
        )
    )
  )
)

validacoes_logicas

# 3. Agregação por ano e mudança da unidade de análise ------------------

# `dados_anuais` muda explicitamente a unidade de análise: agora cada linha
# representa um ano. O denominador e o numerador ficam preservados ao lado
# da taxa média de convergência.
dados_anuais <- banco_resolucoes |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
    n_resolucoes_validas = dplyr::n(),
    n_votos_convergentes = sum(convergente),
    taxa_media_convergencia = mean(convergente),
    .groups = "drop"
  ) |>
  dplyr::arrange(ano)

dados_anuais

# `anos_desde_1997` recentraliza o preditor: zero passa a representar 1997.
# Com o ano bruto, o intercepto seria a previsão para o ano zero: um valor
# matematicamente válido, mas substantivamente pouco útil neste laboratório.
dados_anuais <- dados_anuais |>
  dplyr::mutate(anos_desde_1997 = ano - 1997)

dados_anuais |>
  dplyr::select(
    ano,
    anos_desde_1997,
    n_resolucoes_validas,
    n_votos_convergentes,
    taxa_media_convergencia
  )

# 4. Gráfico de dispersão mínimo ----------------------------------------

# `grafico_basico` contém os 20 pontos anuais, antes de acrescentar a reta.
# Execute a linha `grafico_basico` para vê-lo no painel Plots.
grafico_basico <- ggplot(
  dados_anuais,
  aes(x = ano, y = taxa_media_convergencia)
) +
  geom_point(aes(color = "Taxa anual observada"), size = 2.2) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Taxa anual observada" = "#1D4ED8",
      "Reta ajustada por MQO" = "#C2410C"
    )
  ) +
  scale_x_continuous(breaks = c(1997, 2000, 2003, 2006, 2009, 2012, 2016)) +
  labs(
    title = "Figura 1 (etapa 1). Convergência média anual",
    subtitle = "Votos de Brasil e China na AGNU, 1997 a 2016",
    x = "Ano da votação",
    y = "Convergência média anual (proporção)",
    caption = paste0(
      "Cada ponto representa um ano. Denominador: resoluções com votos ",
      "válidos de Brasil e China em cada ano.\n",
      "Fonte: base didática de votações da AGNU."
    )
  )

grafico_basico

# 5. Cálculo manual do MQO ----------------------------------------------

# Para Y = taxa média e X = anos desde 1997, a inclinação é
# cov(X, Y) / var(X). O intercepto é média(Y) - inclinação * média(X).
covariancia_xy <- cov(
  dados_anuais$anos_desde_1997,
  dados_anuais$taxa_media_convergencia
)

variancia_x <- var(dados_anuais$anos_desde_1997)

inclinacao_manual <- covariancia_xy / variancia_x

intercepto_manual <- mean(dados_anuais$taxa_media_convergencia) -
  inclinacao_manual * mean(dados_anuais$anos_desde_1997)

calculo_manual <- data.frame(
  covariancia_xy = covariancia_xy,
  variancia_x = variancia_x,
  inclinacao = inclinacao_manual,
  intercepto = intercepto_manual
)

# Resposta: proporção anual de convergência; preditor: anos desde 1997.
calculo_manual

# 6. Estimação com lm() e comparação dos coeficientes -------------------

# `modelo_mqo` estima exatamente a mesma reta calculada acima. Nesta aula,
# extraímos somente os coeficientes, sem a saída inferencial do modelo.
modelo_mqo <- lm(
  taxa_media_convergencia ~ anos_desde_1997,
  data = dados_anuais
)

coeficientes_lm <- coef(modelo_mqo)

# Compare primeiro a saída direta de `lm()` com a tabela abaixo.
coeficientes_lm

coeficientes_comparados <- data.frame(
  termo = c("Intercepto", "Anos desde 1997"),
  calculo_manual = c(intercepto_manual, inclinacao_manual),
  lm = unname(coeficientes_lm),
  diferenca_absoluta = abs(
    c(intercepto_manual, inclinacao_manual) - unname(coeficientes_lm)
  )
)

# Diferenças próximas de zero indicam a mesma reta amostral.
coeficientes_comparados

coeficientes_iguais <- isTRUE(all.equal(
  c(intercepto_manual, inclinacao_manual),
  unname(coeficientes_lm),
  tolerance = 0.000000000001
))

# O resultado esperado é TRUE.
coeficientes_iguais

# 7. Valores ajustados e resíduos como construção da reta ---------------

# `dados_com_ajuste` mostra apenas que ajuste + resíduo reconstrói a taxa.
# Resíduos diagnósticos e suas propriedades amostrais ficam para outra aula.
dados_com_ajuste <- dados_anuais |>
  dplyr::mutate(
    valor_ajustado_formula = intercepto_manual +
      inclinacao_manual * anos_desde_1997,
    valor_ajustado_lm = as.numeric(fitted(modelo_mqo)),
    residuo = taxa_media_convergencia - valor_ajustado_lm
  )

# Cada linha representa um ano. Taxa observada = ajuste + resíduo.
dados_com_ajuste |>
  dplyr::select(
    ano,
    taxa_media_convergencia,
    valor_ajustado_formula,
    valor_ajustado_lm,
    residuo
  )

checagens_ajuste <- data.frame(
  verificacao = c(
    "A fórmula e lm() produzem os mesmos valores ajustados",
    "Taxa observada = valor ajustado + resíduo"
  ),
  passou = c(
    isTRUE(all.equal(
      dados_com_ajuste$valor_ajustado_formula,
      dados_com_ajuste$valor_ajustado_lm,
      tolerance = 0.000000000001
    )),
    isTRUE(all.equal(
      dados_com_ajuste$taxa_media_convergencia,
      dados_com_ajuste$valor_ajustado_lm + dados_com_ajuste$residuo
    ))
  )
)

# Ambos os resultados esperados são TRUE.
checagens_ajuste

# 8. Construção incremental do gráfico final ----------------------------

# `grafico_com_reta` acrescenta a relação linear estimada aos pontos.
# A legenda é construída pelas camadas do próprio ggplot.
grafico_com_reta <- grafico_basico +
  geom_line(
    data = dados_com_ajuste,
    aes(y = valor_ajustado_lm, color = "Reta ajustada por MQO"),
    linewidth = 0.9
  ) +
  labs(
    title = "Figura 1 (etapa 2). Pontos anuais e reta de MQO",
    subtitle = "Convergência média dos votos de Brasil e China na AGNU"
  )

grafico_com_reta

# `grafico_com_rotulos` explicita pergunta, unidades, denominador e fonte.
grafico_com_rotulos <- grafico_com_reta +
  labs(
    title = paste0(
      "Figura 1. Associação entre ano e convergência média anual\n",
      "dos votos de Brasil e China"
    ),
    subtitle = "Pontos anuais e reta amostral de mínimos quadrados ordinários",
    x = "Ano da votação",
    y = "Convergência média anual (proporção)",
    caption = paste0(
      "Unidade: ano (20 observações); denominador: resoluções com votos ",
      "válidos dos dois países em cada ano.\n",
      "Fonte: base didática de votações da AGNU. Associação descritiva; ",
      "sem interpretação causal."
    )
  )

grafico_com_rotulos

# `grafico_final` melhora apenas a legibilidade de escalas e elementos visuais.
grafico_final <- grafico_com_rotulos +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Figura 1. Ano e convergência média dos votos de Brasil e China",
    y = "Convergência média anual (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, size = 8),
    legend.position = "bottom"
  )

grafico_final

# Opcional: depois da aula, descomente para salvar o gráfico em arquivo.
# ggplot2::ggsave(
#   filename = here("projeto_agna", "output", "aula_05", "figura_1_mqo_bivariado.png"),
#   plot = grafico_final, width = 9, height = 5.5, units = "in", dpi = 300
# )

# 9. Interpretação substantiva ------------------------------------------

# Multiplicar a inclinação por 100 converte proporção em pontos percentuais.
inclinacao_pontos_percentuais <- 100 * inclinacao_manual
intercepto_percentual <- 100 * intercepto_manual

# Leia a inclinação em pontos percentuais por ano e o intercepto como a
# convergência prevista para 1997. Interprete os dois valores em voz alta.
medidas_interpretacao <- data.frame(
  medida = c("Inclinação (p.p. por ano)", "Intercepto em 1997 (%)"),
  valor = c(inclinacao_pontos_percentuais, intercepto_percentual)
)

medidas_interpretacao

# A reta resume associação linear nos 20 anos; não identifica efeito causal.

# 10. Gráficos da lousa: correlação, regressão e padronização -----------

# As quatro relações do exercício da lousa, da associação mais forte para a
# mais fraca. As três primeiras vêm do arquivo de pontos ideais da AGNU
# (Voeten, Strezhnev e Bailey, versão de junho de 2024). Nele, cada linha é
# um país em uma sessão, e o ano de referência é sessão + 1945. As colunas
# `USAgree`, `ChinaAgree`, `RUSSAgree` e `BrazilAgree` medem a concordância
# de votos de cada país com EUA, China, Rússia e Brasil. Cada relação exclui
# os dois países que definem os eixos.
pontos_ideais <- fread(
  here(
    "projeto_agna",
    "data",
    "raw",
    "ideal_points",
    "IdealpointestimatesAll_Jun2024.csv"
  )
)

eua_china_2019 <- pontos_ideais |>
  dplyr::filter(
    session == 74,
    !iso3c %in% c("USA", "CHN"),
    !is.na(USAgree),
    !is.na(ChinaAgree)
  ) |>
  dplyr::transmute(
    relacao = "1. EUA (X) e China (Y), 2019",
    pais = iso3c,
    x = 100 * USAgree,
    y = 100 * ChinaAgree
  )

eua_russia_2018 <- pontos_ideais |>
  dplyr::filter(
    session == 73,
    !iso3c %in% c("USA", "RUS"),
    !is.na(USAgree),
    !is.na(RUSSAgree)
  ) |>
  dplyr::transmute(
    relacao = "2. EUA (X) e Rússia (Y), 2018",
    pais = iso3c,
    x = 100 * USAgree,
    y = 100 * RUSSAgree
  )

russia_brasil_2019 <- pontos_ideais |>
  dplyr::filter(
    session == 74,
    !iso3c %in% c("RUS", "BRA"),
    !is.na(RUSSAgree),
    !is.na(BrazilAgree)
  ) |>
  dplyr::transmute(
    relacao = "3. Rússia (X) e Brasil (Y), 2019",
    pais = iso3c,
    x = 100 * RUSSAgree,
    y = 100 * BrazilAgree
  )

# Na relação 4, cada ponto é um ano da base desta aula.
brasil_china_anual <- dados_anuais |>
  dplyr::transmute(
    relacao = "4. Ano (X) e convergência Brasil-China (Y)",
    pais = NA_character_,
    x = ano,
    y = 100 * taxa_media_convergencia
  )

relacoes <- dplyr::bind_rows(
  eua_china_2019,
  eua_russia_2018,
  russia_brasil_2019,
  brasil_china_anual
)

# Esperado: 191 países nas relações 1 a 3 e 20 anos na relação 4.
relacoes |>
  dplyr::count(relacao)

# `grafico_relacoes` refaz os gráficos da lousa com a reta de MQO.
# O Brasil aparece em verde nas relações 1 e 2.
grafico_relacoes <- ggplot(relacoes, aes(x = x, y = y)) +
  geom_point(color = "#0F172A", alpha = 0.6, size = 1.8) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = "#C2410C",
    linewidth = 0.9
  ) +
  geom_point(
    data = dplyr::filter(relacoes, pais == "BRA"),
    color = "#15803D",
    size = 3
  ) +
  facet_wrap(~relacao, scales = "free") +
  labs(
    title = "Figura 2. As quatro relações do exercício da lousa",
    x = "X: concordância (%) nas relações 1 a 3; ano na relação 4",
    y = "Y: concordância ou convergência (%)",
    caption = paste0(
      "Relações 1 a 3: cada ponto é um país (191). ",
      "Relação 4: cada ponto é um ano (20). Brasil em verde.\n",
      "Fontes: Voeten, Strezhnev e Bailey, pontos ideais da AGNU ",
      "(jun. 2024); base didática de votações da AGNU."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

grafico_relacoes

# Passo 1. Correlação entre X e Y em cada relação.
correlacoes <- relacoes |>
  dplyr::group_by(relacao) |>
  dplyr::summarise(correlacao = cor(x, y), .groups = "drop")

correlacoes

# Passo 2. Regressão com lm(), começando pela relação 1. A inclinação está em
# pontos percentuais de concordância com a China por ponto percentual de
# concordância com os EUA.
modelo_eua_china <- lm(y ~ x, data = eua_china_2019)

coef(modelo_eua_china)

# Passo 3. Padronize X e Y: subtraia a média e divida pelo desvio-padrão.
# As variáveis padronizadas têm média 0 e desvio-padrão 1.
eua_china_padronizada <- eua_china_2019 |>
  dplyr::mutate(
    x_padronizado = (x - mean(x)) / sd(x),
    y_padronizado = (y - mean(y)) / sd(y)
  )

modelo_eua_china_padronizado <- lm(
  y_padronizado ~ x_padronizado,
  data = eua_china_padronizada
)

# Compare a inclinação padronizada com a correlação da relação 1.
coef(modelo_eua_china_padronizado)

cor(eua_china_2019$x, eua_china_2019$y)

# Por quê? A inclinação de MQO é cov(X, Y) / var(X). Com X e Y padronizados,
# var(X) = 1 e cov(X, Y) passa a ser exatamente a correlação. O intercepto
# vira zero, a menos de erro numérico, porque a reta passa pelo ponto das
# médias, agora (0, 0).

# `comparacao_relacoes` repete os três passos nas quatro relações. A coluna
# `correlacao_vezes_razao_dp` mostra a ponte entre inclinação e correlação:
# inclinação = correlação * dp(Y) / dp(X).
dados_padronizados <- relacoes |>
  dplyr::group_by(relacao) |>
  dplyr::mutate(
    x_padronizado = (x - mean(x)) / sd(x),
    y_padronizado = (y - mean(y)) / sd(y)
  ) |>
  dplyr::ungroup()

comparacao_relacoes <- dados_padronizados |>
  dplyr::group_by(relacao) |>
  dplyr::summarise(
    n = dplyr::n(),
    correlacao = cor(x, y),
    inclinacao = unname(coef(lm(y ~ x))[2]),
    correlacao_vezes_razao_dp = cor(x, y) * sd(y) / sd(x),
    inclinacao_padronizada = unname(
      coef(lm(y_padronizado ~ x_padronizado))[2]
    ),
    .groups = "drop"
  )

comparacao_relacoes

# O resultado esperado é TRUE nas duas linhas.
data.frame(
  verificacao = c(
    "Inclinação = correlação * dp(Y) / dp(X)",
    "Inclinação padronizada = correlação"
  ),
  passou = c(
    isTRUE(all.equal(
      comparacao_relacoes$inclinacao,
      comparacao_relacoes$correlacao_vezes_razao_dp
    )),
    isTRUE(all.equal(
      comparacao_relacoes$inclinacao_padronizada,
      comparacao_relacoes$correlacao
    ))
  )
)

# `grafico_padronizado` põe as quatro relações na mesma escala. A inclinação
# de cada reta laranja é a correlação. A linha tracejada mostra a reta de uma
# correlação perfeita com o mesmo sinal: inclinação +1 ou -1.
correlacoes_perfeitas <- comparacao_relacoes |>
  dplyr::transmute(relacao, inclinacao_perfeita = sign(correlacao))

grafico_padronizado <- ggplot(
  dados_padronizados,
  aes(x = x_padronizado, y = y_padronizado)
) +
  geom_abline(
    data = correlacoes_perfeitas,
    aes(slope = inclinacao_perfeita, intercept = 0),
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_point(color = "#0F172A", alpha = 0.6, size = 1.8) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = "#C2410C",
    linewidth = 0.9
  ) +
  facet_wrap(~relacao) +
  coord_equal() +
  labs(
    title = "Figura 3. As quatro relações com X e Y padronizados",
    x = "X padronizado (desvios-padrão)",
    y = "Y padronizado (desvios-padrão)",
    caption = paste0(
      "Reta laranja: MQO, com inclinação igual à correlação. ",
      "Linha tracejada: correlação perfeita com o mesmo sinal."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

grafico_padronizado

# Perguntas para discutir:
# 1. Ordene as relações pela correlação. A ordem coincide com a dificuldade
#    que a turma teve para desenhar a reta na lousa?
# 2. Na relação 1, a inclinação é menor que -1, mas a correlação não pode
#    ser. Por quê?
# 3. Por que a inclinação padronizada não depende das unidades de X e Y?

# 11. Exercício ---------------------------------------------------------

# Recentrar o ano em 2009:
# 1. Crie `anos_desde_2009 = ano - 2009`.
# 2. Estime a mesma regressão usando o novo preditor.
# 3. Compare a nova inclinação com `inclinacao_manual`.
# 4. Explique por que a inclinação permanece igual e o intercepto muda.
# 5. Interprete o novo intercepto: qual previsão ele representa?
# 6. Explique por que nenhuma parametrização identifica um efeito de 2009.
#
# Pista: recentralizar muda a origem do eixo horizontal, não os pontos nem
# a reta ajustada.

# 12. Limites -----------------------------------------------------------

# - A unidade da regressão é o ano, com apenas 20 observações agregadas.
# - Anos têm números diferentes de resoluções; aqui cada média anual recebe
#   o mesmo peso na reta.
# - A inclinação resume somente uma associação linear na janela observada.
# - O modelo não justifica extrapolar para anos fora de 1997-2016.
# - Não há atribuição aleatória nem estratégia de identificação causal.
# - Portanto, a reta não identifica efeito do tempo, de 2009, do comércio
#   ou da China sobre a convergência dos votos.
