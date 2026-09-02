# Laboratório da Aula 3: CEF, melhores preditores e BLP.
#
# Pergunta: como a convergência dos votos do Brasil com a China varia
# entre temas e entre os períodos 1997-2008 e 2009-2016?
#
# Hoje vamos aprender a:
# 1. calcular uma CEF empírica com group_by() e summarise();
# 2. comparar CEF e BLP quando X é binária;
# 3. devolver previsões à base com left_join();
# 4. comparar preditores pelo erro quadrático médio (EQM);
# 5. construir um gráfico em camadas com ggplot2.
#
# A análise é descritiva e preditiva. Ela não identifica efeitos causais.

options(scipen = 999)

library(data.table)
library(dplyr)
library(ggplot2)
library(here)

# 1. Leitura e validação --------------------------------------------------

banco <- fread(
  here(
    "projeto_agna", "data", "processed",
    "brasil_convergencia_china_1997_2016.csv"
  )
)

# Cada linha é uma resolução com votos válidos de Brasil e China.
# periodo_2009 é o rótulo; pos_2009 vale 0 antes de 2009 e 1 depois.
dim(banco)
head(banco)

# Antes de continuar, responda:
# O que representa uma linha? Qual é a chave? Quais variáveis são binárias?

ausentes <- banco |>
  dplyr::select(
    rcid, data, ano, periodo_2009, pos_2009, tema, convergente
  ) |>
  dplyr::summarise(
    dplyr::across(dplyr::everything(), ~ sum(is.na(.x)))
  )

validacoes <- data.frame(
  verificacao = c(
    "Resoluções duplicadas",
    "Ano fora de 1997-2016",
    "Ano diferente da data",
    "Período diferente do ano",
    "Convergência fora de 0/1"
  ),
  problemas = c(
    sum(duplicated(banco$rcid)),
    sum(!banco$ano %in% 1997:2016),
    sum(as.integer(format(banco$data, "%Y")) != banco$ano),
    sum(
      (banco$ano >= 2009) !=
        (banco$periodo_2009 == "2009-2016")
    ),
    sum(!banco$convergente %in% c(0, 1))
  )
)

ausentes
validacoes

# 2. Melhor preditor constante -------------------------------------------

# Da Aula 2: sem usar X, a média é o melhor preditor sob perda quadrática.
media_geral <- mean(banco$convergente)
eqm_media_geral <- mean((banco$convergente - media_geral)^2)

media_geral
eqm_media_geral

# 3. CEF empírica por período --------------------------------------------

# Como convergente vale 0 ou 1, sua média é a proporção de convergência.
cef_periodo <- banco |>
  dplyr::group_by(periodo_2009, pos_2009) |>
  dplyr::summarise(
    n_resolucoes = dplyr::n(),
    previsao_cef_periodo = mean(convergente),
    .groups = "drop"
  ) |>
  dplyr::arrange(pos_2009)

cef_periodo

# PARE:
# 1. Interprete a previsão para 1997-2008 e a previsão para 2009-2016.
# 2. Calcule a diferença entre elas em pontos percentuais.

# 4. BLP com indicador binário -------------------------------------------

# O BLP tem a forma: previsão = alpha + beta * pos_2009.
beta_blp <- cov(banco$convergente, banco$pos_2009) /
  var(banco$pos_2009)

alpha_blp <- mean(banco$convergente) -
  beta_blp * mean(banco$pos_2009)

alpha_blp
beta_blp

# PARE: calcule a previsão quando pos_2009 é 0 e quando é 1.

tabela_1_cef_blp <- cef_periodo |>
  dplyr::mutate(
    previsao_blp = alpha_blp + beta_blp * pos_2009,
    diferenca = previsao_cef_periodo - previsao_blp
  ) |>
  dplyr::select(
    periodo_2009, n_resolucoes,
    previsao_cef_periodo, previsao_blp, diferenca
  )

# Tabela 1. CEF e BLP usando a mesma informação: o período.
tabela_1_cef_blp

# Com X binária, CEF e BLP produzem as mesmas duas previsões.

# left_join() devolve a previsão de cada período às resoluções daquele período.
banco_previsoes <- banco |>
  dplyr::left_join(
    cef_periodo |>
      dplyr::select(periodo_2009, previsao_cef_periodo),
    by = "periodo_2009"
  ) |>
  dplyr::mutate(
    previsao_media_geral = media_geral,
    previsao_blp = alpha_blp + beta_blp * pos_2009
  )

# 5. CEF empírica por tema e período -------------------------------------

# Agora usamos mais informação: uma média para cada combinação observada.
cef_tema_periodo <- banco |>
  dplyr::group_by(tema, periodo_2009) |>
  dplyr::summarise(
    n_resolucoes = dplyr::n(),
    previsao_cef_tema_periodo = mean(convergente),
    .groups = "drop"
  )

# Para exibir e interpretar, mantemos temas presentes nos dois períodos
# e com pelo menos 30 resoluções em cada período.
tabela_2_tema_periodo <- cef_tema_periodo |>
  dplyr::group_by(tema) |>
  dplyr::filter(
    dplyr::n() == 2,
    min(n_resolucoes) >= 30
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    tema = dplyr::if_else(
      tema == "Sem codificacao tematica",
      "Sem codificação temática",
      tema
    )
  )

# Tabela 2. CEF empírica por tema e período.
print(
  tabela_2_tema_periodo,
  n = nrow(tabela_2_tema_periodo),
  width = Inf
)

# PARE: por que previsões baseadas em poucas resoluções são menos estáveis?

# Para calcular o EQM, devolvemos todas as previsões à base.
banco_previsoes <- banco_previsoes |>
  dplyr::left_join(
    cef_tema_periodo,
    by = c("tema", "periodo_2009")
  )

# 6. Comparação dos EQMs -------------------------------------------------

tabela_3_eqm <- data.frame(
  preditor = c(
    "Média geral",
    "CEF por período",
    "BLP por período",
    "CEF por tema e período"
  ),
  informacao = c(
    "Nenhuma", "Período", "Período", "Tema e período"
  ),
  eqm = c(
    eqm_media_geral,
    mean(
      (banco_previsoes$convergente -
        banco_previsoes$previsao_cef_periodo)^2
    ),
    mean(
      (banco_previsoes$convergente -
        banco_previsoes$previsao_blp)^2
    ),
    mean(
      (banco_previsoes$convergente -
        banco_previsoes$previsao_cef_tema_periodo)^2
    )
  )
)

# Tabela 3. EQM dos preditores e informação utilizada.
print(tabela_3_eqm)

# PARE:
# Por que CEF e BLP por período têm o mesmo EQM?
# Por que usar tema e período reduz o EQM dentro desta amostra?
# Isso garante melhor previsão de novas resoluções?

# 7. Gráfico em camadas --------------------------------------------------

# Figura 1: gráfico de pontos facetado.
# Camada 1: médias condicionais observadas.
grafico_basico <- ggplot(
  tabela_2_tema_periodo,
  aes(
    x = previsao_cef_tema_periodo,
    y = reorder(tema, previsao_cef_tema_periodo)
  )
) +
  geom_point(size = 2.4)

grafico_basico

# Camada 2: referência da média geral.
grafico_com_referencia <- grafico_basico +
  geom_vline(
    xintercept = media_geral,
    linetype = "dashed"
  )

grafico_com_referencia

# Camada 3: períodos e rótulos.
grafico_com_rotulos <- grafico_com_referencia +
  facet_wrap(~periodo_2009) +
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1)
  ) +
  labs(
    title = paste(
      "Figura 1. CEF empírica da convergência",
      "Brasil-China por tema e período"
    ),
    subtitle = paste(
      "Gráfico de pontos facetado;",
      "a linha tracejada representa a média geral"
    ),
    x = "Convergência média observada",
    y = "Tema da resolução",
    caption = paste(
      "Fonte: base didática AGNA.",
      "Exibidos temas com ao menos 30 resoluções em cada período."
    )
  )

grafico_com_rotulos

# Camada 4: tema visual.
grafico_final <- grafico_com_rotulos +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  )

grafico_final

# Salvamos a figura para o relatório.
diretorio_saida <- here("projeto_agna", "output", "aula_03")
dir.create(diretorio_saida, recursive = TRUE, showWarnings = FALSE)

ggsave(
  file.path(diretorio_saida, "figura_1_cef_tema_periodo.png"),
  plot = grafico_final,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Figura 1.1: gráfico de Cleveland pareado, também chamado dumbbell plot.
# Este formato coloca pré e pós na mesma linha e facilita ver a mudança.

dados_cleveland <- tabela_2_tema_periodo |>
  dplyr::mutate(
    periodo_2009 = factor(
      periodo_2009,
      levels = c("1997-2008", "2009-2016")
    )
  ) |>
  dplyr::arrange(tema, periodo_2009) |>
  dplyr::group_by(tema) |>
  dplyr::mutate(
    mudanca =
      dplyr::last(previsao_cef_tema_periodo) -
      dplyr::first(previsao_cef_tema_periodo)
  ) |>
  dplyr::ungroup()

grafico_cleveland_pareado <- ggplot(
  dados_cleveland,
  aes(
    x = previsao_cef_tema_periodo,
    y = reorder(tema, mudanca),
    group = tema
  )
) +
  geom_line(
    color = "grey70",
    linewidth = 1
  ) +
  geom_point(
    aes(
      color = periodo_2009,
      shape = periodo_2009
    ),
    size = 3.2
  ) +
  scale_color_manual(
    values = c(
      "1997-2008" = "#0072B2",
      "2009-2016" = "#D55E00"
    ),
    labels = c(
      "1997-2008" = "Pré: 1997-2008",
      "2009-2016" = "Pós: 2009-2016"
    )
  ) +
  scale_shape_manual(
    values = c(
      "1997-2008" = 16,
      "2009-2016" = 17
    ),
    labels = c(
      "1997-2008" = "Pré: 1997-2008",
      "2009-2016" = "Pós: 2009-2016"
    )
  ) +
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0.5, 1, by = 0.1)
  ) +
  labs(
    title = paste(
      "Figura 1.1. Mudança na convergência",
      "Brasil-China por tema"
    ),
    subtitle = paste(
      "Gráfico de Cleveland pareado;",
      "temas ordenados do maior aumento para a maior redução"
    ),
    x = "Convergência média observada",
    y = "Tema da resolução",
    color = NULL,
    shape = NULL,
    caption = paste(
      "Fonte: base didática AGNA.",
      "Cada linha conecta a CEF empírica pré e pós-2009.",
      "Exibidos temas com ao menos 30 resoluções em cada período."
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  )

grafico_cleveland_pareado

ggsave(
  file.path(
    diretorio_saida,
    "figura_1_1_cleveland_pareado.png"
  ),
  plot = grafico_cleveland_pareado,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Figura 1.2: gráfico de diferenças com baseline zero.
# Para cada tema, o período pré-2009 é zero. O ponto mostra a mudança
# líquida: convergência pós-2009 menos convergência pré-2009.

dados_mudanca_liquida <- dados_cleveland |>
  dplyr::group_by(tema) |>
  dplyr::summarise(
    mudanca_pp = 100 * dplyr::first(mudanca),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    direcao = dplyr::case_when(
      mudanca_pp > 0 ~ "Aumento",
      mudanca_pp < 0 ~ "Redução",
      TRUE ~ "Sem mudança"
    )
  )

grafico_diferencas_baseline_zero <- ggplot(
  dados_mudanca_liquida,
  aes(y = reorder(tema, mudanca_pp))
) +
  geom_vline(
    xintercept = 0,
    color = "grey40",
    linewidth = 0.8
  ) +
  geom_segment(
    aes(
      x = 0,
      xend = mudanca_pp,
      yend = reorder(tema, mudanca_pp),
      color = direcao
    ),
    linewidth = 1.2
  ) +
  geom_point(
    aes(
      x = mudanca_pp,
      color = direcao
    ),
    size = 3.2
  ) +
  geom_text(
    aes(
      x = mudanca_pp,
      label = sprintf("%+.1f pp", mudanca_pp),
      hjust = ifelse(mudanca_pp >= 0, -0.15, 1.15)
    ),
    color = "grey20",
    size = 3.5
  ) +
  scale_color_manual(
    values = c(
      "Aumento" = "#D55E00",
      "Sem mudança" = "grey50",
      "Redução" = "#0072B2"
    ),
    breaks = c("Aumento", "Sem mudança", "Redução")
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, " pp"),
    breaks = seq(-20, 20, by = 10),
    limits = c(-22, 22)
  ) +
  labs(
    title = paste(
      "Figura 1.2. Mudança líquida na convergência",
      "Brasil-China por tema"
    ),
    subtitle = paste(
      "Gráfico de diferenças com baseline zero;",
      "valores pós-2009 menos valores pré-2009"
    ),
    x = "Mudança líquida (pontos percentuais)",
    y = "Tema da resolução",
    color = NULL,
    caption = paste0(
      "Fonte: base didática AGNA.\n",
      "Nota: o período pré-2009 é normalizado em zero; ",
      "cada ponto mostra pós menos pré.\n",
      "Valores positivos indicam aumento e negativos indicam redução.\n",
      "Temas exibidos têm ao menos 30 resoluções em cada período."
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot"
  )

grafico_diferencas_baseline_zero

ggsave(
  file.path(
    diretorio_saida,
    "figura_1_2_mudanca_liquida_pp.png"
  ),
  plot = grafico_diferencas_baseline_zero,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Figura 2: gráfico de Cleveland pareado com temas individuais.
# Aqui permitimos a dupla contagem: uma resolução com três temas aparece
# uma vez em cada tema. Por isso, as contagens dos temas não devem ser somadas.

banco_temas_individuais <- banco |>
  tidyr::separate_rows(tema, sep = "; ") |>
  dplyr::mutate(
    tema = dplyr::if_else(
      tema == "Sem codificacao tematica",
      "Sem codificação temática",
      tema
    )
  )

cef_tema_individual_periodo <- banco_temas_individuais |>
  dplyr::group_by(tema, periodo_2009) |>
  dplyr::summarise(
    n_resolucoes = dplyr::n(),
    previsao_cef_tema_periodo = mean(convergente),
    .groups = "drop"
  )

dados_temas_individuais <- cef_tema_individual_periodo |>
  dplyr::mutate(
    periodo_2009 = factor(
      periodo_2009,
      levels = c("1997-2008", "2009-2016")
    )
  ) |>
  dplyr::arrange(tema, periodo_2009) |>
  dplyr::group_by(tema) |>
  dplyr::filter(
    dplyr::n() == 2,
    min(n_resolucoes) >= 30
  ) |>
  dplyr::mutate(
    mudanca =
      dplyr::last(previsao_cef_tema_periodo) -
      dplyr::first(previsao_cef_tema_periodo)
  ) |>
  dplyr::ungroup()

grafico_cleveland_temas_individuais <- ggplot(
  dados_temas_individuais,
  aes(
    x = previsao_cef_tema_periodo,
    y = reorder(tema, mudanca),
    group = tema
  )
) +
  geom_line(
    color = "grey70",
    linewidth = 1
  ) +
  geom_point(
    aes(
      color = periodo_2009,
      shape = periodo_2009
    ),
    size = 3.2
  ) +
  scale_color_manual(
    values = c(
      "1997-2008" = "#0072B2",
      "2009-2016" = "#D55E00"
    ),
    labels = c(
      "1997-2008" = "Pré: 1997-2008",
      "2009-2016" = "Pós: 2009-2016"
    )
  ) +
  scale_shape_manual(
    values = c(
      "1997-2008" = 16,
      "2009-2016" = 17
    ),
    labels = c(
      "1997-2008" = "Pré: 1997-2008",
      "2009-2016" = "Pós: 2009-2016"
    )
  ) +
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0.6, 1, by = 0.1)
  ) +
  labs(
    title = paste(
      "Figura 2. Convergência Brasil-China",
      "por tema individual"
    ),
    subtitle = paste(
      "Gráfico de Cleveland pareado; resoluções multitemáticas",
      "entram uma vez em cada tema"
    ),
    x = "Convergência média observada",
    y = "Tema original da resolução",
    color = NULL,
    shape = NULL,
    caption = paste0(
      "Fonte: unvotes 0.3.0 e base didática AGNA.\n",
      "Nota: uma resolução pode pertencer a mais de um tema. ",
      "As contagens dos temas se sobrepõem e não devem ser somadas."
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot"
  )

grafico_cleveland_temas_individuais

ggsave(
  file.path(
    diretorio_saida,
    "figura_2_cleveland_temas_individuais.png"
  ),
  plot = grafico_cleveland_temas_individuais,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Figura 3: gráfico de diferenças com baseline zero e temas individuais.
# Mantemos apenas os cinco temas pedidos para facilitar a comparação.

temas_figura_3 <- c(
  "Human rights",
  "Arms control and disarmament",
  "Economic development",
  "Colonialism",
  "Sem codificação temática"
)

dados_mudanca_temas_individuais <- dados_temas_individuais |>
  dplyr::filter(tema %in% temas_figura_3) |>
  dplyr::group_by(tema) |>
  dplyr::summarise(
    mudanca_pp = 100 * (
      dplyr::last(previsao_cef_tema_periodo) -
        dplyr::first(previsao_cef_tema_periodo)
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    tema = dplyr::case_when(
      tema == "Human rights" ~ "Direitos humanos",
      tema == "Arms control and disarmament" ~
        "Controle de armas e desarmamento",
      tema == "Economic development" ~ "Desenvolvimento econômico",
      tema == "Colonialism" ~ "Colonialismo",
      TRUE ~ tema
    ),
    direcao = dplyr::case_when(
      mudanca_pp > 0 ~ "Aumento",
      mudanca_pp < 0 ~ "Redução",
      TRUE ~ "Sem mudança"
    )
  )

grafico_mudanca_temas_individuais <- ggplot(
  dados_mudanca_temas_individuais,
  aes(y = reorder(tema, mudanca_pp))
) +
  geom_vline(
    xintercept = 0,
    color = "grey40",
    linewidth = 0.8
  ) +
  geom_segment(
    aes(
      x = 0,
      xend = mudanca_pp,
      yend = reorder(tema, mudanca_pp),
      color = direcao
    ),
    linewidth = 1.2
  ) +
  geom_point(
    aes(
      x = mudanca_pp,
      color = direcao
    ),
    size = 3.2
  ) +
  geom_text(
    aes(
      x = mudanca_pp,
      label = sprintf("%+.1f pp", mudanca_pp),
      hjust = ifelse(mudanca_pp >= 0, -0.15, 1.15)
    ),
    color = "grey20",
    size = 3.5
  ) +
  scale_color_manual(
    values = c(
      "Aumento" = "#D55E00",
      "Sem mudança" = "grey50",
      "Redução" = "#0072B2"
    ),
    breaks = c("Aumento", "Sem mudança", "Redução")
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, " pp"),
    breaks = seq(-10, 10, by = 5),
    limits = c(-14, 14)
  ) +
  labs(
    title = paste(
      "Figura 3. Mudança líquida na convergência",
      "por tema individual"
    ),
    subtitle = paste(
      "Gráfico de diferenças com baseline zero;",
      "valores pós-2009 menos valores pré-2009"
    ),
    x = "Mudança líquida (pontos percentuais)",
    y = "Tema original da resolução",
    color = NULL,
    caption = paste0(
      "Fonte: unvotes 0.3.0 e base didática AGNA.\n",
      "Nota: o período pré-2009 é normalizado em zero. ",
      "Uma resolução multitemática entra uma vez em cada tema.\n",
      "As contagens dos temas se sobrepõem e não devem ser somadas."
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot"
  )

grafico_mudanca_temas_individuais

ggsave(
  file.path(
    diretorio_saida,
    "figura_3_mudanca_liquida_temas_individuais.png"
  ),
  plot = grafico_mudanca_temas_individuais,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# 8. Exercício e interpretação -------------------------------------------

# Em dupla ou trio:
# 1. Escolha um tema da Tabela 2.
# 2. Use filter() para mostrar suas duas previsões.
# 3. Calcule a diferença entre os períodos em pontos percentuais.
# 4. Compare as previsões com a média geral.
# 5. Escreva de 100 a 150 palavras explicando o resultado, o número de
#    resoluções, o limite causal e o limite da avaliação dentro da amostra.
# 6. Compare o gráfico de pontos facetado, o gráfico de Cleveland pareado e
#    o gráfico de diferenças com baseline zero. O que cada um mostra melhor?
# 7. Compare as Figuras 1.2 e 3. Como a dupla contagem das resoluções
#    multitemáticas altera a interpretação dos resultados?
#
# tema_escolhido <- "nome do tema"
# resultado_dupla <- tabela_2_tema_periodo |>
#   dplyr::filter(tema == tema_escolhido)
# resultado_dupla
#
# Entrega: tabela filtrada, Figuras 1, 1.1, 1.2, 2 e 3, parágrafo e script R.

# Limites -----------------------------------------------------------------

# - As previsões foram calculadas e avaliadas na mesma amostra.
# - Células pequenas geram médias instáveis.
# - Tema e período fornecem informação preditiva, não identificação causal.

message("PASS: laboratório da Aula 3 executado sem erros.")
