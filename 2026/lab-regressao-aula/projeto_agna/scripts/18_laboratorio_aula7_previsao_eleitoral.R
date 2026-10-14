# Aula 7: previsão eleitoral por zona, com regressão e bootstrap de pares --
# FLS 6183 - Métodos Quantitativos de Pesquisa II, 14/10/2026
#
# Abra lab-regressao-aula-2026.Rproj no RStudio. Execute por blocos em duplas.
# Unidade: UF + zona eleitoral. Uma zona pode abranger vários municípios.
# Escala: porcentagem de TODOS os votos válidos do respectivo turno (0 a 100).
# Treino: Lula versus Jair Bolsonaro, nos dois turnos de 2022.
# Aplicação: primeiro turno de 2026 para prever Lula versus Flávio Bolsonaro.
# Dados reais do TSE. As entradas já estão preparadas, sem download em sala.
# São 2.636 zonas de treino e 2.640 zonas de aplicação nas 27 UFs, sem exterior.
#
# Atividade (75 minutos previstos):
# 1. Dados e porcentagens ...................................... 10 min
# 2. Regressão simples e regressão com UF ....................... 20 min
# 3. Previsões por zona e agregação para 2026 .................... 20 min
# 4. Bootstrap de pares dentro de cada UF ....................... 15 min
# 5. Interpretação e entrega .................................... 10 min
#
# O modelo estima a porcentagem de Lula. No segundo turno com dois candidatos,
# a porcentagem do adversário é 100 menos a porcentagem de Lula.
# Cada zona tem peso 1 no AJUSTE. Para AGREGAR as previsões, usamos o tamanho
# das zonas (votos válidos de 2026 T1) como peso, explicitando a hipótese abaixo.

library(dplyr)
library(ggplot2)
library(here)
here::i_am("projeto_agna/scripts/18_laboratorio_aula7_previsao_eleitoral.R")
options(scipen = 999)

# 1. Dados e porcentagens (10 min) ---------------------------------------

diretorio_dados <- here::here("projeto_agna", "data", "eleicoes_2022_2026", "processed")
dados_2022 <- as_tibble(read.csv(file.path(diretorio_dados, "presidente_2022_zonas.csv"),
                                encoding = "UTF-8"))
dados_2026 <- as_tibble(read.csv(file.path(diretorio_dados, "presidente_2026_t1_zonas.csv"),
                                encoding = "UTF-8"))

# Conferência de um exemplo: primeiro some os votos da zona, depois divida.
# No primeiro turno, os votos dos demais candidatos entram no denominador.
dados_2022 |>
  dplyr::select(uf, zona, lula_t1, bolsonaro_t1, validos_t1, pct_lula_t1,
                lula_t2, bolsonaro_t2, validos_t2, pct_lula_t2) |>
  head()

treino <- dados_2022 |>
  transmute(uf = factor(uf), zona, pct_lula_t1, pct_lula_t2,
            pct_bolsonaro_t1, pct_bolsonaro_t2, validos_t1, validos_t2)
aplicacao <- dados_2026 |>
  transmute(uf = factor(uf, levels = levels(treino$uf)), zona,
            pct_lula_t1, pct_flavio_t1, validos_t1)
stopifnot(!anyNA(treino), !anyNA(aplicacao),
          !anyDuplicated(treino[c("uf", "zona")]),
          !anyDuplicated(aplicacao[c("uf", "zona")]),
          all(treino$validos_t1 > 0), all(treino$validos_t2 > 0),
          all(aplicacao$validos_t1 > 0))
nrow(treino)
nrow(aplicacao)
levels(treino$uf)

# Novas chaves em 2026 também recebem previsão: o modelo usa a porcentagem
# do primeiro turno e a UF. Não exige uma correspondência de zonas entre anos.
# Mesmo código em dois anos não garante território idêntico.
anti_join(aplicacao, treino, by = c("uf", "zona"))

# Pergunta 1. O que representa uma linha? Por que as porcentagens de Lula e
# Bolsonaro somam 100 no segundo turno de 2022, mas não no primeiro?
# Por que não basta somar as porcentagens dos municípios de uma zona?

# 2. Regressão simples e regressão com UF (20 min) ------------------------

modelo_simples <- lm(pct_lula_t2 ~ pct_lula_t1, data = treino)
modelo_uf <- lm(pct_lula_t2 ~ pct_lula_t1 + uf, data = treino)
coef(modelo_simples)
coef(modelo_uf)

# Y_i = alpha + beta_1 X_i + soma_j beta_j D_ij + epsilon_i.
# X_i e Y_i são porcentagens de Lula. AC é a UF de referência.
# beta_1: variação prevista em pontos percentuais (p.p.) no segundo turno
# por 1 p.p. a mais no primeiro, mantendo UF fixa.
# Uma indicadora de UF desloca o intercepto em p.p., mantendo X fixa.

# Tabela 1. Coeficientes do modelo com UF.
tabela_1 <- tibble(termo = names(coef(modelo_uf)),
                   estimativa = unname(coef(modelo_uf)))
print(tabela_1, n = Inf)
treino$ajustado_simples <- fitted(modelo_simples)
treino$ajustado_uf <- fitted(modelo_uf)
treino$e_hat <- resid(modelo_uf)

# Erro de ajuste em 2022. Esta comparação usa os próprios dados do treino.
# Ela não avalia a capacidade de prever uma eleição diferente.
erro_ajuste <- tibble(modelo = c("Somente primeiro turno", "Primeiro turno e UF"),
  raiz_erro_quadratico_medio_pp = c(
    sqrt(mean((treino$pct_lula_t2 - treino$ajustado_simples)^2)),
    sqrt(mean(treino$e_hat^2))))
erro_ajuste

# Figura 1. Porcentagens observadas em 2022. A linha é do modelo simples.
figura_1 <- ggplot(treino, aes(pct_lula_t1, pct_lula_t2)) +
  geom_point(alpha = 0.3, size = 1.1, color = "#334155") +
  geom_abline(intercept = coef(modelo_simples)[["(Intercept)"]],
              slope = coef(modelo_simples)[["pct_lula_t1"]],
              color = "#1D4ED8", linewidth = 0.8) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(title = "Figura 1. Porcentagem de Lula por zona eleitoral, 2022",
       subtitle = "2.636 zonas nas 27 UFs; linha: regressão simples",
       x = "Primeiro turno (% dos votos válidos)",
       y = "Segundo turno (% dos votos válidos)",
       caption = "Fonte: TSE. Cada porcentagem usa todos os votos válidos do respectivo turno.") +
  theme_minimal(base_size = 12)
if (interactive()) print(figura_1)

# Pergunta 2. Interprete a inclinação e o coeficiente de ufSP em p.p.
# Qual seria a diferença prevista entre duas zonas de SP e AC com o mesmo X?
# Pergunta 3. O coeficiente de UF tem interpretação causal? A melhora do
# erro de ajuste em 2022 garante uma previsão melhor para 2026?

# 3. Previsões por zona e agregação para 2026 (20 min) --------------------

# Mantemos o ajuste de 2022 e fornecemos X e UF de 2026 em newdata.
aplicacao$pct_lula_t2_simples <- as.numeric(predict(modelo_simples, newdata = aplicacao))
aplicacao$pct_lula_t2_prevista <- as.numeric(predict(modelo_uf, newdata = aplicacao))
aplicacao$pct_flavio_t2_prevista <- 100 - aplicacao$pct_lula_t2_prevista
aplicacao$fora_limites <- aplicacao$pct_lula_t2_prevista < 0 |
                         aplicacao$pct_lula_t2_prevista > 100

# Diagnóstico: uma regressão linear pode produzir porcentagens fora de 0-100.
# O código conserva os valores originais e registra qualquer caso assim.
sum(aplicacao$fora_limites)
filter(aplicacao, fora_limites)
faixas_2022 <- treino |>
  group_by(uf) |>
  summarise(min_x = min(pct_lula_t1), max_x = max(pct_lula_t1), .groups = "drop")
aplicacao <- left_join(aplicacao, faixas_2022, by = "uf") |>
  mutate(fora_faixa_treino_uf = pct_lula_t1 < min_x | pct_lula_t1 > max_x)
sum(aplicacao$fora_faixa_treino_uf)

# Tabela 2. Exemplos de zonas, uma em cada UF selecionada.
tabela_2 <- aplicacao |>
  filter(uf %in% c("AC", "DF", "PE", "SP")) |>
  group_by(uf) |>
  slice_min(zona, n = 1, with_ties = FALSE) |>
  ungroup() |>
  dplyr::select(uf, zona, pct_lula_t1, pct_flavio_t1,
                pct_lula_t2_prevista, pct_flavio_t2_prevista)
print(tabela_2, width = Inf)

# Agregação: porcentagens de zonas pequenas e grandes não têm o mesmo peso.
# Os votos válidos do SEGUNDO turno de 2026 ainda não estão observados.
# Usamos os do PRIMEIRO turno como pesos. A previsão agregada fica condicionada
# à hipótese de que a distribuição dos votos válidos entre zonas se mantenha.
# Uma mudança uniforme no total de votos válidos não altera esses pesos relativos.
# Exemplo: zonas com 10% e 90%, e pesos 100 e 900, resultam em 82%, não 50%.
weighted.mean(c(10, 90), c(100, 900))

# Tabela 3. Previsões agregadas por UF, com pesos de 2026 T1.
tabela_3 <- aplicacao |>
  group_by(uf) |>
  summarise(zonas = n(), total_validos_t1 = sum(validos_t1),
    pct_lula_t1 = weighted.mean(pct_lula_t1, validos_t1),
    pct_lula_t2_prevista = weighted.mean(pct_lula_t2_prevista, validos_t1),
    previsoes_fora_limites = sum(fora_limites), .groups = "drop") |>
  mutate(pct_flavio_t2_prevista = 100 - pct_lula_t2_prevista)
print(tabela_3, n = Inf, width = Inf)

# Tabela 4. Previsão agregada nas 27 UFs. Votos no exterior não entram.
previsao_agregada <- weighted.mean(aplicacao$pct_lula_t2_prevista, aplicacao$validos_t1)
tabela_4 <- tibble(modelo = c("Somente primeiro turno", "Primeiro turno e UF"),
  pct_lula_t2_prevista = c(
    weighted.mean(aplicacao$pct_lula_t2_simples, aplicacao$validos_t1), previsao_agregada)) |>
  mutate(pct_flavio_t2_prevista = 100 - pct_lula_t2_prevista)
tabela_4

# Pergunta 4. Compare os modelos na Tabela 4 e em uma UF da Tabela 3.
# Qual hipótese permite aplicar a relação entre os turnos de 2022 em 2026?
# Qual hipótese adicional é usada para agregar as porcentagens previstas?
# Examine também as zonas fora da faixa de X observada em sua UF no treino.

# 4. Bootstrap de pares dentro de cada UF (15 min) -----------------------

# Reamostramos linhas de 2022, mantendo (X, Y, UF) juntos. Dentro de cada UF,
# sorteamos com reposição o mesmo número de zonas que ela tinha no treino.
# Os preditores e os pesos de 2026 permanecem fixos em todas as repetições.
# Temos todas as zonas de 2022: este bootstrap é um exercício de sensibilidade
# do ajuste à composição do treino, sob a reamostragem de zonas dentro de UF.
indices_por_uf <- split(seq_len(nrow(treino)), treino$uf)
B_boot <- 500L
beta_boot <- numeric(B_boot)
previsao_boot <- numeric(B_boot)
indices_boot <- matrix(NA_integer_, nrow(treino), B_boot)
set.seed(618316)
for (b in seq_len(B_boot)) {
  indices <- unlist(lapply(indices_por_uf, function(grupo) {
    grupo[sample.int(length(grupo), size = length(grupo), replace = TRUE)]
  }), use.names = FALSE)
  indices_boot[, b] <- indices
  reamostra <- treino[indices, ]
  modelo_b <- lm(pct_lula_t2 ~ pct_lula_t1 + uf, data = reamostra)
  beta_boot[b] <- coef(modelo_b)[["pct_lula_t1"]]
  previsao_boot[b] <- weighted.mean(predict(modelo_b, newdata = aplicacao),
                                  aplicacao$validos_t1)
}
bootstrap <- tibble(reamostra = seq_len(B_boot), inclinacao = beta_boot,
                    pct_lula_t2_prevista = previsao_boot,
                    pct_flavio_t2_prevista = 100 - previsao_boot)

# Tabela 5. Dispersão da inclinação e da previsão agregada sob o bootstrap.
tabela_5 <- tibble(medida = c("Inclinação (p.p. por p.p.)", "Previsão de Lula, 27 UFs (%)"),
  estimativa_original = c(coef(modelo_uf)[["pct_lula_t1"]], previsao_agregada),
  media_bootstrap = c(mean(beta_boot), mean(previsao_boot)),
  desvio_padrao_bootstrap = c(sd(beta_boot), sd(previsao_boot)))
print(tabela_5, width = Inf)

# Figura 2. Distribuição bootstrap das previsões agregadas.
figura_2 <- ggplot(bootstrap, aes(pct_lula_t2_prevista)) +
  geom_histogram(bins = 30, fill = "#1D4ED8", color = "white") +
  geom_vline(xintercept = previsao_agregada, color = "#047857", linetype = "22",
              linewidth = 1) +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",", accuracy = 0.01)) +
  labs(title = "Figura 2. Previsão agregada nas reamostras de 2022",
       subtitle = "500 reamostras de zonas dentro de UF; entradas de 2026 fixas",
       x = "Lula no segundo turno de 2026 (% previsto, 27 UFs)", y = "Reamostras",
       caption = paste("Fonte: TSE e cálculo didático. Linha: previsão do ajuste original.",
                       "Pesos: votos válidos de 2026 T1. A dispersão mede sensibilidade à reamostragem.",
                       sep = "\n")) +
  theme_minimal(base_size = 12)
if (interactive()) print(figura_2)

# Pergunta 5. O que fica fixo e o que muda no bootstrap? Por que X e Y devem
# ser reamostrados juntos? A versão de pares pressupõe unidades independentes,
# aqui condicionando nos números de zonas das UFs. Dependência entre zonas,
# mudanças entre eleições e a variabilidade do resultado futuro não entram
# nessa dispersão. Não a interprete como toda a incerteza da previsão de 2026.

# 5. Interpretação e entrega (10 min) ------------------------------------

# Pergunta 6. Escreva 5 a 7 linhas com o modelo, a unidade e a escala, a
# previsão agregada, a comparação com uma UF e as duas hipóteses de aplicação.
# Distinga o segundo turno observado em 2022, o primeiro turno observado em
# 2026 e o segundo turno previsto. Explique o alcance do bootstrap.
# Entrega: este script com respostas, Tabelas 3 e 4 e o parágrafo.
# O laboratório de Monte Carlo está no script 14, como atividade complementar.

saida <- here::here("projeto_agna", "output", "aula_07", "laboratorio_eleitoral")
dir.create(saida, recursive = TRUE, showWarnings = FALSE)
for (nome in c("tabela_1", "tabela_2", "tabela_3", "tabela_4", "tabela_5", "erro_ajuste")) {
  write.csv(get(nome), file.path(saida, paste0(nome, ".csv")),
            row.names = FALSE, fileEncoding = "UTF-8")
}
write.csv(aplicacao, file.path(saida, "previsoes_2026_zonas.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(bootstrap, file.path(saida, "bootstrap_previsoes.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
saveRDS(list(treino = treino, aplicacao = aplicacao, modelo_simples = modelo_simples,
             modelo_uf = modelo_uf, bootstrap = bootstrap, indices_boot = indices_boot,
             B_boot = B_boot, tabelas = list(tabela_1, tabela_2, tabela_3, tabela_4, tabela_5)),
         file.path(saida, "resultados_laboratorio_eleitoral.rds"))
ggsave(file.path(saida, "figura_01_relacao_2022.pdf"), figura_1,
        width = 9, height = 5, device = grDevices::pdf, useDingbats = FALSE)
ggsave(file.path(saida, "figura_02_bootstrap_previsoes.pdf"), figura_2,
        width = 9, height = 5, device = grDevices::pdf, useDingbats = FALSE)
message("Laboratório eleitoral concluído. Resultados em: ", saida)
