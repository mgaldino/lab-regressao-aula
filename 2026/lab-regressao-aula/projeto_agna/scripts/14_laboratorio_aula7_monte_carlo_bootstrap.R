# Aula 7: Monte Carlo e bootstrap de pares -------------------------------
# FLS 6183 - Métodos Quantitativos de Pesquisa II
# 14 de outubro de 2026
#
# Abra lab-regressao-aula-2026.Rproj no RStudio e execute por blocos,
# em duplas ou trios. Antes de cada comparação, discuta a pergunta indicada.
# Os dados são sintéticos: neste laboratório conhecemos a população.
# A unidade dos dados é um par (X,Y); a unidade dos resultados de Monte Carlo
# é uma amostra inteira, representada pelos coeficientes estimados nela.
#
# Partes (75 minutos, incluindo execução, discussão e registro):
# 1. Uma amostra da população conhecida ......................... 10 min
# 2. Distribuição amostral por Monte Carlo ...................... 20 min
# 3. Tamanho da amostra e ruído ................................. 15 min
# 4. Bootstrap de pares ........................................ 20 min
# 5. Comparação, interpretação e entrega ....................... 10 min
#
# Leituras: Aronow e Miller (2019), seções 3.2.2 e 3.4.3;
# Hansen, Econometrics, seções 4.4--4.8 e 7.2. Mesmos exemplos dos slides.
# Não é necessário executar o script que gera as figuras dos slides.

library(ggplot2)
library(dplyr)
library(here)
here::i_am("projeto_agna/scripts/14_laboratorio_aula7_monte_carlo_bootstrap.R")
options(scipen = 999)

# 1. Uma amostra da população conhecida (10 min) -------------------------

# Regra: X ~ N(10, 3^2), epsilon ~ N(0, 2^2), independentes,
# Y = alpha + beta * X + epsilon, com alpha = 5 e beta = 1,5.
# Aqui a esperança condicional é linear e coincide com o BLP.
alpha_pop <- 5
beta_pop <- 1.5
media_x_pop <- 10
dp_x_pop <- 3
n_base <- 40L
sigma_base <- 2

# Pergunta 1. Antes de gerar os dados: uma amostra de 40 pares deve produzir
# coeficientes exatamente iguais a 5 e 1,5? Justifique sua previsão.

# A função reúne a regra geradora. Cada chamada produz uma NOVA amostra.
# n é o número de unidades na amostra; sigma é o desvio-padrão do erro.
gerar_amostra <- function(n, sigma = 2) {
  x <- rnorm(n, mean = media_x_pop, sd = dp_x_pop)
  epsilon <- rnorm(n, mean = 0, sd = sigma)
  y <- alpha_pop + beta_pop * x + epsilon
  data.frame(id = seq_len(n), x = x, y = y, epsilon = epsilon)
}

# A semente e a regra reproduzem a primeira amostra mostrada nos slides.
set.seed(618307)
dados <- gerar_amostra(n = n_base, sigma = sigma_base)
head(dados)
nrow(dados)
anyNA(dados)

modelo_observado <- lm(y ~ x, data = dados)
coef(modelo_observado)
beta_observado <- coef(modelo_observado)[["x"]]

# Figura 1. Uma amostra de 40 pares: reta de MQO e BLP da população.
# As cores também usam tipos de linha diferentes.
retas <- data.frame(
  reta = c("BLP da população", "MQO da amostra"),
  intercepto = c(alpha_pop, coef(modelo_observado)[["(Intercept)"]]),
  inclinacao = c(beta_pop, beta_observado)
)
figura_1 <- ggplot(dados, aes(x, y)) +
  geom_point(color = "#334155", size = 2.2) +
  geom_abline(data = retas, aes(intercept = intercepto, slope = inclinacao,
                               color = reta, linetype = reta), linewidth = 1) +
  scale_color_manual(name = NULL, values = c("BLP da população" = "#047857",
                                           "MQO da amostra" = "#1D4ED8")) +
  scale_linetype_manual(name = NULL, values = c("BLP da população" = "22",
                                              "MQO da amostra" = "solid")) +
  labs(title = "Figura 1. Uma amostra da população conhecida",
       subtitle = paste(n_base, "pares (X,Y); coeficiente populacional da inclinação =", beta_pop),
       x = "X (unidades)", y = "Y (unidades)",
       caption = "Fonte: dados sintéticos; erro independente de X, com desvio-padrão 2.") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
if (interactive()) print(figura_1)

# Erro populacional gerado (epsilon) e resíduo de MQO (e_hat).
dados$e_hat <- resid(modelo_observado)
mean(dados$epsilon)
mean(dados$e_hat)
beta_observado - beta_pop

# Pergunta 2. Por que a média dos resíduos é praticamente zero, enquanto
# a média dos erros gerados pode ser diferente de zero? A diferença entre
# beta_observado e 1,5, nesta única amostra, revela o viés do estimador?

# 2. Distribuição amostral por Monte Carlo (20 min) -----------------------

# Mantemos n = 40. R_mc conta quantas NOVAS amostras independentes geramos.
# A função guarda o MQO de cada amostra. No for, r identifica a repetição.
simular_mqo <- function(n, sigma, R_mc) {
  alpha_estimado <- numeric(R_mc)
  beta_estimado <- numeric(R_mc)
  for (r in seq_len(R_mc)) {
    amostra_r <- gerar_amostra(n = n, sigma = sigma)
    modelo_r <- lm(y ~ x, data = amostra_r)
    alpha_estimado[r] <- coef(modelo_r)[["(Intercept)"]]
    beta_estimado[r] <- coef(modelo_r)[["x"]]
  }
  data.frame(repeticao = seq_len(R_mc), n = n, sigma = sigma,
             alpha_estimado = alpha_estimado, beta_estimado = beta_estimado)
}

R_mc <- 1000L
set.seed(618312)
mc_base <- simular_mqo(n = n_base, sigma = sigma_base, R_mc = R_mc)
head(mc_base)
nrow(mc_base)

# A média das R_mc estimativas aproxima E[beta_hat_n]. Sua diferença para
# beta_pop é uma APROXIMAÇÃO de Monte Carlo do viés. Como temos um número
# finito de repetições, essa aproximação pode ser diferente de zero mesmo
# quando o estimador é teoricamente não viesado.
media_mc <- mean(mc_base$beta_estimado)
vies_mc <- media_mc - beta_pop
variancia_mc <- mean((mc_base$beta_estimado - media_mc)^2)
eqm_mc <- mean((mc_base$beta_estimado - beta_pop)^2)
ep_mc <- sd(mc_base$beta_estimado)

# Tabela 1. Propriedades aproximadas pela simulação (n = 40; R_mc = 1.000).
# Variância e EQM usam a unidade da inclinação ao quadrado. O erro-padrão
# usa unidades de Y por unidade de X, como a própria inclinação.
# variancia_mc usa divisor R_mc; sd() usa R_mc - 1.
tabela_1 <- data.frame(
  medida = c("Parâmetro beta", "Média das estimativas", "Viés aproximado",
             "Variância aproximada", "EQM aproximado", "Erro-padrão aproximado"),
  valor = c(beta_pop, media_mc, vies_mc, variancia_mc, eqm_mc, ep_mc)
)
tabela_1

# Decomposição do EQM nas estimativas simuladas.
c(eqm_mc = eqm_mc, variancia_mais_vies_quadrado = variancia_mc + vies_mc^2)

# Figura 2. Cada inclinação vem de uma das 1.000 amostras de 40 pares.
figura_2 <- ggplot(mc_base, aes(beta_estimado)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.025,
                 fill = "#1D4ED8", color = "white") +
  geom_vline(xintercept = beta_pop, color = "#047857", linetype = "22",
              linewidth = 1) +
  labs(title = "Figura 2. Distribuição amostral aproximada por Monte Carlo",
       subtitle = paste("n =", n_base, ";", R_mc,
                        "amostras independentes; linha tracejada em beta =", beta_pop),
       x = "Inclinação estimada (unidades de Y por unidade de X)",
       y = "Densidade", caption = "Fonte: simulação da população conhecida.") +
  theme_minimal(base_size = 12)
if (interactive()) print(figura_2)

# Pergunta 3. Qual é a unidade representada por uma linha de mc_base?
# Por que temos 1.000 linhas se cada regressão usa somente 40 observações?
# Explique por que vies_mc diferente de zero não demonstra, por si só,
# que o estimador é viesado.

# R_mc afeta a precisão da aproximação de Monte Carlo. Compare as primeiras
# 50 estimativas com as 1.000; o tamanho de CADA amostra continua sendo 40.
data.frame(
  repeticoes_usadas = c(50L, R_mc),
  media_estimativas = c(mean(mc_base$beta_estimado[1:50]), media_mc),
  desvio_padrao_estimativas = c(sd(mc_base$beta_estimado[1:50]), ep_mc)
)

# A precisão melhora em termos probabilísticos. Uma sequência concreta de
# médias pode oscilar em torno da esperança verdadeira conforme R_mc cresce.

# Pergunta 4. Aumentar R_mc de 50 para 1.000 tem o mesmo significado que
# aumentar n de 40 para 1.000? O que cada alteração permite aprender?

# 3. Tamanho da amostra e ruído (15 min) ----------------------------------

# Pergunta 5. Antes de executar: o que espera acontecer com o centro e a
# dispersão das inclinações ao aumentar n? E ao aumentar sigma, mantendo n?

# Reutilizamos o cenário básico da parte 2 e mudamos um elemento por vez.
set.seed(618313)
mc_n160 <- simular_mqo(n = 160L, sigma = sigma_base, R_mc = R_mc)
mc_n640 <- simular_mqo(n = 640L, sigma = sigma_base, R_mc = R_mc)
mc_ruido <- simular_mqo(n = n_base, sigma = 6, R_mc = R_mc)

comparacao_mc <- bind_rows(
  mutate(mc_base, cenario = "n = 40, sigma = 2"),
  mutate(mc_n160, cenario = "n = 160, sigma = 2"),
  mutate(mc_n640, cenario = "n = 640, sigma = 2"),
  mutate(mc_ruido, cenario = "n = 40, sigma = 6")
)
comparacao_mc$cenario <- factor(comparacao_mc$cenario,
  levels = c("n = 40, sigma = 2", "n = 160, sigma = 2",
             "n = 640, sigma = 2", "n = 40, sigma = 6"))

# Tabela 2. Mesma população e mesmo beta, variando n ou o ruído.
# Erro-padrão: desvio-padrão das 1.000 inclinações por cenário.
tabela_2 <- comparacao_mc |>
  group_by(cenario, n, sigma) |>
  summarise(repeticoes = n(), media_estimativas = mean(beta_estimado),
            vies_aproximado = mean(beta_estimado) - beta_pop,
            erro_padrao_mc = sd(beta_estimado),
            eqm_aproximado = mean((beta_estimado - beta_pop)^2),
            .groups = "drop")
print(tabela_2, width = Inf)

# Figura 3. Eixos e largura de classe iguais permitem comparar dispersões.
figura_3 <- ggplot(comparacao_mc, aes(beta_estimado)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.025,
                 fill = "#1D4ED8", color = "white") +
  geom_vline(xintercept = beta_pop, color = "#047857", linetype = "22",
              linewidth = 1) +
  facet_wrap(~cenario, ncol = 2) +
  labs(title = "Figura 3. Tamanho da amostra e ruído",
       subtitle = paste(R_mc, "amostras por cenário; linha tracejada em beta =", beta_pop),
       x = "Inclinação estimada (unidades de Y por unidade de X)",
       y = "Densidade", caption = "Fonte: dados sintéticos; erros homocedásticos e independentes.") +
  theme_minimal(base_size = 12)
if (interactive()) print(figura_3)

# Pergunta 6. Compare a Tabela 2 com sua previsão. Use centro e dispersão
# para explicar o não viés e a consistência neste exemplo. O experimento
# ilustra as propriedades; a demonstração teórica exige as hipóteses da aula.

# 4. Bootstrap de pares (20 min) -----------------------------------------

# Imagine que conhecemos apenas os 40 pares observados na parte 1.
# O bootstrap usará esses pares. A coluna epsilon não é uma entrada dele.
amostra_observada <- dados |> dplyr::select(id, x, y)
n_observado <- nrow(amostra_observada)

# Pergunta 7. Por que uma reamostra pode conter a mesma unidade várias
# vezes? O que aconteceria se sorteássemos x e y separadamente?

# Um único sorteio de índices seleciona as linhas inteiras.
set.seed(618314)
indices_exemplo <- sample.int(n_observado, size = n_observado, replace = TRUE)
amostra_boot_exemplo <- amostra_observada[indices_exemplo, ]
head(amostra_boot_exemplo)
length(unique(indices_exemplo))
coef(lm(y ~ x, data = amostra_boot_exemplo))

# Agora repetimos a reamostragem. B_boot conta as reamostras da MESMA
# amostra observada. Seu tamanho permanece n_observado = 40.
B_boot <- 1000L
beta_boot <- numeric(B_boot)
indices_boot <- vector("list", B_boot)
set.seed(618309)
for (b in seq_len(B_boot)) {
  indices <- sample.int(n_observado, size = n_observado, replace = TRUE)
  reamostra_b <- amostra_observada[indices, ]
  modelo_b <- lm(y ~ x, data = reamostra_b)
  beta_boot[b] <- coef(modelo_b)[["x"]]
  indices_boot[[b]] <- indices
}

bootstrap <- data.frame(reamostra = seq_len(B_boot), beta_estimado = beta_boot)
ep_boot <- sd(beta_boot)
ep_boot
beta_observado

# Figura 4. Distribuição bootstrap obtida a partir da amostra da parte 1.
# A linha marca a estimativa observada, não um parâmetro populacional novo.
figura_4 <- ggplot(bootstrap, aes(beta_estimado)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.025,
                 fill = "#1D4ED8", color = "white") +
  geom_vline(xintercept = beta_observado, color = "#047857", linetype = "22",
              linewidth = 1) +
  labs(title = "Figura 4. Distribuição bootstrap da inclinação",
       subtitle = paste(B_boot, "reamostras de", n_observado,
                        "pares; linha tracejada na estimativa original"),
       x = "Inclinação estimada (unidades de Y por unidade de X)",
       y = "Densidade", caption = "Fonte: bootstrap de pares da amostra sintética observada.") +
  theme_minimal(base_size = 12)
if (interactive()) print(figura_4)

# Pergunta 8. Descreva o que fica fixo e o que muda entre as reamostras.
# Como o bootstrap estima o erro-padrão sem receber beta_pop como entrada?
# Ele também é uma aproximação: neste exemplo usamos unidades i.i.d.
# e uma amostra aleatória da população descrita na parte 1.

# 5. Comparação, interpretação e entrega (10 min) -------------------------

# Monte Carlo: desvio-padrão de estimativas de novas amostras da população.
# Bootstrap: desvio-padrão de estimativas de reamostras da amostra observada.
# B_boot maior melhora a precisão da aproximação bootstrap, mantendo fixa
# a informação disponível nos 40 pares originais.

# Tabela 3. Dois procedimentos para avaliar a variabilidade da inclinação.
# Ambos em unidades de Y por unidade de X. As duas últimas colunas resumem
# distribuições distintas, e as estimativas de erro-padrão podem diferir.
tabela_3 <- data.frame(
  procedimento = c("Monte Carlo", "Bootstrap de pares"),
  origem = c("Novas amostras da população conhecida", "Reamostras de 40 pares observados"),
  repeticoes = c(R_mc, B_boot),
  media_estimativas = c(media_mc, mean(beta_boot)),
  erro_padrao_aproximado = c(ep_mc, ep_boot)
)
tabela_3

# Pergunta 9. Escreva um parágrafo de 5 a 7 linhas que explique:
# - por que uma única estimativa diferente de 1,5 não revela o viés;
# - o que mudou ao aumentar n, e o que mudou ao aumentar R_mc;
# - qual é a diferença entre a origem das amostras de Monte Carlo e
#   a origem das reamostras bootstrap;
# - o que o número ep_boot descreve sobre a inclinação estimada.
# Use a Tabela 2 e a Tabela 3 para sustentar a interpretação.

# Entrega: este script com suas respostas em comentários, Tabelas 2 e 3,
# Figura 3, Figura 4 e o parágrafo da pergunta 9. A atividade concentra-se
# na variabilidade do estimador e na interpretação do erro-padrão.

# Salvamos os resultados para retomar a atividade e reproduzir as figuras.
# Os arquivos ficam na pasta do laboratório, separados dos resultados dos slides.
saida_lab <- here::here("projeto_agna", "output", "aula_07", "laboratorio")
dir.create(saida_lab, recursive = TRUE, showWarnings = FALSE)
write.csv(tabela_1, file.path(saida_lab, "tabela_01_monte_carlo.csv"), row.names = FALSE)
write.csv(tabela_2, file.path(saida_lab, "tabela_02_cenarios.csv"), row.names = FALSE)
write.csv(tabela_3, file.path(saida_lab, "tabela_03_monte_carlo_bootstrap.csv"), row.names = FALSE)

resultados_lab <- list(
  parametros = list(alpha = alpha_pop, beta = beta_pop, media_x = media_x_pop,
                    dp_x = dp_x_pop, n = n_base, sigma = sigma_base,
                    R_mc = R_mc, B_boot = B_boot),
  dados = dados, beta_observado = beta_observado,
  mc = comparacao_mc, bootstrap = bootstrap, indices_boot = indices_boot,
  tabelas = list(tabela_1 = tabela_1, tabela_2 = tabela_2, tabela_3 = tabela_3)
)
saveRDS(resultados_lab, file.path(saida_lab, "resultados_laboratorio_aula_07.rds"))
saveRDS(list(figura_1 = figura_1, figura_2 = figura_2, figura_3 = figura_3,
             figura_4 = figura_4), file.path(saida_lab, "figuras_laboratorio_aula_07.rds"))

# ggsave() exporta sem abrir uma janela gráfica durante a execução em lote.
ggsave(file.path(saida_lab, "figura_03_cenarios.pdf"), figura_3,
        width = 9, height = 6, units = "in", device = grDevices::pdf, useDingbats = FALSE)
ggsave(file.path(saida_lab, "figura_04_bootstrap.pdf"), figura_4,
        width = 9, height = 4.8, units = "in", device = grDevices::pdf, useDingbats = FALSE)

message("Laboratório concluído. Resultados em: ", saida_lab)
